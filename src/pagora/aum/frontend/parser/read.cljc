;;This is a frontend namespace, but is defined as a cljc file so you can run it
;;in clojure and test it.
(ns pagora.aum.frontend.parser.read
  "This namespace defined read* which can be passed to an om parser. It
  will try to resolve a key given to it using denorm/db->tree. Use
  make make-read-fn to actually get the read-fn. If you pass in
  db->tree hooks you can define your own returns for the keys in the
  whole of a query, not just the root keys.
  "
  (:refer-clojure :exclude [read])
  (:require [pagora.aum.frontend.parser.denormalize :as denorm]
            [pagora.aum.frontend.parser.denormalize-hooks :refer [db->tree-hooks]]

            [pagora.aum.dev.debug :as d :refer [mark-point now-in-ms warn-when]]
            [pagora.aum.om.util :as om-util]
            #?@(:clj  [ ;; [transcriptor :as check]
                       [taoensso.tufte :as tufte :refer (defnp p profiled profile)]]
                :cljs [[taoensso.tufte :as tufte :refer-macros (defnp p profiled profile)]])
            [taoensso.timbre :as timbre]))

(def om-query-keys (atom (make-hierarchy)))

(defn derive-om-query-key! [om-query-key1 om-query-key2]
  (swap! om-query-keys derive om-query-key1 om-query-key2))

;; (ns-unmap *ns* 'read)
(defmulti read
  "Dispatch fn is not used. A get-method needs to be done to find the proper read
  method. Used in denormalize-hooks as the read method for keys in a query"
  nil :hierarchy om-query-keys)

(def read-fns (atom nil))

;; (defn make-read-fns-map []
;;   (reset! read-fns (methods read))
;;   read-fns)

(defn _find-read-fn
  "Finds a method fn that returns the ast for the target if it exists.
  Either a a read method or a read-for-target method"
 [target key]
  (let [s (now-in-ms)
        ;; read-fns (or @read-fns (deref (make-read-fns-map)))
        read-for-target-method (when-let [method-for-target
                                          (p :get-method1
                                             ;; This messes with hierarchical methods
                                             ;; (get read-fns [target key])
                                             (get-method read [target key]))]
                                 method-for-target)
        ret (or read-for-target-method
                (when-let [read-method (p :get-method2
                                          ;; This messes with hierarchical methods
                                          ;; (get read-fns key)
                                          (get-method read key)
                                          )]
                  (fn [env key params] (get (read-method env key params) target))))
        e (now-in-ms)]
    (d/add-to-accumulator (- e s))
    ret
    ))

(defn memoize-with
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  {:added "1.0"
   :static true}
  [f mem]
  (fn [& args]
    (if-let [e (find @mem args)]
      (val e)
      (let [ret (apply f args)]
        (swap! mem assoc args ret)
        ret))))

(def mem (atom {}))
(def find-read-fn ;; _find-read-fn
  (memoize-with _find-read-fn mem))

(defn aum-read
  "Resolves key against app-data using db->tree for all targets"
  [{:keys [target query state ast default-remote query-root dispatch-key db->tree] :as  env} key params]
  ;; (timbre/info :#p "++++++++++++++++++++++++++++++++++++++++")
  ;; (timbre/info :#cp {:key key :query query :query-root query-root :ast ast})
  (let [s (now-in-ms)
        target (or target :value)
        _ (when (or true (= key :route/companies)) (timbre/debug :#p "read*" target key))
        app-data @state
        env (assoc env :target target)
        env (dissoc env :parser)
        query-root? (not=  query-root :om.next/root) ;;(an ident of a cmp)
        key (if query-root? query-root key)
        expr (condp = (:type ast)
               :prop key
               :join {key query})
        query [(if (some? params) (list expr params) expr)]
        ret (condp = target
              :value {:value (let [
                                   ;; query (if query-root?
                                   ;;         (if (some? query)
                                   ;;           {query-root query}
                                   ;;           [query-root])
                                   ;;         query)
                                   ;; _ (timbre/info :#r query)
                                   result (do
                                            (let [_ (mark-point :read*-before-db->tree )
                                                  ret (db->tree env {:query query
                                                                     :data  app-data
                                                                     :refs  app-data})]
                                              (mark-point :read*-after-db->tree )
                                              ret))]
                               ;; (timbre/info :#r "Result of default read")
                               ;; (timbre/info :#cp result)
                               ;; (timbre/info :#cp (get result key))

                               (get result key))}
              ;;some remote target
              (do
                (let [sparse-query (do
                                     (mark-point :read*-before-db->tree {:target target})
                                     (let [ret (db->tree env {:query query
                                                              :data app-data
                                                              :refs app-data
                                                              :sparsify-query? true})
                                           ]
                                       (mark-point :read*-after-db->tree)
                                       ret)
                                     )
                      sparse-ast (when (seq sparse-query)
                                   (let [{:keys [type]} ast]
                                     (case type
                                       :prop ast
                                       :join (assoc ast :query (om-util/join-value (first sparse-query))))))]

                  (timbre/debug :#p "Result for read*" key "for" target)
                  (timbre/debug "Sparse query " key)
                  (timbre/debug :#pp (:query sparse-ast))

                  {target sparse-ast})))]

    (warn-when s 0 (str target ": read* of query for key " key))
    ret))

(defn db->tree [{:keys [db->tree-hooks]:as env}
                {:keys [query data refs ignore-hooks?] :as args}]
  (let [args (cond-> args
               (not ignore-hooks?) (merge db->tree-hooks))]
    (denorm/db->tree env args)))

(defn make-read-fn
  "This adds default-remote key to env to indicate name of default remote.
  Mutations and reads can then then decide which remote keys to set
  using that debug. This to avoid having to add name of default remote
  throughout code. Pass in db->tree hooks to return your non-standard
  values and queries for keys in a query"
  ([]
   (make-read-fn {:default-remote :remote}))
  ([extra-env]
   (fn [env key params]
     (profile {} (aum-read (merge env extra-env
                                  {:db->tree db->tree
                                   :db->tree-hooks (or (:db->tree-hooks extra-env) db->tree-hooks)
                                   :find-read-fn find-read-fn})
                           key params)))))


;; #?(:clj
;;    (check/run))

;; (pprint ((methods read)))





;;obsolete
;; ========================================
;; Read remote

;; (defn resolve-join-to-remote
;;   [{:keys [state query parser ast context-data target] :as env} key]
;;   (let [app-data @state
;;         value    (get (or context-data app-data) key)]
;;     ;; return denormalized value
;;     (let [result (cond
;;                    ;; We don't have a value for this key. Remote is set to ast
;;                    ;; for recursiv-query, otherwise we're going to go down the
;;                    ;; rabbit warren and see what else we don't have in the
;;                    ;; subquery. Value is set to what gets returned in a recursive
;;                    ;; parse of the subquery if non recursive.
;;                    (contains? #{ nil :fetch} value) (let [ast-query  (:query ast)
;;                                                           recursive? (or (number? ast-query) (= ast-query '...))]
;;                                                       (cond
;;                                                         (= query ['*]) ast
;;                                                         recursive?     ast
;;                                                         ;;We recursively parse rest of
;;                                                         ;;subquery so any custom read methods
;;                                                         ;;will be called
;;                                                         :else          (let [remote-only-query
;;                                                                              ;;There was no value at
;;                                                                              ;;this key so no context
;;                                                                              ;;data for parser
;;                                                                              (parser (assoc env :context-data {} :subquery true) query target)]

;;                                                                          (and (seq remote-only-query)
;;                                                                               (assoc ast :query remote-only-query)))))

;;                    ;; The key is a (sub)root, since it's value is a map. It's value is a
;;                    ;; single item. We therefore can continue parsing (and keep deciding what
;;                    ;; for what bits to keep in the remote query) We now set the value of the
;;                    ;; key as context-data and the the query of the join as query.
;;                    (map? value) (let [remote-only-query (parser (assoc env :context-data value :subquery true) query target)]
;;                                   (and (seq remote-only-query)
;;                                        (assoc ast :query remote-only-query)))

;;                    ;; Ah, we have something, a list of things.We assume all data is here.
;;                    ;;In calc_ast we have actually a more sophisticated resolver for this case
;;                    (vector? value) nil

;;                    ;; Else just return whatever we've found
;;                    :else nil)]
;;       result)))

;; (defn default-read-remote
;;   "Resolve a query (key or join) against app-state "
;;   [{:keys [state query parser ast context-data target] :as env} key params]
;;   ;; (timbre/debug "default-read-remote" key query)
;;   (let [app-data           @state
;;         context-data       (or context-data app-data)
;;         {:keys [type key]} ast
;;         result (condp = type
;;                  :prop (cond
;;                          ;;Client keys and idents do not get sent to backend
;;                          (bu/client-key? key) nil
;;                          (om-util/ident? key)  nil
;;                          ;; Using context-data to allow for new query roots
;;                          :else (let [value (get context-data key)]
;;                                  (contains? #{nil :fetch} value)))
;;                  ;; Map (possibly wrapped in list to include params)
;;                  :join (cond
;;                          ;;Don't send client keys to backend
;;                          (bu/client-key? key) nil
;;                          ;; Allows idents as key in a join to be sent to server. Server
;;                          ;; processes this correctly, however om-next is designed to only
;;                          ;; use idents client side, I got some weird errors trying to use
;;                          ;; this.
;;                          (om-util/ident? key) nil ;;nil for now, don't send to remote
;;                          ;; (let [value (resolve-ident env {:ident    key
;;                          ;;                                 :query    query
;;                          ;;                                 :app-data @state})]
;;                          ;;   (or (:force? params)
;;                          ;;       (let [query-keys (mapv pu/extract-key query)
;;                          ;;             value-keys (if (map? value) (keys value) [])]
;;                          ;;         (not= (sort query-keys) (sort value-keys)))))


;;                          ;; Key is really a key now. We're gonna get the value for it in
;;                          ;; app-data, or in context-data if we have any.
;;                          :else (resolve-join-to-remote env key)))]
;;     result))

;; ========================================
;; Read value

;; (defn default-read-value
;;   "Resolve a query (key or join) against app-state, or context-data
;;   when it is a subquery. Context data is a sub tree of the app state
;;   as set in env  when parser is called again from within this fn."
;;   [{:keys [state query ast context-data parser] :as env} params]
;;   (let [app-data           @state
;;         context-data       (or context-data app-data)
;;         {:keys [type key]} ast]
;;     ;; (timbre/debug "default-read-value" type key query)
;;     (get (db->tree env {:query (condp = type
;;                                  :prop [key]
;;                                  :join [{key query}])
;;                         :data context-data
;;                         :refs  app-data
;;                         }) key)))


;; ========================================
;; Obsolete

;; (defn resolve-ident [env {:keys [ident query app-data]}]
;;   (timbre/debug "resolving ident")
;;   (cond
;;     (nil? query) (if (= (second ident) '_)
;;                    (get app-data (first ident))
;;                    (get-in app-data ident))
;;     :else (:foo (db->tree env {:query [{:foo query}]
;;                                :data {:foo ident}
;;                                :refs app-data}))))

;; (defn resolve-client-key
;;   "Looks for key in app-data root, if value is an ident, or vector
;;   of idents, resolves ident(s), otherwise just returns value. So a
;;   client key can not be a vector, because it is assumed to be either
;;   ident or vector of idents"
;;   [env key app-data]
;;   (let [value (get app-data key)]
;;     (cond
;;       (om-util/ident? value) (resolve-ident env {:ident value :app-data app-data})
;;       (vector? value)        (db->tree env
;;                                        {:query [{key ['*]}]
;;                                         :data app-data
;;                                         :refs app-data})
;;       :else                  value)))

;; (defn resolve-join-to-value [{:keys [state query parser ast context-data target default-remote] :as env} key]
;;   (let [app-data @state
;;         value (get (or context-data app-data) key)]
;;     (timbre/debug "resolve-join-to-value" value)
;;     ;;return denormalized value
;;     (cond
;;       ;; We don't have a value for this key. If query is
;;       ;;  recursive or '* we return the value found otherwise
;;       ;;  we're going to go down the rabbit warren and see
;;       ;;  what else we don't have in the subquery. Value is
;;       ;;  set to what gets returned in a parse of the subquery
;;       ;;  in this case (we might have idents and/or client
;;       ;;  keys in the subqueries that are resolved against
;;       ;;  app-data, not context-date which is empty at the
;;       ;;  moment)
;;       (contains? #{ nil :fetch} value)
;;       (let [ast-query  (:query ast)
;;             recursive? (or (number? ast-query) (= ast-query '...))]
;;         (cond
;;           (or (= query ['*]) recursive?) value
;;           :else                          (pu/replace-idents
;;                                           (parser (assoc env :context-data {}) query nil))))

;;       ;; Ah, we have something, a list of things. Let's denormalize
;;       ;; it and return it.
;;       (vector? value) (get (db->tree env
;;                                      {:query [(om/ast->query ast)]
;;                                       :data {key value}
;;                                       :refs app-data})
;;                            key)

;;       ;; The key is a (sub)root, since it's value is a map. Let's
;;       ;; call parser again, but now with the value of the key as
;;       ;; context-data and the the query of the join as query.
;;       (map? value) (pu/replace-idents
;;                     (parser (assoc env :context-data value) query))

;;       ;; Else just return whatever we've found
;;       :else value)) )

;; Old result for props:
;; {:foo {:value :for-foo, :bar :bax},
;;  :path1 {:foo {:value :for-foo, :bar :bax}},
;;  :path3
;;  {[:translation/by-id 1] {:id 1, :key "some-translation-key 1"},
;;   [:translation/by-id 2] {:id 2}},
;;  :path5 {:subpath5 {:value :for-foo, :bar :bax}},
;;  :path6 {:subpath6 {:id 1, :key "some-translation-key 1"}},
;;  :path7 #:client{:key1 :value-for-client-key1,
;;                  :key2 {:value :for-foo, :bar :bax},
;;                  :key3
;;                  #:client{:key3
;;                           [{:id 1}
;;                            {:id 2}
;;                            {:id 1, :key "some-translation-key 1"}]}}}

;; Old result for joins
;; {:path1 #:client{:key4 {:id 1}},
;;  :path2 {[:translation/by-id 1] {:id 1}},
;;  :path3 {:foo {:value :for-foo}},
;;  :some1 {:path {:into {}}},
;;  :some2 {:path {:into #:client{:key1 :value-for-client-key1}}},
;;  :some3 {:path {:into {:foo {:value :for-foo, :bar :bax}}}},
;;  :some4
;;  {:path
;;   {:into
;;    {[:translation/by-id 1] {:id 1, :key "some-translation-key 1"}}}},
;;  :some-translations
;;  {:translation
;;   [{:id 1, :key "some-translation-key 1"}
;;    {:id 2, :key "some-translation-key 2"}]}}


;; (defn default-read-value
;;   "Resolve a query (key or join) against app-state, or context-data
;;   when it is a subquery. Context data is a sub tree of the app state
;;   as set in env  when parser is called again from within this fn."
;;   [{:keys [state query ast context-data parser] :as env} params]
;;   (let [app-data           @state
;;         context-data       (or context-data app-data)
;;         {:keys [type key]} ast]
;;     (timbre/debug "default-read-value" type key query)
;;     ;; (timbre/debug :#r "TYPE" type)


;;     ;; (timbre/debug :#pp (db->tree env {:query (condp = type
;;     ;;                                           :prop [key]
;;     ;;                                           :join  [{key query}])
;;     ;;                                  :data app-data
;;     ;;                                  :refs app-data
;;     ;;                                  }))
;;     ;; (get (db->tree env {:query (condp = type
;;     ;;                              :prop [key]
;;     ;;                              :join [{key query}])
;;     ;;                     :data  app-data
;;     ;;                     :refs  app-data
;;     ;;                     }) key)

;;     ;; (timbre/debug :#pp (db->tree env {:query query}))
;;     (condp = type
;;       ;; Keywords and idents
;;       :prop (cond
;;               ;; Any props namespaced with client will get resolved and will not
;;               ;; get send to remote
;;               (bu/client-key? key) (let [value (get app-data key)]
;;                                      (cond
;;                                        (om-util/ident? value) (if (= (second value) '_)
;;                                                                 (get app-data (first value))
;;                                                                 (get-in app-data value))
;;                                        ;; (resolve-ident env {:ident value :app-data app-data})
;;                                        (vector? value)        (do
;;                                                                 (db->tree env
;;                                                                           {:query [{key ['*]}]
;;                                                                            :data  app-data
;;                                                                            :refs  app-data}))
;;                                        :else                  value))
;;               ;; (resolve-client-key env key app-data)

;;               ;; Any prop that's an ident gets resolved recursively and will not
;;               ;; be sent to remote
;;               (om-util/ident? key) (if (= (second key) '_)
;;                                      (get app-data (first key))
;;                                      (get-in app-data key))
;;               ;; (resolve-ident env {:ident key
;;               ;;                     :app-data app-data})

;;               ;; Else return what we have in app state, well in context data, which
;;               ;; is a subtree of app-state by now if this prop is not a root prop. If the
;;               ;; value is an ident resolve it.
;;               :else (let [value (get context-data key)]
;;                       (if (om-util/ident? value)
;;                         (if (= (second value) '_)
;;                           (get app-data (first value))
;;                           (get-in app-data value))
;;                         ;; (resolve-ident env {:ident    value
;;                         ;;                     :app-data app-data})
;;                         value)))

;;       ;; Map (possibly wrapped in list to include params)
;;       :join (cond
;;               ;; Client keys are always looked up in the root of the app-data
;;               ;; map. This is a join, so we use the query of the join to resolve the key.
;;               (bu/client-key? key) (get (db->tree env
;;                                                   {:query [{key query}]
;;                                                    :data app-data
;;                                                    :refs app-data})
;;                                         key)

;;               ;;An ident can have a query:
;;               (om-util/ident? key) (if (= (second key) '_)
;;                                      (if (nil? query)
;;                                        (get app-data (first key))
;;                                        (get (db->tree env {:query [{key query}]
;;                                                            :data app-data
;;                                                            :refs app-data}) (first key)))
;;                                      (if (nil? query)
;;                                        (get-in app-data key)
;;                                        (get (db->tree env {:query [{key query}]
;;                                                            :data  app-data
;;                                                            :refs  app-data}) key)
;;                                        ))
;;               ;; (cond
;;               ;;                        (nil? query) (if (= (second key) '_)
;;               ;;                                       (get app-data (first key))
;;               ;;                                       (get-in app-data key))
;;               ;;                        :else (get (db->tree env {:query [{key query}]
;;               ;;                                                  :data app-data
;;               ;;                                                  :refs app-data}) (first key)))
;;               ;; (resolve-ident env {:ident    key
;;               ;;                     :query    query
;;               ;;                     :app-data @state})

;;               ;; Key is really a key now. We're gonna get the value for it in
;;               ;; app-data, or in context-data if we have any.
;;               :else (let [value (get (or context-data app-data) key)]
;;                       (timbre/debug "resolve-join-to-value" value)
;;                       ;;return denormalized value
;;                       (cond
;;                         ;; We don't have a value for this key. If query is
;;                         ;;  recursive or '* we return the value found otherwise
;;                         ;;  we're going to go down the rabbit warren and see
;;                         ;;  what else we don't have in the subquery. Value is
;;                         ;;  set to what gets returned in a parse of the subquery
;;                         ;;  in this case (we might have idents and/or client
;;                         ;;  keys in the subqueries that are resolved against
;;                         ;;  app-data, not context-date which is empty at the
;;                         ;;  moment)
;;                         (contains? #{ nil :fetch} value)
;;                         (let [ast-query  (:query ast)
;;                               recursive? (or (number? ast-query) (= ast-query '...))]
;;                           (cond
;;                             (or (= query ['*]) recursive?) value
;;                             :else                          (pu/replace-idents
;;                                                             (parser (assoc env :context-data {}) query nil))))

;;                         ;; Ah, we have something, a list of things. Let's denormalize
;;                         ;; it and return it.
;;                         (vector? value) (get (db->tree env
;;                                                        {:query [(om/ast->query ast)]
;;                                                         :data {key value}
;;                                                         :refs app-data})
;;                                              key)

;;                         ;; The key is a (sub)root, since its value is a map.
;;                         ;; Let's call parser again, but now with the value of
;;                         ;; the key as context-data and the the query of the join
;;                         ;; as query.
;;                         (map? value) (pu/replace-idents
;;                                       (parser (assoc env :context-data value) query))

;;                         ;; Else just return whatever we've found
;;                         :else value))

;;               ;; (resolve-join-to-value env key)
;;               ))))

;; (defn remove-client-keys-from-remote [{:keys [subquery target default-remote] :as env} parser-result]
;;   (let [remote (get parser-result default-remote)]
;;     (cond-> parser-result
;;       (and (not subquery)
;;            ;; (some? target)
;;            (= target default-remote)
;;            (boolean remote)
;;            (map? remote))               ;root queries
;;       (update default-remote (fn [ast]
;;                                (update ast :query #(pu/remove-keys % (fn [k]
;;                                                                        (bu/client-only-key? k)))))))))
