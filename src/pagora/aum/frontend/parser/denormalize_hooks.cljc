(ns pagora.aum.frontend.parser.denormalize-hooks
  "
  This namespace is one way to implement the hooks that denormalize/db->tree offers. In short:

  - The find-read-fn (in the env) is used to look up if there's a read
  method for the particular target and key. If so then that's used to
  resolve the prop or join. If not then:

  - Any prop or join which has a key that's is namespaced and starts
  with ':client' is looked up in the root app-state and not sent to
  the remote, Client keys namespaced as 'client-prop' are looked up as
  normal.

  - Revert to default resovling if above two cases don't apply
  "
  (:require
   [pagora.aum.util :as au]
   [pagora.aum.om.next.impl.parser :as om-parser]
   [pagora.aum.om.util :as om-util]
   [taoensso.timbre :as timbre]
   #?@(:clj  [[taoensso.tufte :as tufte :refer (defnp p profiled profile)]]
       :cljs [[taoensso.tufte :as tufte :refer-macros (defnp p profiled profile)]])))


(defn resolve-prop [{:keys [default-resolve-prop db->tree find-read-fn] :as env} prop data refs]
  ;;TODO: if value is an ident you could resolve (use denorm/resolve-ident-recursively-to-data).
  (let [result (if-let [read-fn (p :find-read-fn-prop (find-read-fn :value prop))]
                 (do
                   ;; (timbre/info :g "Found read-fn" prop)

                   (read-fn (assoc env
                                   :context-data data
                                   :query nil
                                   :ast (om-parser/expr->ast prop))
                            prop
                            nil)) ;;params, but db->tree throws them away, so we don't have them

                 (cond
                   (au/root-client-key? prop) (let [value (get refs prop)]
                                                (cond
                                                  (om-util/ident? value) (if (= (second value) '_)
                                                                           (get refs (first value))
                                                                           (get-in refs value))
                                                  (vector? value)        (db->tree env
                                                                                   {:query [{prop ['*]}]
                                                                                    :data  refs
                                                                                    :refs  refs})
                                                  :else                  value))
                   :else                      (default-resolve-prop env prop data refs)))]

    result))

(defn resolve-join
  [{:keys [default-resolve-join db->tree find-read-fn] :as env} key query params data refs]
  ;; (when (or true (= key :route/companies)) (timbre/debug "Resolve join" key))
  ;; (timbre/info "Resolve join:" key)

  (let [result (if-let [read-fn (p :find-read-fn-join (find-read-fn :value key))]
                 (do
                   ;; (timbre/info :#g "Found read-fn for:" key)
                   (read-fn (assoc env
                                   :context-data data
                                   :query query
                                   :ast (om-parser/join->ast {key query})
                                   )
                            key
                            params))
                 (cond
                   (au/root-client-key? key) (let [value (get refs key)]
                                               (db->tree env
                                                         {:query query
                                                          :data  value
                                                          :refs  refs}))
                   :else                     (default-resolve-join env key query params data refs)))]
    ;; (when  (or true (= key :route/companies))
    ;;   (timbre/debug "Result for resolve-join" key)
    ;;   (timbre/debug :#pp result)
    ;;   )
    result))

(defn check-for-prop [{:keys [default-check-for-prop target find-read-fn] :as env} prop data refs]
  ;; (timbre/debug "Check for prop" prop)
  (let [result (if-let [read-fn (find-read-fn target prop)]
                 (do
                   ;; (timbre/info :#g "Found read-fn for:" prop)
                   (read-fn (assoc env
                                   :context-data data
                                   :query nil
                                   :ast (om-parser/expr->ast prop))
                            prop
                            nil)) ;;params, but db->tree throws them away, so we don't have them

                 (cond
                   (au/client-only-key? prop) nil
                   :else                      (default-check-for-prop env prop data refs)))]

    result))

(defn resolve-join-to-query
  [{:keys [default-resolve-join-to-query target find-read-fn] :as env} key query params data refs]
  ;; (when (or true (= key :supergroup-search-for-group)) (timbre/debug :#p "Resolve join to query" key))
  (let [result (if-let [read-fn (find-read-fn target key)]
                 (let [;; _    (timbre/info :#g "Found read-fn for:" key)
                       ast    (assoc (om-parser/join->ast {key query}) :params params)
                       sparse-query (read-fn (assoc env
                                                    :context-data data
                                                    :query query
                                                    :ast ast)
                                             key
                                             params)
                       sparse-query    (when sparse-query
                                         (if (true? sparse-query)
                                           (do
                                            (timbre/debug "default-resolve-to-query" key query)
                                             (default-resolve-join-to-query env key query params data refs))
                                           sparse-query))]
                   sparse-query
                   )
                 (cond
                   (au/client-only-key? key) nil
                   ;; (om-util/ident? key)      nil
                   :else                     (default-resolve-join-to-query env key query params data refs)))]
    ;; (when (or true (= key :supergroup-search-for-group))
    ;;   (timbre/debug :#r "Result for resolve-join-to-query" key)
    ;;   (timbre/debug :#pp result)
    ;;   )
    result
    ))

(def db->tree-hooks
  {:resolve-prop resolve-prop
   :resolve-join resolve-join
   :check-for-prop check-for-prop
   :resolve-join-to-query resolve-join-to-query})
