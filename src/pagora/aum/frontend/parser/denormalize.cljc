(ns pagora.aum.frontend.parser.denormalize
  (:require
   [pagora.aum.dev.debug :refer [warn-when now-in-ms]]
   [pagora.aum.frontend.parser.denormalize-hooks :refer [db->tree-hooks]]
   [pagora.aum.om.db-to-tree :refer [reduce-query-depth reduce-union-recursion-depth mappable-ident?]]
   [pagora.aum.om.util :as om-util]

   #?@(:clj  [;; [transcriptor :as check :refer [returns!]]
              [taoensso.tufte :as tufte :refer (defnp p profiled profile)]]
       :cljs [[taoensso.tufte :as tufte :refer-macros (defnp p profiled profile)]])
   [taoensso.timbre :as timbre]))


;;Copied and refactored from db->tree in the om-next repo. Commit used:
;; commit df21993712a948a2365fdfe776ebbddf7497e4df (origin/master, origin/HEAD)
;; Author: David Nolen <david.nolen@gmail.com>
;; Date:   Wed Jan 31 16:23:21 2018 +0000

;;     beta2

;;As of Oct 2018 the original db->tree has not been modified since. In general only very
;;little changes have been made to om-next. Currently om-next is on beta-4

;;Basically this version of db->tree lets you add hooks for resolving props and
;;joins. It also expects env as first param so you can use it when resolving the
;;prop or join. I also pulled the db->tree fn apart somewhat and added comments

(defn resolve-ident-recursively-to-data [ident-or-data refs map-ident]
  (loop [ident ident-or-data]
    (if (mappable-ident? refs ident)
      (recur (get-in refs (map-ident ident)))
      ident)))

(defn resolve-ident-recursively-to-ident [ident-or-data refs map-ident]
  (if (mappable-ident? refs ident-or-data)
    (loop [ident ident-or-data]
      (let [resolved-ident (get-in refs (map-ident ident))]
        (if (mappable-ident? refs resolved-ident)
          (recur (get-in refs (map-ident ident)))
          (map-ident ident))))
    ident-or-data))

(declare denormalize*)

(defn combine-queries
  "Return the union of the queries, deduplicated. For every join again return the
  deduplicated union, recursively"
  [queries]
  (if (contains? (set queries) '...)
    ;;Return the most inclusive query.
    '...
    (let [group-unique-props-and-joins
          (fn [a e]
            (if (om-util/join? e)
              (let [ [join params] (cond-> e
                                     (not (seq? e)) list)
                    [key query] (om-util/join-entry join)]
                (update a :joins conj {:key key :query query :params params}))
              (update a :props conj e)))
          {:keys [props joins]}
          (reduce (fn [a q]
                    (reduce group-unique-props-and-joins a q))
                  {:props #{}
                   :joins #{}}
                  queries)
          joins (group-by :key joins)
          joins (map (fn [[k join-variations]]
                       (let [query-variations (map :query join-variations)
                             params (-> join-variations first :params)
                             join {k (combine-queries query-variations)}]
                         (cond-> join
                           (some? params)  (list params))))
                     joins)]
      (into [] (concat props joins)))))



(defn resolve-props [{:keys [resolve-prop sparsify-query?] :as env} props data refs]
  (let [;;Lift prop from list with params
        props (mapv #(cond-> % (seq? %) first) props)]
    (loop [props          props
           resolved-props (if sparsify-query? nil {})]
      (if (seq props)
        (let [prop          (first props)
              resolved-prop (resolve-prop env prop data refs)]
          (recur (next props) (if sparsify-query?
                                (cond-> resolved-props
                                  resolved-prop (conj prop))
                                (cond-> resolved-props
                                  (some? resolved-prop) (assoc prop resolved-prop)))))
        resolved-props))))

(defn resolve-query-against-vector [{:keys [resolve-join sparsify-query? denormalize-data map-ident] :as env}
                                    query data refs ;; map-ident
                                    ]
  (let [{:keys [idents-seen union-expr recurse-key union-recur? joins-seen]} denormalize-data
        step (fn [ident-or-data]
               ;;Data vector element is an ident:
               (if (mappable-ident? refs ident-or-data)
                 (let [ ;;Resolve the ident
                       ident       ident-or-data
                       data        (get-in refs (map-ident ident))
                       query       (cond-> query
                                     union-recur? (reduce-union-recursion-depth recurse-key))
                       ;; also reduce query depth of union-seen, there can
                       ;; be more union recursions inside
                       union-seen' (cond-> union-expr
                                     union-recur? (reduce-union-recursion-depth recurse-key))
                       ;;If query is a map, so a union then use the query
                       ;;under the key which is the same as the first
                       ;;element of the ident vector. Which is how unions
                       ;;get resolved.
                       query'      (cond-> query
                                     (map? query) (get (first ident)))] ;; UNION
                   ;;Resolve the query against the looked up ident.
                   (denormalize* (assoc env :denormalize-data {:idents-seen idents-seen
                                                               :union-expr union-seen'
                                                               :recurse-key nil
                                                               :joins-seen joins-seen}
                                        ;; idents-seen union-seen' nil joins-seen
                                        ) query' data refs
                                 ;; map-ident
                                 ;; denormalize-data
                                 ;; idents-seen union-seen' nil joins-seen
                                 ))

                 ;;Data vector element is a map:
                 (if (= query '[*])
                   ;;Return whatever we're querying against.
                   (if sparsify-query?
                     (if (some? data)
                       nil
                       query)
                     ident-or-data)
                   ;;We need to resolve query against the data (which is a map or unmappable ident)
                   (let [data                     ident-or-data
                         meta-data (meta data)
                         ;;Split query in props and joins
                         {props false joins true} (group-by om-util/join? query)

                         resolved-joins (loop [joins          (seq joins)
                                               resolved-joins (if sparsify-query? nil {})]
                                          (if (seq joins)
                                            (let [join          (first joins)
                                                  [join params] (cond-> join
                                                                  (not (seq? join)) list)
                                                  [k subquery]  (om-util/join-entry join)
                                                  context-data  (get data k)
                                                  resolved-join (resolve-join (assoc env :denormalize-data
                                                                                     {:joins-seen (assoc joins-seen k subquery)
                                                                                      :idents-seen idents-seen
                                                                                      :union-expr union-expr
                                                                                      :recurse-key recurse-key})
                                                                              k subquery params context-data refs)]
                                              (recur (next joins)
                                                     (if sparsify-query?
                                                       (cond-> resolved-joins
                                                         (some? resolved-join) (conj resolved-join))
                                                       (assoc resolved-joins k resolved-join))))
                                            resolved-joins))

                         ;;Props can have params, raise prop from the list
                         resolved-props (resolve-props env props data refs)]
                     ;;Return fully resolved query for this vector element
                     (if sparsify-query?
                       (when-let [result (into resolved-joins resolved-props)]
                         (into [] result))
                       (with-meta (merge resolved-props resolved-joins) meta-data))))))]

    ;;Because the data was vector we return a vector of results. With each
    ;;element the result of applying the query on the corresponding element
    ;;in the data vector
    (if sparsify-query?
      ;;Some resolved remote queries might return different data than others. So we need to return
      ;;a query that's leaving out only the keys that exist in every element of the data vector.
      (let [result (into [] (map step) data)
            result (combine-queries result)]
        (when (seq result)
          result))
      ;;The data is a vector, so we return the resolved queries  as a vector
      (into [] (map step) data))))

(defn resolve-query-against-map [{:keys [resolve-join sparsify-query? denormalize-data
                                         map-ident] :as env}
                                 query data refs]
  (let [{:keys [idents-seen union-expr joins-seen]} denormalize-data
        {props false joins true} (group-by #(or (om-util/join? %)
                                                ;;We're chucking idents with the joins
                                                (om-util/ident? %)
                                                ;;And idents with  params
                                                (and (seq? %) ;;list?
                                                     (om-util/ident? (first %))))
                                           query)
        meta-data (meta data)]
    ;;Resolving each join of the query
    (loop [joins          (seq joins)
           resolved-joins (if sparsify-query? nil {})]
      (if-not (nil? joins)
        (let [join (first joins)
              ;;We chucked idents with our joins. Which are bare. Let's
              ;;add a query to it.
              join           (cond-> join
                               (om-util/ident? join) (hash-map '[*]))
              ;;Lift from list if params are added, eg: ({:a [:b :c]} {:some :params})
              [join params] (cond-> join
                              (not (seq? join)) list)
              [key subquery] (om-util/join-entry join)

              recurse?    (om-util/recursion? subquery) ;;number or ...
              recurse-join? (keyword? subquery)
              _ (when recurse-join?
                  (when-not (contains? joins-seen subquery)
                    (throw (ex-info
                            "You can use a keyword to create a recursive query, but it needs to refer to an already seen join!!"
                            {:KEY key :SUBQUERY subquery
                             :JOINS-SEEN joins-seen
                             })))
                  )
              recurse-key (when recurse? key)

              ;;Look up the value for the key, or ident as may be the case.
              context-data (if (om-util/ident? key)
                             (if (= '_ (second key))
                               (get refs (first key))
                               (get-in refs (map-ident key)))
                             (get data key))

              key (cond-> key (om-util/unique-ident? key) first)

              ;;If context-data is an ident and it resolves again to
              ;;an ident use the latest ident in the recursive chain.
              context-data (resolve-ident-recursively-to-ident context-data refs map-ident)
              limit        (if (number? subquery) subquery :none)
              union-entry  (if (om-util/union? join) ;;subquery is a map
                             subquery
                             (when recurse?
                               union-expr))

              subquery     (cond
                             recurse-join? (get joins-seen subquery)
                             recurse?
                             (if-not (nil? union-expr)
                               union-entry
                               (reduce-query-depth query key))

                             (and (mappable-ident? refs context-data)
                                  (om-util/union? join))
                             (get subquery (first context-data))

                             (and (om-util/ident? key)
                                  (om-util/union? join))
                             (get subquery (first key))

                             :else subquery)

              recurse? (or recurse? recurse-join?)
              graph-loop? (and recurse?
                               (contains? (set (get idents-seen key)) context-data)
                               (= :none limit))
              idents-seen (if (and (mappable-ident? refs context-data) recurse?)
                            (-> idents-seen
                                (update-in [key] (fnil conj #{}) context-data)
                                (assoc-in [:last-ident key] context-data))
                            idents-seen)
              extract-key (fn [k]
                            (or
                             (cond-> k
                               (om-util/ident? k) (-> first
                                                      namespace
                                                      keyword))
                             k))]

          (if (or (= 0 limit)
                  graph-loop?
                  ;;If context-data is nil we keep on parsing deeper into query
                  ;;regardless unless the query is recursive. This is so all
                  ;;subkeys get a chance to resolve.
                  (and (nil? context-data) recurse?))
            ;;We skip the key altogether if we reached the recurse
            ;;depth, we encountered a loop or there's just no
            ;;context-data to resolve against
            (recur (next joins) (cond-> resolved-joins
                                  sparsify-query? (conj join)))

            ;;Otherwise we do our best to resolve it.
            (let [resolved-join (resolve-join (assoc env :denormalize-data {:idents-seen idents-seen
                                                                            :joins-seen (assoc joins-seen
                                                                                               (extract-key key) subquery)
                                                                            :union-expr union-entry
                                                                            :recurse-key recurse-key})
                                              key subquery params (or context-data {}) refs
                                              )
                  resolved-joins (if sparsify-query?
                                   (cond-> resolved-joins
                                     (and (not (and (contains? (set (keys data)) key)
                                                    (nil? context-data)))
                                          resolved-join)
                                     (conj resolved-join))
                                   (assoc resolved-joins key resolved-join))]
              (recur (next joins) resolved-joins))))
        ;;joins is nil
        (if-let [looped-key (some
                             (fn [[k identset]]
                               (if (contains? identset (get data k))
                                 (get-in idents-seen [:last-ident k])
                                 nil))
                             (dissoc idents-seen :last-ident))]
          looped-key
          (let [ ;;Resolve the props
                resolved-props (resolve-props env props data refs)]

            (if sparsify-query?
              (when (or (seq resolved-joins) (seq resolved-props))
                (into (into  [] resolved-props) (into [] resolved-joins)))
              ;;Return fully resolved query for this data
              (with-meta (merge resolved-props resolved-joins) meta-data))))))))

(defn- denormalize*
  "Denormalize a data based on query. refs is a data structure which maps idents
   to their values. map-ident is a function taking a ident to another ident,
   used during tempid transition. idents-seen is the set of idents encountered,
   used to limit recursion. union-expr is the current union expression being
   evaluated. recurse-key is key representing the current recursive query being
   evaluted."
  [{:keys [denormalize-data map-ident] :as env} query data refs
   ;; map-ident
   ;; {:keys [idents-seen union-expr recurse-key joins-seen] :as denormalize-data}
   ;; idents-seen union-expr recurse-key joins-seen
   ]
  ;; If we're gonna resolve a query against data we're basically selecting the
  ;; keys of the query from the data map. Or from each map in a vector.
  ;; {:pre [(or (map? query) (vector? query)) ;;vector of props and joins or union map
  ;;        (or (map? data) (vector? data))]}

  (let [{:keys [idents-seen union-expr recurse-key joins-seen]} denormalize-data
        union-recur? (and union-expr recurse-key)
        recur-ident  (when union-recur?
                       data)
        ;;If data is an ident look it up and set that as data, do this
        ;;recursively
        data         (resolve-ident-recursively-to-data data refs map-ident)]

    (cond
      (vector? data) (resolve-query-against-vector (update env :denormalize-data
                                                           assoc :union-recur? union-recur?)
                                                   query data refs
                                                   ;; map-ident
                                                   ;; idents-seen union-expr recurse-key union-recur? joins-seen
                                                   )

      (and (map? query) union-recur?)
      (let [;;Get relevant part of union query
            query (get query (first recur-ident))]
        (denormalize* env query data refs
                      ;; map-ident
                      ;; denormalize-data
                      ;; idents-seen union-expr recurse-key joins-seen
                      ))

      ;; Query against a map
      :else (if (= '[*] query)
              (if (:sparsify-query? env)
                (if (some? data)
                  nil
                  query)
                data)
              ;;We've got a real query and a map. We need to resolve the first against the second one.
              (resolve-query-against-map (assoc env :denormalize-data {:idents-seen idents-seen
                                                                       :union-expr union-expr
                                                                       :joins-seen joins-seen})
                                         query data refs
                                         ;; map-ident
                                         ;; denormalize-data
                                         ;; idents-seen union-expr joins-seen
                                         )))))

(defnp default-resolve-prop [env prop data refs]
  (get data prop))

(defn default-resolve-join
  "Receives env, the key and query of the join and app-data (refs).
  Expected to return the result of applying the query to the data
  using the refs to lookup idents."
  [env key query params data refs]
  ;; (timbre/info "Default resolve join" key query)
  (denormalize* env query data refs
                ;; map-ident
                ;; (:denormalize-data env)
                ))

(defn default-check-for-prop
  "Returns true if prop needs to stay in the query"
  [env prop data refs]
  (or (nil? data)
      (not (map? data))
      (not (contains? (set (keys data)) prop))))

;; (default-check-for-prop nil :a nil nil)

(defn default-resolve-join-to-query
  "Receives env, the key and query of the join and app-data (refs).
  Expected to return join if join needs to stay in the query "
  [env key query params data refs]

  (let [result (denormalize* env query data refs
                             ;; map-ident
                             ;; denormalize-data
                             ;; idents-seen union-expr recurse-key joins-seen
                             )]
    (when (some? result)
      (cond-> {key result}
        params (list params)))))

(defn db->tree
  "Given a query, some data in the default database format, and the entire
   application state in the default database format, return the tree where all
   ident links have been replaced with their original node values."
  [{:keys [denormalize-data] :as env}
   {:keys [query data refs map-ident resolve-prop resolve-join check-for-prop resolve-join-to-query
           sparsify-query?]
    :or   {map-ident  identity
           resolve-prop default-resolve-prop
           resolve-join default-resolve-join
           check-for-prop default-check-for-prop
           resolve-join-to-query default-resolve-join-to-query
           denormalize-data {:idents-seen {}
                             :union-expr nil
                             :recurse-key nil
                             :joins-seen {}}
           }}]
  {:pre [(map? refs)]}
  ;; (timbre/info :#r "IN MY DB->TREE!!!!!!")

  (p ::denormalize* (denormalize* (assoc env
                                         :sparsify-query? sparsify-query?
                                         :map-ident map-ident
                                         :default-resolve-prop default-resolve-prop
                                         :default-resolve-join default-resolve-join
                                         :default-check-for-prop default-check-for-prop
                                         :default-resolve-join-to-query default-resolve-join-to-query

                                         :resolve-prop (if sparsify-query? check-for-prop resolve-prop)
                                         :resolve-join (if sparsify-query? resolve-join-to-query resolve-join)
                                         :denormalize-data denormalize-data)
                                  query data refs
                                  ;; denormalize-data
                                  ;; idents-seen union-expr recurse-key joins-seen
                                  )))

(comment
  (try
    (let [query
          '[{:group [:id :name
                     {:dossier-type [:id
                                     {:dossier-type
                                      ...}]}]}]

          ;; [{[:foo/by-id 1] [:f-id {:join [:j-id {:foo :foo}]}]}]
          refs {
                :foo/by-id {1 {:f-id 1 :join [[:join/by-id 1]]}
                            2 {:f-id 2  :join [[:join/by-id 2]]}
                            3 {:f-id 3}
                            }
                :join/by-id {1  {:j-id 1 :foo [[:foo/by-id 2]]}
                             2  {:j-id 2 :foo [[:foo/by-id 3]]}}
                :dossier-type/by-id {1 {:id 1 :dossier-type []}
                                     2 {:id 2}}
                :group/by-id {1 {:id 1 :name "groupone"
                                 :dossier-type [[:dossier-type/by-id 1]
                                                [:dossier-type/by-id 2]]}}
                }
          context-data
          {:group [[:group/by-id 1]]
           }


          result (db->tree
                  {:state (atom {})} ;;env
                  {:query query
                   :data context-data
                   :refs refs
                   :sparsify-query? true

                   :resolve-prop aum.frontend.reconciler.parser.denormalize-hooks/resolve-prop
                   :resolve-join aum.frontend.reconciler.parser.denormalize-hooks/resolve-join
                   :check-for-prop aum.frontend.reconciler.parser.denormalize-hooks/check-for-prop
                   :resolve-join-to-query aum.frontend.reconciler.parser.denormalize-hooks/resolve-join-to-query
                   })]
      result
      )
    (catch Exception e
      (timbre/info e)
      )
    ;; =>

    ;; (check/returns!
    ;;  [{:dossier-type [:id {:dossier-type '...}]}]

    ;;  )
    ))


(defn db->tree-with-hooks [{:keys [db->tree-hooks]:as env}
                          {:keys [query data refs ignore-hooks?] :as args}]
  (let [args (cond-> args
               (not ignore-hooks?) (merge db->tree-hooks))]
    (p :db->tree (db->tree env args))))

;; (defn run-test []
;;   (try
;;     (let [s (now-in-ms)
;;           result (profile {} (db->tree-with-hooks
;;                               {:state (atom app-state)
;;                                :query-root :om.next/root
;;                                :db->tree-hooks db->tree-hooks
;;                                :db->tree db->tree-with-hooks} ;;env
;;                               {:query big-query
;;                                :data app-state
;;                                :refs app-state
;;                                }))
;;           dt (warn-when s 0 "db->tree")
;;           ]

;;       ;; (pprint result)
;;       ;; (timbre/info "db->tree took" dt "ms")
;;       )
;;     (catch #?(:clj Exception
;;               :cljs :default) e
;;       (timbre/info e)
;;       )
;;     )
;;   )


(comment
  (run-test)
  )





;; #?(:clj
   ;; (check/run))
