(ns pagora.aum.om.db-to-tree
  (:refer-clojure :exclude #?(:clj  [replace]
                              :cljs [replace key]))
  (:require
   [pagora.aum.om.util :as util]
   [pagora.aum.om.focus :refer [focus-query]]
   [pagora.aum.om.misc :refer [query-template replace]]
   [clojure.zip :as zip]
   )
  )

(defn reduce-query-depth
  "Changes a join on key k with depth limit from [:a {:k n}] to [:a {:k (dec n)}]"
  [q k]
  (if-not (empty? (focus-query q [k]))
    (let [pos (query-template q [k])
          node (zip/node pos)
          node' (cond-> node (number? node) dec)]
      (replace pos node'))
    q))

(defn reduce-union-recursion-depth
  "Given a union expression decrement each of the query roots by one if it
   is recursive."
  [union-expr recursion-key]
  (->> union-expr
    (map (fn [[k q]] [k (reduce-query-depth q recursion-key)]))
    (into {})))

(defn mappable-ident? [refs ident]
  (and (util/ident? ident)
       (contains? refs (first ident))))


;; TODO: easy to optimize

(defn- denormalize*
  "Denormalize a data based on query. refs is a data structure which maps idents
   to their values. map-ident is a function taking a ident to another ident,
   used during tempid transition. idents-seen is the set of idents encountered,
   used to limit recursion. union-expr is the current union expression being
   evaluated. recurse-key is key representing the current recursive query being
   evaluted."
  [query data refs map-ident idents-seen union-expr recurse-key]
  ;; support taking ident for data param
  (let [union-recur? (and union-expr recurse-key)
        recur-ident (when union-recur?
                      data)
        data (loop [data data]
               (if (mappable-ident? refs data)
                 (recur (get-in refs (map-ident data)))
                 data))]
    (cond
      (vector? data)
      ;; join
      (let [step (fn [ident]
                   (if-not (mappable-ident? refs ident)
                     (if (= query '[*])
                       ident
                       (let [{props false joins true} (group-by util/join? query)
                             props (mapv #(cond-> % (seq? %) first) props)]
                         (loop [joins (seq joins) ret {}]
                           (if-not (nil? joins)
                             (let [join        (first joins)
                                   [key sel]   (util/join-entry join)
                                   v           (get ident key)]
                               (recur (next joins)
                                 (assoc ret
                                   key (denormalize* sel v refs map-ident
                                         idents-seen union-expr recurse-key))))
                             (merge (select-keys ident props) ret)))))
                     (let [ident'       (get-in refs (map-ident ident))
                           query        (cond-> query
                                          union-recur? (reduce-union-recursion-depth recurse-key))
                           ;; also reduce query depth of union-seen, there can
                           ;; be more union recursions inside
                           union-seen'  (cond-> union-expr
                                          union-recur? (reduce-union-recursion-depth recurse-key))
                           query'       (cond-> query
                                          (map? query) (get (first ident)))] ;; UNION
                       (denormalize* query' ident' refs map-ident idents-seen union-seen' nil))))]
        (into [] (map step) data))

      (and (map? query) union-recur?)
      (denormalize* (get query (first recur-ident)) data refs map-ident
        idents-seen union-expr recurse-key)

      :else
      ;; map case
      (if (= '[*] query)
        data
        (let [{props false joins true} (group-by #(or (util/join? %)
                                                      (util/ident? %)
                                                      (and (seq? %)
                                                           (util/ident? (first %))))
                                         query)
              props (mapv #(cond-> % (seq? %) first) props)]
          (loop [joins (seq joins) ret {}]
            (if-not (nil? joins)
              (let [join        (first joins)
                    join        (cond-> join
                                  (seq? join) first)
                    join        (cond-> join
                                  (util/ident? join) (hash-map '[*]))
                    [key sel]   (util/join-entry join)
                    recurse?    (util/recursion? sel)
                    recurse-key (when recurse? key)
                    v           (if (util/ident? key)
                                  (if (= '_ (second key))
                                    (get refs (first key))
                                    (get-in refs (map-ident key)))
                                  (get data key))
                    key         (cond-> key (util/unique-ident? key) first)
                    v           (if (mappable-ident? refs v)
                                  (loop [v v]
                                    (let [next (get-in refs (map-ident v))]
                                      (if (mappable-ident? refs next)
                                        (recur next)
                                        (map-ident v))))
                                  v)
                    limit       (if (number? sel) sel :none)
                    union-entry (if (util/union? join)
                                  sel
                                  (when recurse?
                                    union-expr))
                    sel         (cond
                                  recurse?
                                  (if-not (nil? union-expr)
                                    union-entry
                                    (reduce-query-depth query key))

                                  (and (mappable-ident? refs v)
                                       (util/union? join))
                                  (get sel (first v))

                                  (and (util/ident? key)
                                       (util/union? join))
                                  (get sel (first key))

                                  :else sel)
                    graph-loop? (and recurse?
                                  (contains? (set (get idents-seen key)) v)
                                  (= :none limit))
                    idents-seen (if (and (mappable-ident? refs v) recurse?)
                                  (-> idents-seen
                                    (update-in [key] (fnil conj #{}) v)
                                    (assoc-in [:last-ident key] v)) idents-seen)]
                (cond
                  (= 0 limit) (recur (next joins) ret)
                  graph-loop? (recur (next joins) ret)
                  (nil? v)    (recur (next joins) ret)
                  :else       (recur (next joins)
                                (assoc ret
                                  key (denormalize* sel v refs map-ident
                                        idents-seen union-entry recurse-key)))))
              (if-let [looped-key (some
                                    (fn [[k identset]]
                                      (if (contains? identset (get data k))
                                        (get-in idents-seen [:last-ident k])
                                        nil))
                                    (dissoc idents-seen :last-ident))]
                looped-key
                (merge (select-keys data props) ret)))))))))

(defn db->tree
  "Given a query, some data in the default database format, and the entire
   application state in the default database format, return the tree where all
   ident links have been replaced with their original node values."
  ([query data refs]
   {:pre [(map? refs)]}
   (denormalize* query data refs identity {} nil nil))
  ([query data refs map-ident]
   {:pre [(map? refs)]}
   (denormalize* query data refs map-ident {} nil nil)))
