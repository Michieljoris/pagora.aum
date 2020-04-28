(ns pagora.aum.om.process-roots
  (:require
   [pagora.aum.om.util :as util]))

(defn- merge-joins
  "Searches a query for duplicate joins and deep-merges them into a new query."
  [query]
  (letfn [(step [res expr]
            (if (contains? (:elements-seen res) expr)
              res ; eliminate exact duplicates
              (update-in
                (if (and (util/join? expr)
                         (not (util/union? expr))
                         (not (list? expr)))
                  (let [jk (util/join-key expr)
                        jv (util/join-value expr)
                        q  (or (-> res :query-by-join (get jk)) [])
                        nq (cond
                             (util/recursion? q) q
                             (util/recursion? jv) jv
                             :else (merge-joins (into [] (concat q jv))))]
                    (update-in res [:query-by-join] assoc jk nq))
                  (update-in res [:not-mergeable] conj expr))
                [:elements-seen] conj expr)))]
    (let [init {:query-by-join {}
                :elements-seen #{}
                :not-mergeable []}
          res  (reduce step init query)]
      (->> (:query-by-join res)
        (mapv (fn [[jkey jsel]] {jkey jsel}))
        (concat (:not-mergeable res))
        (into [])))))

(defn rewrite [rewrite-map result]
  (letfn [(step [res [k orig-paths]]
            (let [to-move (get result k)
                  res'    (reduce #(assoc-in %1 (conj %2 k) to-move)
                            res orig-paths)]
              (dissoc res' k)))]
    (reduce step result rewrite-map)))

(defn- move-roots
  "When given a join `{:join selector-vector}`, roots found so far, and a `path` prefix:
  returns a (possibly empty) sequence of [re-rooted-join prefix] results.
  Does NOT support sub-roots. Each re-rooted join will share only
  one common parent (their common branching point).
  "
  [join result-roots path]
  (letfn [(query-root? [join] (true? (-> join meta :query-root)))]
    (if (util/join? join)
      (if (query-root? join)
        (conj result-roots [join path])
        (let [joinvalue (util/join-value join)]
          (if (vector? joinvalue)
            (mapcat
             #(move-roots % result-roots
                          (conj path (util/join-key join)))
             joinvalue)
            result-roots)))
      result-roots)))

(defn process-roots [query]
  "A send helper for rewriting the query to remove client local keys that
   don't need server side processing. Give a query this function will
   return a map with two keys, :query and :rewrite. :query is the
   actual query you should send. Upon receiving the response you should invoke
   :rewrite on the response before invoking the send callback."
  (letfn [(retain [expr] [[expr []]]) ; emulate an alternate-root element
          (reroot [expr]
            (let [roots (move-roots expr [] [])]
              (if (empty? roots)
                (retain expr)
                roots)))
          (rewrite-map-step [rewrites [expr path]]
            (if (empty? path)
              rewrites
              (update-in rewrites [(util/join-key expr)] conj path)))]
    (let [reroots     (mapcat reroot query)
          query       (merge-joins (mapv first reroots))
          rewrite-map (reduce rewrite-map-step {} reroots)]
     {:query   query
      :rewrite (partial rewrite rewrite-map)})))
