(ns pagora.aum.om.focus
  (:require
   [pagora.aum.om.util :as util]))

(declare focus-query*)

(defn- focused-join [expr ks full-expr union-expr]
  (let [expr-meta (meta expr)
        expr' (cond
                (map? expr)
                (let [join-value (-> expr first second)
                      join-value (if (and (util/recursion? join-value)
                                          (seq ks))
                                   (if-not (nil? union-expr)
                                     union-expr
                                     full-expr)
                                   join-value)]
                  {(ffirst expr) (focus-query* join-value ks nil)})

                (seq? expr) (list (focused-join (first expr) ks nil nil) (second expr))
                :else       expr)]
    (cond-> expr'
      (some? expr-meta) (with-meta expr-meta))))

(defn- focus-query*
  [query path union-expr]
  (if (empty? path)
    query
    (let [[k & ks] path]
      (letfn [(match [x]
                (= k (util/join-key x)))
              (value [x]
                (focused-join x ks query union-expr))]
        (if (map? query) ;; UNION
          {k (focus-query* (get query k) ks query)}
          (into [] (comp (filter match) (map value) (take 1)) query))))))

(defn focus-query
  "Given a query, focus it along the specified path.

  Examples:
    (pagora.aum.om.next/focus-query [:foo :bar :baz] [:foo])
    => [:foo]

    (pagora.aum.om.next/focus-query [{:foo [:bar :baz]} :woz] [:foo :bar])
    => [{:foo [:bar]}]"
  [query path]
  (focus-query* query path nil))

(defn focus->path
  "Given a focused query return the path represented by the query.

   Examples:

     (pagora.aum.om.next/focus->path [{:foo [{:bar {:baz []}]}])
     => [:foo :bar :baz]"
  ([focus]
   (focus->path focus '* []))
  ([focus bound]
   (focus->path focus bound []))
  ([focus bound path]
   (if (and (or (= bound '*)
                (and (not= path bound)
                     (< (count path) (count bound))))
            (some util/join? focus)
            (== 1 (count focus)))
     (let [[k focus'] (util/join-entry (first focus))
           focus'     (if (util/recursion? focus')
                        focus
                        focus')]
       (recur focus' bound (conj path k)))
     path)))

;; (focus->path [{:foo [{:d ['...]}]}])

;; (util/join? [{:foo [{:d [:e]} {:bar {:baz []}}]}])
