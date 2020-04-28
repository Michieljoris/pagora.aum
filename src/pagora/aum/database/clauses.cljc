(ns pagora.aum.database.clauses
  (:require [pagora.clj-utils.core :as cu]
            [pagora.aum.database.build-sql :as build-sql]
            [taoensso.timbre :as timbre]
            [cuerdas.core :as str]))

(def order-by->sqlvec build-sql/order-by-snip)

(defn get-p-type [p]
  (cond (or (= p :null) (= p :NULL)) :n
        (keyword? p) :i
        (or (sequential? p) (set? p)) :v*
        :else :v))

(def operators [:= :<> :> :< :>= :<= :LIKE :IN :like :in :is :IS :is-not :IS-NOT])

(defn assert-value [p]
  (if-not (or (string? p) (number? p))
    (throw (ex-info
            (str p " should be a string or number") {}))))

(defn assert-col
  "Throws if cols is not nil and c is not in cols"
  [c cols]
  (if-not (or (nil? cols) (cu/includes? cols c))
    (throw (ex-info
            (str c " is not an allowed column for this table") {:col c :cols cols}))))

(defn assert-op [op]
  (if-not (or (= op :in) (= op :IN))
    (throw (ex-info
            (str "only :in operator is allowed when second value is a list or vector")
            {:op op}))))

(defn assert-is [op]
  (if-not (cu/includes? [:is :is-not :IS-NOT :IS] op)
    (throw (ex-info
            (str "only :is or :is-not operator is allowed when second value is a :null")
            {:op op}))))

(defn lookup-prop
  "If p is namespaced at all, then return value of name of p in
  props"
  [props p]
  (let [prop? (and (keyword? p) (namespace p))]
    (when prop?
      (let [prop-keyword (keyword (name p))]
        (if (contains? props prop-keyword)
          (get props prop-keyword)
          (throw (ex-info (str "Condition requires a prop called " p " but is not found") {:props props})))))))

(defn cond->cond-snip-params [{:keys [table table-name alias-prefix props cols
                                      no-prefix-table-name cond]
                               [p1 op p2] :cond}]
  (if-not (cu/includes? operators op)
    (throw (ex-info  (str op " is not an operator") {:cond [p1 op p2]})))
  (let [alias-prefix (or (:alias-prefix (meta cond)) alias-prefix)
        p1 (or (lookup-prop props p1) p1)
        p2 (or (lookup-prop props p2) p2)
        found-cols []
        p1-type (get-p-type p1)
        found-cols (if (= p1-type :i) (conj found-cols p1) [])
        p2-type (get-p-type p2)
        found-cols (if (= p2-type :i) (conj found-cols p2) found-cols)
        p-types (keyword (str (name p1-type) (name p2-type)))
        ]
    (when (= p1-type :v) (assert-value p1))
    (when (and cols (= p1-type :i)) (assert-col p1 cols))
    (when (= p2-type :v) (assert-value p2))
    (when (and cols (= p2-type :i)) (assert-col p2 cols))
    (when (= p-types :iv*) (assert-op op) )
    (when (= p-types :in) (assert-is op) )
    (when-not (cu/includes? [:iv :iv* :vi :ii :vv :in] p-types)
      (throw (ex-info
              (str "comparison is not correct for " p1 " and " p2)
              {:cond [p1 op p2]})))
    (let [prefix-table-name (fn [p] (cond->> p
                                      (not (or no-prefix-table-name (str/includes? p ".")))
                                      (str (build-sql/q (str alias-prefix table-name)) ".")))
          p1 (if (= p1-type :i)
               ((comp prefix-table-name cu/hyphen->underscore name) (build-sql/q p1))
               p1)
          p2 (if (= p2-type :i)
               ((comp prefix-table-name cu/hyphen->underscore name) (build-sql/q p2))
               p2)
          p2 (if (= p2-type :n) "NULL" p2)
          op (str/replace (name op) "-" " ")]
      {:cond [p1 op p2] :p-types p-types :found-cols found-cols})))

(defn clause? [[k v]]
  (and (keyword? k) (vector? v)))

;; TODO: memoize!!
(defn conds->sqlvec
  "Takes a list of valid columns and a vector such as [:or [[:a :<
  1] [:and [[:b :in [1 2]] [:c :< 3]]]]] and returns a sqlvec to be
  passed to a hugsql db query as the where param. Pass nil for cols to
  return vector of column keys uses in conds."
  [{:keys [table table-name alias-prefix props cols conds derive-cols?] :as params}]
  (if derive-cols?
    (let [cols (conds->sqlvec (assoc params :cols nil :derive-cols? false))]
      (conds->sqlvec (assoc params :cols cols :derive-cols? false)))
    (let [found-cols (atom [])]
      (letfn [(make-sqlvec [prefix conds]
                (if (clause? conds)
                  (let [logical-operator (first conds)
                        conds (second conds)
                        n-conds (count conds)]
                    (if-not (cu/includes? [:or :and :OR :AND] logical-operator)
                      (throw (ex-info
                              (str "unknown logical operator " logical-operator) {})))
                    (if-not (pos? n-conds)
                      (throw (ex-info  (str "empty " logical-operator " clause") {})))
                    (condp = n-conds
                      1 (make-sqlvec prefix (first conds))
                      (let [logical-operator (str/upper (name logical-operator))
                            first-cond (make-sqlvec nil (first conds))
                            rest-conds (map (partial make-sqlvec logical-operator) (rest conds)) ;recurse
                            conds (cons first-cond rest-conds)
                            clause-snip-params {:cond conds}
                            clause-snip-params (if prefix (assoc clause-snip-params :prefix prefix)
                                                   clause-snip-params)]
                        (build-sql/clause-snip clause-snip-params)))) ;make clause snip
                  (let [cond-snip-params (cond->cond-snip-params (assoc params :cond conds))
                        cond-snip-params (if prefix (assoc cond-snip-params :prefix prefix)
                                             cond-snip-params)]
                    (swap! found-cols concat (:found-cols cond-snip-params))
                    (build-sql/cond-snip cond-snip-params))))] ;make cond snip
        (let [result (build-sql/where-snip {:clause (make-sqlvec nil conds)})]
          (if cols
            result
            (vec @found-cols)))))))

;; (conds->sqlvec {:table :t1
;;                 :table-name "t"
;;                 :alias-prefix "prefix-"
;;                 :props {}
;;                 :cols [:a]
;;                 :conds [:a := 1]})
;; => ["where `prefix-t`.`a` = ?" 1]

;; (conds->sqlvec {:table :t :table-name "t" :alias-prefix "" :props {:g 1} :cols [:a :b :c] :conds [:and [[:a := :p/g] [:b := 2]]]})
;; (conds->sqlvec {:table :t :table-name "t" :alias-prefix "" :props {:g 1} :cols [:a :b :c] :conds [:b := 2]})
;; => ["where  t.b = ?" 2]
;; => ["where ( t.a = ? AND t.b = ?)" 1 2]
;; => ["where (t.a = ? AND\nt.b = ?)" 1 2]



(defn make-where-clause [{:keys [table table-name alias-prefix scope cond where props cols] :as params}]
  (when (or scope cond where)
    (let [conds (filterv some? [scope cond where])]
                                        ;If there's only one cond in the and, the and will be removed by conds->sqlvec
      (conds->sqlvec (assoc params :conds [:and (filterv some? [scope cond where])])
                     ;; table alias-prefix props cols [:and conds]
                     ))))

;; (make-where-clause {:table-name "t1a"

;;                     :cols [:a] :where [:and  [[:a := 2][:a := 1]]]})
;; => ["where (t1a.a = ? AND t1a.a = ?)" 2 1]
;; (let [where [:and [[:id := 1] [:or [[:group-id := :cu/group-id] [:group-id :in :cu/subgroup-ids]]]]]]
;;   (make-where-clause {:table-name :table-name, :derive-cols? true, :where where :props {:group-id 1 :subgroup-ids [1 2]}}))

(defn make-order-by-clause [{:keys [table table-name alias-prefix order-by cols] :as params}]
  (when (and order-by (pos? (count order-by)))
    (build-sql/order-by-snip params)))

(defn positive-number-or-nil? [n]
  (or (nil? n) (cu/parse-natural-number n)))

(defn make-limit-clause [{:keys [count offset] :as limit} limit-max]
  (if-not (and (positive-number-or-nil? count) (positive-number-or-nil? offset))
    (throw (ex-info "Either count or offset is not a positive number" limit)))
  (when-let [count (or count limit-max)]
    (build-sql/limit-snip {:count count :offset offset})))

;; (make-limit-clause {:count 1} 10)
;; (make-order-by-clause {:order-by [[:id :desc] ["bla" :desc]]})
