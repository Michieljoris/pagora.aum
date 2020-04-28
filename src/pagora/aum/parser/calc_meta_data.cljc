(ns pagora.aum.parser.calc-meta-data
  (:require
   [pagora.aum.database.clauses :as clauses]
   [pagora.aum.database.inspect :as db-inspect]
   [pagora.aum.database.query :refer [sql]]
   [taoensso.timbre :as timbre :refer [spy error info warn]])
  )


;; (ns-unmap *ns* 'calc-meta-data)
(defmulti calc-meta-data (fn [& args]))
;; not used

(defmethod calc-meta-data :count
  [env rows {:keys [sql-fn sql-fn-args return-empty-vector? calculation-params]}]

  ;; (timbre/info :#pp rows)
  ;; (timbre/info sql-fn)
  ;; (timbre/info :#pp sql-fn-args)

  (if return-empty-vector?
    0
    (let [make-cond-scope-clause (fn []
                                   (let [{:keys [t2 scope cond alias-prefix]} sql-fn-args]
                                     (clauses/make-where-clause {:table t2
                                                                 :table-name (db-inspect/table-name env t2)
                                                                 :scope scope
                                                                 :cond cond
                                                                 :alias-prefix alias-prefix
                                                                 :where nil
                                                                 :derive-cols? true
                                                                 })))
          sql-fn-args (cond-> sql-fn-args
                        (:ignore-where? calculation-params)
                        (assoc :where-clause (make-cond-scope-clause)))]
      (case sql-fn
        :get-cols-from-table (sql env :count sql-fn-args)
        :get-joined-rows (sql env sql-fn (assoc sql-fn-args :count? true))
        :not-implemented))))

(defmethod calc-meta-data :count-by-join
  [env rows {:keys [sql-fn sql-fn-args return-empty-vector? calculation-params]}]

  ;; (timbre/info :#pp rows)
  ;; (timbre/info sql-fn)
  ;; (timbre/info :#pp sql-fn-args)

  (if return-empty-vector?
    0
    (let [make-cond-scope-clause (fn []
                                   (let [{:keys [t2 scope cond alias-prefix]} sql-fn-args]
                                     (clauses/make-where-clause {:table t2
                                                                 :table-name (db-inspect/table-name env t2)
                                                                 :scope scope
                                                                 :cond cond
                                                                 :alias-prefix alias-prefix
                                                                 :where nil
                                                                 :derive-cols? true
                                                                 })))
          sql-fn-args (cond-> sql-fn-args
                        (:ignore-where? calculation-params)
                        (assoc :where-clause (make-cond-scope-clause)))]
      (case sql-fn
        :get-cols-from-table (sql env :count-by-join sql-fn-args)
        :not-implemented))))



(defmethod calc-meta-data :as-rows
  [env rows {:keys [sql-fn sql-fn-args return-empty-vector? calculation-params]}]
  true)
