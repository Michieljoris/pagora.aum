(ns pagora.aum.database.process-params
  (:require [pagora.aum.database.inspect :as db-inspect]
            [pagora.aum.security :as security]
            [pagora.clj-utils.core :as cu]
            [pagora.aum.database.clauses :as clauses]
            [pagora.aum.database.query :as q]
            [cuerdas.core :as str]
            [taoensso.timbre :as timbre :refer [info]]
            #?@(:clj [[clj-time.core :as t]
                      [clj-time.local :as l]
                      [clj-time.format :as f]
                      [clj-time.coerce :as c]])))

#?(:clj
   (defn utc-now-mysql-str []
     (f/unparse
      (f/formatter :mysql)
      (t/now)))
   :cljs
   (defn utc-now-mysql-str []
     (.format (js/moment) "YYYY-MM-DD hh-mm-ss")))

;; (utc-now-mysql-str)
;; => "2019-12-12 04:00:03"

(defn hyphened-keywords->underscored-strings [cols]
  (mapv (comp cu/hyphen->underscore name) cols))

(defmethod q/aum-process-params :get-cols-from-table
  [env _ {:keys [cols table where where-clause user] :as params}]
  (let [table-name (db-inspect/table-name env table)]
    (assoc params
           :where-clause (or where-clause
                             (clauses/make-where-clause {:table-name table-name
                                                         :derive-cols? true
                                                         ;; :no-prefix-table-name no-prefix-table-name
                                                         ;; :scope scope
                                                         :props user
                                                         :where where}))
           :table table-name
           :cols (hyphened-keywords->underscored-strings cols))))

(defmethod q/aum-process-params :get-joined-rows
  [env _ {:keys [cols join-table-cols] :as params}]
  ;; When t1==t2 aliases kick in, see hug.sql
  (let [t1 (:t1 params)
        t2 (:t2 params)
        t1-name (db-inspect/table-name env t1)
        t2-name (db-inspect/table-name env t2)
        t1=t2? (= t1 t2)
        [t1-alias t2-alias] (if t1=t2?
                              [(str "t1_" t1-name) (str "t2_" t2-name)]
                              [t1-name t2-name])
        t1-foreign-key (cu/keyword->underscored-string (:t1-foreign-key params))
        t2-foreign-key (cu/keyword->underscored-string (:t2-foreign-key params))
        cols (hyphened-keywords->underscored-strings cols)
        cols (map #(str (if t1=t2? t2-alias t2-name) "." %) cols)
        ;;This adds a columnn from many-to-many join table to the joined
        ;;row. However this is needed for read.clj to add these rows to the
        ;;right 'joined from' row again.
        cols (conj cols (str (:join-table params) "." t1-foreign-key) )
        join-table (:join-table params)
        join-table-cols (mapv (fn [col] (str join-table "." (cu/hyphen->underscore (name  col)))) join-table-cols)
        cols (into cols join-table-cols)]
    (assoc params
           :t1-name t1-name :t1-alias t1-alias
           :t2-name t2-name :t2-alias t2-alias
           :t1=t2? t1=t2?
           :cols cols
           :t1-foreign-key t1-foreign-key
           :t2-foreign-key t2-foreign-key)))

(defmethod q/aum-process-params :count-belongs-to
  [env _ {:keys [table cond id belongs-to-column] :as params}]
  {:table (db-inspect/table-name env table)
   :belongs-to-column (cu/keyword->underscored-string belongs-to-column)
   :id id
   :cond cond})

(defn process-params-insert-record
  [env {:keys [table mods] :as params}]
  (let [{:keys [keys vals]} (cu/map->keys-and-vals mods)
        cols (hyphened-keywords->underscored-strings keys)
        now (utc-now-mysql-str)]
    (assoc params
           :table-keyword table ;;used in aum validate-sql-fn multimethod
           :table (db-inspect/table-name env table) :cols cols :vals vals
           :created-at now
           :updated-at now
           :no-timestamp? (db-inspect/no-timestamp? env table))))

(defn process-params-insert-records
  [env {:keys [table cols records] :as params}]
  (let [now (utc-now-mysql-str)
        table-cols (db-inspect/list-columns env table)
        set-created-at? (and (not (contains? (set cols) :created-at))
                             (contains? (set table-cols) :created-at))
        set-updated-at? (and (not (contains? (set cols) :updated-at))
                             (contains? (set table-cols) :updated-at))
        cols (cond-> cols
               set-created-at? (conj :created-at)
               set-updated-at? (conj :updated-at))
        vals (mapv (fn [record]

                     (mapv #(get (-> record
                                     (assoc :created-at now)
                                     (assoc :updated-at now))
                                 %) cols))
                   records)
        cols (hyphened-keywords->underscored-strings cols)]
    (assoc params
           :table-keyword table ;;used in bilby validate-sql-fn multimethod
           :table (db-inspect/table-name env table)
           :vals vals :cols cols)))

(defmethod q/aum-process-params :insert-record
  [env _ params]
  (process-params-insert-record env params))

(defmethod q/aum-process-params :insert-records
  [env _ params]
  (process-params-insert-records env params))

(defmethod q/aum-process-params :insert-event
  [env _ params]
  (process-params-insert-record env (assoc params :created-at (utc-now-mysql-str))))

;;For mutations like update, delete and bulk-update
(defn make-where-clause-add-scope [{:keys [user] :as env} {:keys [table table-name method where
                                                                  no-prefix-table-name]}]
  (let [{:keys [scope]} (security/get-permissions env method table user)]
    (clauses/make-where-clause {:table-name table-name
                                :derive-cols? true
                                :no-prefix-table-name no-prefix-table-name
                                :scope scope
                                :props user
                                :where where})))

(defmethod q/aum-process-params :update-record
  [env _ {:keys [table id no-timestamp? mods where current-record] :as params}]
  (let [current-record (or current-record
                           (first (sql env :get-cols-from-table {:table table :cols []
                                                                 :where [:id := id]})))
        to-be-modded (select-keys current-record (keys mods))]
    (if (= to-be-modded mods)
      {:noop 1}
      (let [{:keys [keys vals]} (cu/map->keys-and-vals mods)
            cols (hyphened-keywords->underscored-strings keys)
            table-name (db-inspect/table-name env table)
            where (or where [:id := id])]
        (assoc params
               :where-clause (make-where-clause-add-scope env {:table table
                                                               :table-name table-name
                                                               :method :update
                                                               ;;alasql does not accept table name prefix for
                                                               ;;columns in an update statement
                                                               #?@(:cljs [:no-prefix-table-name true])
                                                               :where where})
               :current-record current-record
               :table-keyword table
               :table table-name
               :cols cols :vals vals
               :updated-at (utc-now-mysql-str)
               :no-timestamp? (or (db-inspect/no-timestamp? env table)
                                  no-timestamp?))))))

(defmethod q/aum-process-params :delete-record
  [env _ {:keys [table id where] :as params}]
  (let [table-name (db-inspect/table-name env table)]
    (assoc params
           :where-clause (make-where-clause-add-scope env {:table table
                                                           :table-name table-name
                                                           :method :delete
                                                           :where (or where [:id := id])})
           :table-keyword table
           :table table-name)))

(defmethod q/aum-process-params :bulk-update
  [env _  {:keys [table where where-cols mods no-timestamp?] :as params}]
  (let [table-name (db-inspect/table-name env table)
        {:keys [keys vals]} (cu/map->keys-and-vals mods)
        cols (hyphened-keywords->underscored-strings keys)]
    (assoc params
           :table-keyword table
           :table table-name
           :updated-at (utc-now-mysql-str)
           :no-timestamp? (or (db-inspect/no-timestamp? env table)
                              no-timestamp?)
           :cols cols :vals vals
           :where-clause (make-where-clause-add-scope env {:table table
                                                           :table-name table-name
                                                           :method :bulk-update
                                                           :where where}))))

(defn rows->cols-and-vals [rows]
  (let [cols (reduce (fn [s row] (into s (keys row)))
                     #{} rows)
        vals (mapv (fn [row]
                     (mapv #(get row %)
                           cols))
                   rows)
        cols (hyphened-keywords->underscored-strings cols)]
    {:cols cols :vals vals}))

;; (rows->cols-and-vals [{:a 1 :b 1} {:c-d 3}])
;; => {:cols ["c_d" "b" "a"], :vals [[nil 1 1] [3 nil nil]]}

(defmethod q/aum-process-params :insert-rows
  [env _  {:keys [table rows] :as params}]
  (let [table-name (db-inspect/table-name env table)
        {:keys [cols vals]} (rows->cols-and-vals rows)]
    (assoc params
           :table-keyword table
           :table-name table-name
           :mods {:rows rows}
           :cols cols :rows vals)))

(defmethod q/aum-process-params :search-translations
  [env _  {:keys [cols] :as params}]
  (assoc params
         :cols (hyphened-keywords->underscored-strings cols)))

(defmethod q/aum-process-params :count
  [env _ {:keys [table] :as params}]
  (let [table-name (db-inspect/table-name env table)
        where-clause (make-where-clause-add-scope env {:table table
                                                       :table-name table-name
                                                       :method :read
                                                       :where (:where (:params params))})]
    (assoc params
           :where-clause where-clause
           :table-keyword table
           :table table-name)))


(defmethod q/aum-process-params :count-by-join
  [env _ {:keys [table where-clause] :as params}]
  (let [table-name (db-inspect/table-name env table)]
    (assoc params
           :t1-foreign-key (cu/keyword->underscored-string (:t1-foreign-key params))
           :where-clause where-clause
           :table-keyword table
           :table table-name)))
