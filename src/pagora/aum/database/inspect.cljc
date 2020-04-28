(ns pagora.aum.database.inspect
  (:require
   [clojure.set :as set]
   [pagora.clj-utils.core :as cu]
   [taoensso.timbre :as timbre]))


;>>>> data store inspection

;For repl-ing
;; #?(:clj
;;    (ns-unmap *ns* 'table-name))
;; #?(:clj
;;    (ns-unmap *ns* 'table?))

;; ========== table-name
(defmulti table-name
  (fn [{:keys [db-config]} table]
    (get-in db-config [table :backend])))

(defn table-name-sql
  "Table names are singular hyphenated and keywords. This fn returns the actual table name
  by looking it up in db-config or otherwise just adds an 's'. Returns a string or nil."
  [{:keys [db-config]} table]
  (when (some? table)
    (let [table (keyword table)]
      (-> (or (get-in db-config [table :table-name]) (str (name table) "s"))
          name
          cu/hyphen->underscore))))

(defmethod table-name :default ;;sql
  [env table]
  (table-name-sql env table))

(defmethod table-name :sql
  [env table]
  (table-name-sql env table))

(defmethod table-name :es
  [_ table]
  (keyword table))

;; ========== list-columns
(defn ^:dynamic list-columns-sql
  "Returns list of hyphened keywords, table name is first converted
  into actual table name. Expects schema in env, throws otherwise"
  [{:keys [schema] :as env} table]
  {:pre [(some? (:schema env))]}
  (get-in schema [(table-name env table) :columns]))

(defmulti list-columns
  (fn [env table]
    (get-in env [:db-config table :backend])))

(defmethod list-columns :default ;;sql
  [env table]
  (list-columns-sql env table))

(defmethod list-columns :sql
  [env table]
  (list-columns-sql env table))

(defmethod list-columns :es
  [env table]
  ;;TODO
  )

(defn list-tables-sql
  "Returns list of tables as hyphened keywords, but not singular.
  Expects schema in env, throws otherwise"
  [{:keys [schema] :as env}]
  {:pre [(some? (:schema env))]}
  (map (comp keyword cu/underscore->hyphen)
       (keys schema)))

;; ========== table?
(defmulti table?
  (fn [{:keys [db-config] :as env} table]
    (get-in db-config [table :backend])))

(defn table?-sql [env t]
  (when (and (keyword? t) (not (namespace t)))
    (let [t (table-name-sql env t)]
      (cu/includes? (list-tables-sql env)
                   (-> t cu/underscore->hyphen keyword)))))

(defmethod table? :default ;;sql
  [env t]
  (table?-sql env t))

(defmethod table? :sql
  [env t]
  (table?-sql env t))

(defmethod table? :es ;;elasticsearch
  [env t]
  (when (keyword? t)
    ;;TODO-ES
    true
    ))
;;<<< data store inspection

;; TODO: memoize
(defn no-timestamp?
  "Return true if table has updated-at and created-at columns"
  [env table]
  (let [cols (list-columns env table)]
    (empty? (set/intersection #{:updated-at :created-at} (set cols)))))


(defn reverse-join [join-type]
  {:pre [(or (cu/includes? [:has-many :belongs-to :many-to-many nil] join-type)
             (throw (ex-info (str "reverse-join received an illegal join type: " join-type) {})))]
   :post [(or (cu/includes? [:has-many :belongs-to :many-to-many nil] %)
              (throw (ex-info (str "reverse-join returned an illegal join type: " %) {})))]}
  (condp = join-type
    :belongs-to :has-many
    :has-many :belongs-to
    join-type))

(defn type-or-keyword [map-or-keyword]
  (if (map? map-or-keyword) (:type map-or-keyword) map-or-keyword))

(defn table->foreign-key [t]
  (-> t name (str "-id") keyword))

(defn get-join-type-from-db-config
  "Returns join type from t1 to t2 as set in db-config, otherwise nil"
  [t1 t2 join-t1-t2 join-t2-t1]
  (let [join-type-t1-t2 (type-or-keyword join-t1-t2)
        join-type-t2-t1-reversed (reverse-join (type-or-keyword join-t2-t1))
        join-type (or join-type-t1-t2 join-type-t2-t1-reversed)]
    (when (and join-type-t1-t2
               join-type-t2-t1-reversed
               (not= t1 t2) ;join onto itself is ok
             (not= join-type-t1-t2 join-type-t2-t1-reversed))
      (throw (ex-info (str "Join as defined in db-config is not correct for " t1 " and " t2
                           " namely " join-type-t1-t2 " and " (type-or-keyword join-t2-t1))
                      {})))
    join-type))

(defn find-join-table
  ([env t1 t2] (find-join-table env t1 t2 {:as-database-name true}))
  ([env t1 t2 {:keys [as-database-name]}]
   (let [t1-table-name (cu/underscore->hyphen (table-name env t1))
         t2-table-name (cu/underscore->hyphen (table-name env t2))
         join-table (keyword (str t1-table-name "-" t2-table-name))
         join-table-reversed (keyword (str t2-table-name "-" t1-table-name))]
     (cond
       ;;TODO-ES
       ;;should pass in backend key and dispatch list-tables on it
       (cu/includes? (list-tables-sql env) join-table)
       (if as-database-name
         join-table
         (keyword (str (name t1) "-" (name t2))))
       (cu/includes? (list-tables-sql env) join-table-reversed)
       (if as-database-name
         join-table-reversed
         (keyword (str (name t2) "-" (name t1))))))))

(defn decide-foreign-key [t-info]
  (or (:foreign-key t-info)
      ;; (and (:derive-join-type-from-schema config) (table->foreign-key (:table t-info)))))
      (table->foreign-key (:table t-info))))

(defn schema-many-to-many?
  [{:keys [schema raw-schema] :as env} join-table t1-foreign-key t2-foreign-key]
  ;;TODO-ES
  ;;should pass in backend key and dispatch list-tables on it
  (let [join-table-exists? (cu/includes? (list-tables-sql env) join-table)
        columns-join-table (when join-table
                             (get-in schema [(cu/hyphen->underscore (name join-table)) :columns]))
        join-table-has-t1-foreign-key? (cu/includes? columns-join-table  t1-foreign-key)
        join-table-has-t2-foreign-key? (cu/includes? columns-join-table  t2-foreign-key)]
    (and join-table-exists? join-table-has-t1-foreign-key? join-table-has-t2-foreign-key?)))

(defn many-to-many [{:keys [parser-config] :as env}
                    {:keys [t1-info t2-info] :as info}]
  (let [t1-join-table (:join-table t1-info)
        t2-join-table (:join-table t2-info)
        join-table-defined (or t1-join-table t2-join-table)
        join-table-derived (and (:derive-join-type-from-schema parser-config)
                                (find-join-table env (:table t1-info) (:table t2-info)))
        ;; _ (info t1-info)
        ;; _ (info t2-info)
        t1-foreign-key (decide-foreign-key t1-info)
        t2-foreign-key (decide-foreign-key t2-info)
        join-table (or join-table-defined join-table-derived)
        {:keys [defined-join-type]} info
        many-to-many? (and (or (= :many-to-many defined-join-type) join-table)
                           (or (nil? defined-join-type) (= :many-to-many defined-join-type))
                           (schema-many-to-many? env join-table t1-foreign-key t2-foreign-key))]
    (and t1-join-table t2-join-table (not= t1-join-table t2-join-table)
         (throw (ex-info (str (:table t1-info) " and " (:table t2-info)
                              " have different join tables defined for their join")
                         {:info info})))
    (and many-to-many? {:join-type :many-to-many :join-table (cu/hyphen->underscore (name join-table))
                        :join-table-kw (keyword (str (name (:table t1-info)) "-" (name (:table t2-info))))
                        :t1-foreign-key t1-foreign-key :t2-foreign-key t2-foreign-key})))

(defn has-rel? [{:keys [parser-config] :as env}
                {:keys [t1-info t2-info defined-join-type] :as info}
                rel]
  (let [table1 (:table (if (= rel :has-many) t1-info t2-info))
        table2 (:table (if (= rel :has-many) t2-info t1-info))

        defined-foreign-key (:foreign-key t1-info)
        reverse-defined-foreign-key (:foreign-key t2-info)
        foreign-key (or defined-foreign-key reverse-defined-foreign-key
                        (table->foreign-key table1))
        foreign-key-exists? (cu/includes? (list-columns env table2) foreign-key)
        _     (and (= defined-join-type rel)
                   (not foreign-key-exists?)
                   (throw (ex-info (str "Relationship of " defined-join-type " has been defined but " table2
                                        " has not the foreign-key for " table1 " namely " foreign-key) info)))
        has-rel? (and foreign-key-exists?
                      (or (= rel defined-join-type)
                          (and (:derive-join-type-from-schema parser-config)
                               (nil? defined-join-type))))]

    (when has-rel?
      (and
       ;;If the foreign keys are defined for this join in both tables..
       defined-foreign-key reverse-defined-foreign-key
       ;;then if this is a join to a table that has a join back to t1 then we allow
       ;;the foreign key for that join as defined in t2 to be different.
       (not (:self-join-table (if (= rel :has-many) t1-info t2-info)))
       ;;otherwise we require them to be the same!!!!
       (not= defined-foreign-key reverse-defined-foreign-key)
       (throw (ex-info (str "Foreign key defined for " (name rel) " relationship between "
                            (:table t1-info) " and " (:table t2-info) " does not match."
                            "If " table2  " has more than one " (name rel) " relationship to " table1  " you should set "
                            ":self-join-table to true for the " table2  " join in the configuration for " table1 ) info)))
      {:join-type rel :t1-foreign-key foreign-key :t2-foreign-key foreign-key})))

(defn _get-join-info
  "Get join type from db-config otherwise deduce from schema. Both t1
  and t2 are expected to be existing tables in the database. :type key of
  returned map is one of :has-many, :belongs-to, :many-to-many,
  if type is many-to-many :table key contains an existing join table
  name. If no join is defined or found nil is returned"
  [{:keys [db-config] :as env} t1 t2]
  {:post [(cu/includes? [:has-many :belongs-to :many-to-many nil] (:type %))]}
  (let [t1-joins (get-in db-config [t1 :joins])
        t2-joins (get-in db-config [t2 :joins])
        t1-t2-join (get t1-joins t2)
        t2-t1-join (get t2-joins t1)
        t2 (or (:alias-for-table t1-t2-join) t2)
        defined-join-type (get-join-type-from-db-config t1 t2 t1-t2-join t2-t1-join)
        info {:t1-info {:foreign-key (or (:t1-foreign-key t1-t2-join) (:foreign-key t1-t2-join))
                        :join-table (:join-table t1-t2-join)
                        :self-join-table (:self-join-table t1-t2-join)
                        :table t1}
              :t2-info {:foreign-key (or (:t2-foreign-key t2-t1-join) (:foreign-key t2-t1-join))
                        :join-table (:join-table t2-t1-join)
                        :self-join-table (:self-join-table t2-t1-join)
                        :table t2}
              :defined-join-type defined-join-type}
        ]
    (if-not (table? env t1)
      (throw (ex-info (str "Table " t1 " doesn't exist") {:table t1})))
    (if-not (table? env t2)
      (throw (ex-info (str "Table " t2 " doesn't exist") {:table t2})))
    (let [info (or (many-to-many env info)
                   (has-rel? env info :has-many)
                   (has-rel? env info :belongs-to)
                   )]
      (if (:alias-for-table t1-t2-join)
        (assoc info :table t2)
        info))))

;; (def get-join-info (memoize _get-join-info))
(def get-join-info _get-join-info)
