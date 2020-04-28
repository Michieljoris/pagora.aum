(ns pagora.aum.database.schema
  (:require
   [pagora.clj-utils.core :as cu]
   [taoensso.timbre :as timbre]
   ;; #?(:cljs [backend.database.connection :refer [make-db-connection]])
   #?(:cljs [goog.object :as gobj])
   #?(:clj [clojure.java.jdbc :as jdbc])))

(defn- column-map
  [{:keys [table_name]} columns]
  (let [table-columns (filter #(= table_name (:table_name %))
                              columns)]
    (zipmap (map :column_name table-columns) table-columns)))

(defn- tables-with-columns
  [{:keys [tables columns]}]
  (for [table tables]
    (assoc table :columns (column-map table columns))))

(defn- add-primary-keys
  [{:keys [primary-keys]} tables]
  (for [table tables]
    (assoc table :primary-key (some #(if (= (:table_name table) (:table_name %))
                                       %)
                                    primary-keys))))

(defn denormalized-schema
  "Build a denormalized schema map with table names as first level keys."
  [schema]
  (let [tables (->> (tables-with-columns schema)
                    (add-primary-keys schema))]
    (reduce (fn [m table]
              (assoc m (keyword (:table_name table)) table))
            {} tables)))

#?(:clj
   (defn get-tables
     [dbm schema-name]
     (jdbc/result-set-seq (.getTables dbm nil schema-name nil (into-array String ["TABLE"]))))
   :cljs
   (defn get-tables [alasql-db]
     (js->clj (gobj/get alasql-db "tables"))))

#?(:clj
   (defn get-columns
     [dbm schema-name]
     (jdbc/result-set-seq (.getColumns dbm nil schema-name nil nil)))
   :cljs
   (defn get-columns [alasql-db table]
     (js->clj (gobj/get alasql-db "tables" table "columns"))))

#?(:clj
   (defn get-primary-keys-of-table
     [dbm schema-name table]
     (jdbc/result-set-seq (.getPrimaryKeys dbm nil schema-name table))))

#?(:clj
   (defn get-primary-keys [dbm schema-name tables]
     (map #(first (get-primary-keys-of-table dbm schema-name %)) (map :table_name tables))))

#?(:clj
   (defn get-schema
     "Get a schema from a database."
     ([db-conn] (get-schema db-conn "public"))
     ([db-conn schema-name]
      ;; There's a few more ways to get at schema data, like SHOW TABLES and
      ;; DESCRIBE TABLE, and from the INFORMATION_SCHEMA database: the TABLES and
      ;; COLUMNS tables, they need multiple requests. The metadata comes with the
      ;; connection. Might be a slight performance benifit when testing, since we
      ;; build specific databases with different schemas for the tests. Much of a
      ;; muchness though.
      (with-open [conn (jdbc/get-connection db-conn)]
        (let [dbm (.getMetaData conn)
              tables (get-tables dbm schema-name)]
          (denormalized-schema {:tables       tables
                                :columns      (get-columns dbm schema-name)
                                :primary-keys (get-primary-keys dbm schema-name tables)})))))
   :cljs
   (defn get-schema
     ([db-conn] (get-schema db-conn nil))
     ([{:keys [alasql db-name]} _]
      (let [alasql-db (aget alasql "databases" db-name)
            tables (get-tables alasql-db)]
        (reduce (fn [m [table-name table-info]]
                  (let [columns (js->clj (aget table-info "columns"))
                        columns (reduce (fn [m column]
                                          (assoc m (get column "columnid")
                                                 column))
                                        {} columns)]
                    (assoc m (keyword table-name)
                           {:table_cat (gobj/get alasql-db "databaseid")
                            :table_name table-name
                            :columns columns
                            }))
                  )
                {} tables)))))


;; #?(:cljs
;;    (let [test-db-conn (make-db-connection {:db-name "aum_test" :print-spec true})]
;;      (timbre/info "SCHEMA:")
;;      (js/console.log ">>" (get-tables test-db-conn))))





;; TODO: return schema where strings are keywords, and plural db names are
;; singular, hyphens instead of underscores are used. At the moment this
;; conversion happens in database.inspect.

;; get-schema produces something like:
;; {:users {:table_name "users",
;;          :table_cat "chinchilla_development",
;;          :columns {"id" {:table_cat "chinchilla_development",
;;                          :table_name "checklist_invitation_histories"
;;                          <various column info>},
;;                    "email" {:table_cat "chinchilla_development",
;;                             :table_name "checklist_invitation_histories"
;;                             <various column info}},
;;          :primary-key {:table_cat "chinchilla_development",
;;                        :table_name "checklist_invitation_histories",
;;                        :column_name "id",
;;                        :key_seq 1,
;;                        :pk_name "PRIMARY"}
;;          <various other table data>}
;;  :dossiers {....}
;;  etc}

;; Perhaps it should be:
;; {:users {:table-name "users"
;;          :columns [:id :email]
;;          :primary-key :id}
;;  :table-foo {..}
;;  :etc}
;; But database.inspect will have to be adjusted.

;; (require
;;  ;; '[digicheck.database.connection :refer [make-db-connection]]
;;  '[database.connection :refer [db-conn]])

;; (def schema (get-schema db-conn))

(do
  (defn underscored-string->hyphened-kw [s]
    ((comp keyword cu/underscore->hyphen) s))

  (defn make-condensed-schema [schema]
    (into {}
          (map (fn [[_ {:keys [:table_name :columns :primary-key]}]]
                 [table_name {:primary-key (underscored-string->hyphened-kw (:column_name primary-key))
                              :columns (mapv underscored-string->hyphened-kw (keys columns))
                              }])
               schema)))

  ;; (make-condensed-schema schema)
  )
