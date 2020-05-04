(ns pagora.aum.modules.events.init
  (:require
   [app.config :refer [config]]
   [cuerdas.core :as str]
   [clojure.set :as set]
   [bilby.database.jdbc-defaults :as jdbc-defaults]
   [clojure.data.json :as json]
   [bilby.parser :as bilby-parser]
   [clj-time.core :as t]
   [clj-time.coerce :as c]
   [clj-time.format :as f]
   [clojure.java.jdbc  :as jdbc]
   [parser.core :refer [parser]]
   [database.config :as db-config]
   [database.connection :refer [db-conn]]
   [taoensso.timbre :as timbre :refer [error info]]
   [bilby.database.query :refer [sql]]
   [event-store.create :as event-store-create]
   [clojure.set :as set]))


(defn make-table-config [table {:keys [blacklist table-name]}]
  {:root true
   :table-name table-name
   :columns (-> db-config/db-config table :columns)
   :read {:role {"super-admin" {:blacklist (or blacklist [])}}}})

(def table-configs {:user {:blacklist [:encrypted-password :confirmation-token :remember-token]}
                    :dossier-type {}
                    :group {}
                    :field {}
                    :job-offer {}
                    :translation {}
                    :company {:table-name :companies }
                    :admin {}
                    :scheduled-alert {:table-name :alerts-scheduled}})

(defn make-db-config [table-configs]
  (into {} (map (fn [[k v]]
                  [k (make-table-config k v)])
                table-configs)))

(defn write-events
  "Write batch of events to event-store table"
  [env events]
  (when (seq events)
    (let [cols (keys (first events))]
      (sql env :insert-records {:table :event-store
                                :skip-validate? true
                                :cols cols
                                :records events}))))

(defn get-table-count [env table]
  (let [result (parser env [(list {table ['*]}
                                  {:with-meta :count
                                   :order-by [[:id :desc]]
                                   :limit {:count 0}})])
        table-count (get-in result [table :meta :count])]
    (if table-count table-count
        (throw (ex-info "table-count returned error??" {:result result})))))


(defn remove-nil-values [m]
  (into {} (remove (fn [[k v]]
                     (nil? v))
                   m)))

(defn make-fake-create-event
  "Returns a generic record-created, record-updated or record-deleted
  event map."
  [env table record]
  (let [event-name (str (name table) "-" "created")
        ;;NOTE: this is the convention currently:
        ;; created-at (c/from-sql-time (:updated-at record))
        ;; created-at (:updated-at record)

        ;;NOTE: this would make more sense:
        ;; created-at (t/now)
        ;; created-at (f/unparse (f/formatters :mysql) created-at)

         created-at (:created-at record)
        ]

    (assoc
     (event-store-create/make-event env event-name (name table) (:id record) (remove-nil-values record))
     :user-id nil ;;TODO: ???? should be user-id of user thast created event
     :group-id nil ;;TODO: ???? should be group-id of user that created event
     :sequence-no 1
     :created-at created-at)))

(defn make-a-batch-of-events [env {:keys [table offset batch-size sample-event]}]
  (let [batch (parser env [(list {table ['*]}
                                 {:order-by [[:id :asc]]
                                  :limit {:count batch-size :offset offset}})])
        records (get batch table)
        record-ids (mapv :id records)
        existing-events (sql env :get-cols-from-table {:table :event-store
                                                       :cols [:entity-id]
                                                       :where [:and [[:entity-type := (:entity-type sample-event)]
                                                                     [:entity-id :in record-ids]
                                                                     [:name := (:name sample-event )]]]})
        existing-events-record-ids (mapv :entity-id existing-events)
        missing-records (remove #(contains? (set existing-events-record-ids) (:id %)) records)]
    (mapv #(make-fake-create-event env table %)
          missing-records)))

(defn get-new-events-for-table [env {:keys [table batch-size result]}]
  (let [sample-event (make-fake-create-event env table nil)
        table-count (get-table-count env table)
        result (assoc result :table-count table-count)]
    (loop [new-events [] offset 0]
      (if (< offset table-count)
        (recur (let [batch-of-events (make-a-batch-of-events env {:table table
                                                                  :batch-size batch-size
                                                                  :offset offset
                                                                  :sample-event sample-event})]
                 (into new-events batch-of-events))
               (+ offset batch-size))
        (assoc result :new-events new-events)))))

(defn delete-admin-events [env]
  (timbre/info :#b "Deleting all admin events")
  (doseq [table (keys table-configs)]
    (sql env :delete-record {:table :event-store
                             :skip-validate? true
                             :where [:entity_type := (name table)]})))

(defn write-fake-create-events-for-table [env {:keys [table batch-size result simulate?]}]
  (let [result (assoc result :table table :simulate? simulate?)]
    (try
      (jdbc/with-db-transaction [tx (:db-conn env)]
        (when simulate?
          (timbre/info :#b "Simulating..")
          (jdbc/db-set-rollback-only! tx))

        (timbre/info :#b "Making fake create events for table" table)
        (let [env (assoc env :db-conn tx)
              {:keys [new-events] :as result} (get-new-events-for-table env {:table table :batch-size batch-size :result result})
              result (dissoc result :new-events)

              _ (timbre/info :#b "Writing events")
              partition (partition 300 300 nil new-events) ;What a weird API
              computed (pmap (fn [events]
                               (write-events env events))
                             partition)]
          (assoc result :events-written (apply + computed))))
      (catch Exception e
        (timbre/error e)
        (assoc result
               :error-msg (str "No events written for " table " Exception thrown:" (.toString e)))))))

(defn init-admin-events [env batch-size {:keys [simulate?]}]
  (loop [result {} tables (keys table-configs)]
    (if-let [table (first tables)]
      (recur (assoc result table
                    (write-fake-create-events-for-table env {:table table :batch-size batch-size :result nil
                                                             :simulate? simulate?}))
             (rest tables))
      result)))

(comment
  (let [batch-size 100
        db-config (make-db-config table-configs)
        db-config (assoc db-config :event-store (:event-store db-config/db-config))
        env (bilby-parser/parser-env {:parser-config {:print-exceptions true
                                                      :limit-max batch-size
                                                      :normalize false
                                                      :sql-log false}
                                      :user {:id 1990 :some-user "afoobar" :role "super-admin" :group-id 10}
                                      :schema-validating :strict
                                      :parser parser
                                      :db-conn       db-conn
                                      :db-config     db-config
                                      :sql {:hugsql-ns "database.queries"
                                            :datetime-return-type :sql}})]


    ;; (timbre/info :#pp (sql env :set-global-time-zone {}))

    ;; (timbre/info :#pp (sql env :set-session-time-zone {}))

    ;; (timbre/info :#pp (sql env :session-time-zone {}))
    (delete-admin-events env)
    (let [ result (init-admin-events env 100 {:simulate? false})]
      (timbre/info :#pp result))
    ))
