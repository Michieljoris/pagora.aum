(ns pagora.aum.modules.auth.subscriptions.init
  (:require
   [clojure.set :as set]
   [clojure.data.json :as json]
   [pagora.aum.parser.core :as aum-parser]
   [clj-time.core :as t]
   [clj-time.coerce :as c]
   [clj-time.format :as f]
   [clojure.java.jdbc  :as jdbc]
   [taoensso.timbre :as timbre :refer [error info]]
   [pagora.aum.database.query :refer [sql]]
   [pagora.aum.modules.auth.subscriptions.core :as sub]
   [pagora.aum.modules.auth.subscriptions.time :as time]
   )
  )


;; [database.config :as db-config]
(def db-config nil)
;; [parser.core :refer [parser]]
(def parser nil)
;; [database.connection :refer [db-conn]]
(def db-conn nil)
   ;; [app.config :refer [config]]

(defn make-table-config [table {:keys [blacklist table-name]}]
  {:root true
   :table-name table-name
   :columns (-> db-config table :columns)
   :read {:role {"super-admin" {:blacklist (or blacklist [])}}}})

(def table-configs {:user {:blacklist [:encrypted-password :confirmation-token :remember-token]}})

(defn make-db-config [table-configs]
  (into {} (map (fn [[k v]]
                  [k (make-table-config k v)])
                table-configs)))


(defn write-subscriptions
  "Write batch of events to event-store table"
  [env subscriptions {:keys [validate?]}]
  (when (seq subscriptions)
    (timbre/info :#c "Writing batch of " (count subscriptions) " subscriptions")

    (let [cols (keys (first subscriptions))]
      (sql env :insert-records {:table :subscription
                                :skip-validate? (not validate?)
                                :cols cols
                                :records subscriptions}))))

(defn get-enabled-user-count [env]
  (let [result (parser env [(list {:user [:id]}
                                  {:with-meta :count
                                   :where [:or [[:deleted := 0]
                                                [:deleted :is :null]]]
                                   :order-by [[:id :desc]]
                                   :limit {:count 0}})])
        table-count (get-in result [:user :meta :count])]
    (if table-count table-count
        (throw (ex-info "table-count returned error??" {:result result})))))


(defn remove-nil-values [m]
  (into {} (remove (fn [[k v]]
                     (nil? v))
                   m)))


(defn make-a-batch-of-subscriptions [env {:keys [offset batch-size]}]
  (let [batch (parser env [(list {:user [:id :group-id {:subscription [:id]}]}
                                 {:order-by [[:id :asc]]
                                  :where [:or [[:deleted := 0]
                                               [:deleted :is :null]]]
                                  :limit {:count batch-size :offset offset}})])
        users (->> (get batch :user)
                   (remove #(or (nil? (:group-id %))
                                (-> %
                                    :subscription
                                    seq))))]
    (mapv #(sub/make-user-subscription % (time/now))
          users)))

(defn make-initial-subscriptions [env {:keys [batch-size]}]
  (let [enabled-user-count (get-enabled-user-count env)
        result {:enabled-user-count enabled-user-count}]
    (loop [initial-subscriptions [] offset 0]
      (if (< offset enabled-user-count)
        (recur (let [batch-of-subscriptions (make-a-batch-of-subscriptions
                                             env {:batch-size batch-size
                                                  :offset offset})]
                 (into initial-subscriptions batch-of-subscriptions))
               (+ offset batch-size))
        (assoc result :initial-subscriptions initial-subscriptions)))))

(defn write-initial-subscriptions [env {:keys [batch-size simulate? validate?]}]
  (let [result {:simulate? simulate?}]
    (try
      (jdbc/with-db-transaction [tx (:db-conn env)]
        (when simulate?
          (timbre/info :#b "Simulating..")
          (jdbc/db-set-rollback-only! tx))

        (timbre/info :#c "Making subscriptions for users")
        (let [env (assoc env :db-conn tx)
              {:keys [initial-subscriptions enabled-user-count]}
              (make-initial-subscriptions env {:batch-size batch-size})
              result (assoc result :enabled-user-count enabled-user-count)

              _ (timbre/info :#c "Writing subscriptions")
              partition (partition 300 300 nil initial-subscriptions) ;What a weird API
              computed (pmap (fn [subscriptions]
                               (write-subscriptions env subscriptions {:validate? validate?}))
                             partition)]
          (assoc result :subscriptions-written (apply + computed)
                 :validate? validate?)))
      (catch Exception e
        (timbre/error e)
        (assoc result
               :error-msg (str "No subscriptions written." " Exception thrown:" (.toString e)))))))

(defn delete-subscriptions [env]
  (timbre/info :#c "Deleting all subscriptions")
  (sql env :delete-record {:table :subscription
                           :skip-validate? true
                           :where [:id :> 0]}))

(comment
  (let [batch-size 100
        db-config (make-db-config table-configs)
        db-config (assoc db-config
                         :event-store (:event-store db-config/db-config)
                         :subscription (:subscription db-config/db-config))
        env (aum-parser/parser-env {:parser-config {:print-exceptions true
                                                      :limit-max batch-size
                                                      :event-store-disabled true
                                                      :normalize false
                                                      :sql-log false}
                                      :user {:id 1990 :some-user "afoobar" :role "super-admin" :group-id 10}
                                      :schema-validating :strict
                                      :parser parser
                                      :db-conn       db-conn
                                      :db-config     db-config
                                      :sql {:hugsql-ns "database.queries"
                                            ;; :datetime-return-type :joda
                                            }})]



    (delete-subscriptions env)
    (let [result (write-initial-subscriptions env {:batch-size 100 :simulate? false
                                                   :validate? false})]
      (timbre/info :#pp result)

      )
    ;; (timbre/info :#pp (get-enabled-user-count env))
    ;; (timbre/info :#pp (make-a-batch-of-subscriptions env {:offset 0 :batch-size 10}))
    ;; (let [{:keys [enabled-user-count new-subscriptions]}
    ;;       (make-initial-subscriptions env {:batch-size batch-size})]
    ;;   (timbre/info enabled-user-count (count new-subscriptions)))

    ;; (let [subscriptions [{:user-id 1 :entry-at "2021-01-01"}
    ;;                      ;; {:user-id 1992 :entry-at "2021-01-01"}
    ;;                      ]]
    ;;   (sql env :insert-records {:table :subscription
    ;;                             :cols (keys (first subscriptions))
    ;;                             :records subscriptions}))
    ;; (let [result (parser env [(list {:user [:id {:subscription [:id :entry-at]}]}
    ;;                                 {:order-by [[:id :asc]]
    ;;                                  :where [:or [[:deleted := 0]
    ;;                                               [:deleted :is :null]]]
    ;;                                  :limit {:count 10 ;; :offset offset
    ;;                                          }})])]

    ;;   (timbre/info :#pp result)
    ;; ;;   )


    ;; ;; (let [result (parser env [(list {:subscription [:id :entry-at :expired-at :deleted :user-id
    ;; ;;                                                 :invalidated :updated-at :created-at]}
    ;; ;;                                 {:order-by [[:id :asc]]
    ;; ;;                                  :limit {:count batch-size ;; :offset offset
    ;; ;;                                          }})])]

    ;; ;;    (timbre/info :#pp result)
    ;; ;;   )


    )
  )
