(ns pagora.aum.modules.auth.subscriptions.core
  (:require
   [taoensso.timbre :as timbre :refer [error info warn]]
   [bilby.database.query :refer [sql]]
   [bilby.database.clauses :as db-clauses]
   [clj-time.core :as t]
   [clj-time.local :as l]
   [clj-time.format :as f]
   [clj-time.coerce :as c]
   [subscriptions.time :as time]
   ))


;;CHIN

;;TODO: set updated-at, created-at columns to not nullable in chin migration!!!
;;TODO: add time-zone-id column to user table

;;BILBY

;;DONE: sort out joda/inst/sql/string of dates
;;DONE: adjust return value of result-set-read-column-* in query for timezone
;; If timezone of computer where db is run is not set to UTC we get timezone
;; adjusted datetimes back. But we expect to send UTC datetimes to the frontend.
;; So we adjust it. Still to be done is other than the timestamps. Plus decided
;; timezone offset earlier and store it. Also, if timezone is utc just return value.
;;To set timezone:
;;sudo dpkg-reconfigure tzdata
;;sudo systemctl restart mariadb
;;restart bilby

;;DONE: sort out throwing of exceptions in hooks. Hooks do their own sql query
;;potentially, so again they're wrapped in a transaction

;;DONE  when updating record, get record before validating, unless passed in already.

;;DONE: when mutating a record, skip it if record will not be modified!!! And so,
;;also don't record an event for it!!!!

;;DONE: get subscriptions in user form


;;DONE: replace all cals for t/now with time/now and make the fn dynamic so we can spoof it in tests


;;DONE: find out how to filter users on being enabled at any point during a
;;particular period

;;DONE user+subscription virtual table


;;DONE: add name of month to downloaded csv
;;DONE: revert button icon is gone!!!!

;;TODO: verify: group-id is added to sub, users without group-id don't get a sub!!!!
;;TODO: don't use virtual table if not needed
;;TODO: pass in today for validation!!!
;;TODO: write test suite for subscriptions
;;TODO: chin: add time-zone-id to user migration
;;TODO: chin: add group-id to sub migration

;;LATER:
;;TODO later: auto renew subscription
;; =>>>> have worker check every day, and if a subscription is set to autorenew,
;; and next month's subscription is about to happen, extend subscription to include
;; following month
;;TODO later: sign up online, pay with cc/stripe/etc

(defn get-valid-subscription
  "Returns the first subscription that's valid at any time between start and end
  inclusive. To check whether there's a valid subscripton on a particular date
  pass in date instead. Set end to nil to extend the range into infinity.
  Subscription validations should make sure subscriptions don't overlap so the
  first is also always the only one found."
  [env {:keys [user-id date start end]}]
  (when (number? user-id)
    (let [start (.toString (if date date start))
          end (if date date end)
          end (.toString (if end end "9999-12-12"))
          where-clause (db-clauses/make-where-clause
                        {:table-name "subscriptions"
                         :derive-cols? true
                         :where [:and [[:user-id := user-id]
                                       [:or
                                        [[:deleted := 0]
                                         [:deleted :is :null]]]
                                       [:and [[:entry-at :<= end]
                                              [:or [[:expired-at :is :null]
                                                    [:expired-at :>= start]]]]]]]})
          subscriptions (sql env :get-cols-from-table {:table :subscription
                                                       :cols [:id :user-id :entry-at :expired-at
                                                              :invalidated :deleted]
                                                       :where-clause where-clause})]
      (first subscriptions))))

(defn make-user-subscription
  ([user entry-at]
   (make-user-subscription user entry-at nil))
  ([{:keys [id group-id]} entry-at expired-at]
   {:group-id group-id :user-id id :entry-at entry-at :expired-at expired-at}))
;; (make-user-subscription {:id 1 :group-id 2} 100)
;; => {:group-id 2, :user-id 1, :entry-at 100, :expired-at nil}

(defn update-subscription-from-user-deleted
  "*Updates a user's subscription correctly based on whether the user is
  deleted (disabled) or not"
  [env {{:keys [time-zone-id] :as user
         :or {time-zone-id "Europe/Amsterdam"}}  :user
        {:keys [deleted]} :mods}]

  (let [enabled (not deleted)
        ;;NOTE: There's an edge case where today might be different once validation happens. Maybe
        ;;pass today into the validation?
        today (.toString (time/today-in-tz time-zone-id))
        subscription (get-valid-subscription env {:user-id(:id user)
                                                  :date today})]
    (timbre/info :#pp {:sub subscription})

    (cond
      enabled (cond
                (and subscription (:invalidated subscription))
                (sql env :update-record {:table :subscription
                                         :current-record subscription
                                         :id (:id subscription)
                                         :mods {:invalidated false
                                                :expired-at nil}})
                (nil? subscription)
                (let [new-subscription (make-user-subscription user today)]
                  (sql env :insert-record {:table :subscription
                                           :mods new-subscription})))

      (and (not enabled)
           subscription
           (or
            (not (:invalidated subscription))
            (nil? (:expired-at subscription))
            (not (= today (.toString (time/sql-date-to-local-date (:expired-at subscription)))))))
      (sql env :update-record {:table :subscription
                               :id (:id subscription)
                               :current-record subscription
                               :mods {:invalidated true
                                      :expired-at today}}))))
