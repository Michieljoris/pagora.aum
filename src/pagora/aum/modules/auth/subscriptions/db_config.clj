(ns pagora.aum.modules.auth.subscriptions.db-config
  (:require
   [pagora.aum.database.validate.core :as bv :refer [Rules]]
   [pagora.aum.database.query :refer [sql]]
   [pagora.aum.modules.auth.db-config.util :refer [master-account-admin-scope account-admin-scope
                                                   validate-master-account-admin-access
                                                   not-deleted-scope
                                                   validate-account-admin-access
                                                   account-id-has-to-be-valid]]
   [pagora.aum.modules.auth.subscriptions.core :as subs]
   [pagora.aum.modules.auth.subscriptions.time :as time]
   [clj-time.core :as t]
   [clj-time.format :as f]
   [clj-time.coerce :as c]
   [pagora.aum.database.validate.Rule :refer [rule]]
   [pagora.aum.database.validate.rules :as rule]
   [taoensso.timbre :as timbre :refer [error info]]
))

;;Goal is to have an easy way to authorize users based on a subscription, and to
;;have a straightforward way to calculate active users over a period for billing
;;purposes.

;;Idea there's only one subscription at all times that is valid/current for any
;;user, so no overlap of subscriptions. The validations below ensure this is
;;always the case. I've tried to write down these validations as well as I
;;could, since the whole system hinges on its accuracy. When a subs expires a
;;new one has to be created. This way we'll have an historic record of when a
;;user was subscribed or not anytime from the date of the initialisation of the
;;subscriptions table.

;;Alternatively, we could write any current subscription to the user record
;;itself instead of having it in the subscription table at the highest id for
;;that user. But you'd have to write the lapsed subscription to the subscription
;;table when setting a new one on the user, so you'd need a hook in the backend
;;for that. Also you'd have to add that sub on the user to the subscription
;;table when doing calculations on aggregates.

;;To facilitate gradual introduction of this feature authorization in both aum
;;and chin can keep relying on the deleted flag on a user because initially this
;;subs table can be used as a way to just keep historical records.
;;
;;Whenever a user's deleted flag is flipped a hook in aum can update the sub
;;for that user, preferably atomically. Turning on a user creates a new sub with
;;expiry date set to nil. Turning off sets the last sub's expiry date to today.
;;To prevent superfluous subs from getting created if the user is turned on and
;;off multiple times in a day, the hook can take note of the expiry date of the
;;latest subscription and just update that one to nil instead of creating a new
;;sub every time.
;;
;;So from a user's perspective nothing will change as yet. But we'll have a
;;record of when the user was on and off. In and of it self this has no impact
;;on any other functionality, hence it can be deployed without much risk, and it
;;can be monitored/tested for a bit to see if it works ok

;;Next step would be to add a button to the account's page that calculates a list
;;of active users over a period. The logic for the sql query would be:
;;Find all subs where user-id in (all the account's users' ids) AND
;; (not (or (and (some? expired-at)
;;               (expired-at < start))
;;          (entry-at > end)))

;;With some table juggling it might be possible to even add this filter to the
;;user's page. So filter for all users that were active during the month january
;;of 2019. And then push the button to export this to csv or pdf etc.

;;If all goes well, the next step would be to enable authorization based on the
;;subs table instead of on the deleted flag. In aum.security is a sample fn
;;called validate-subscription.
;;
;;This would then make possible the adding of the feature of manuallly updating
;;of the applicable sub for a user on the users admin page. So then it would
;;become possible to plan assign a start to a sub, and/or and end to it.

;;Instead of charging by the whole month it's also possible to charge by the
;;day. It's a simple matter of adding the number of days a user is 'on', for all
;;the users in a account and comparing that to how much you've invoiced this account so far.

;;A complication is that different users might be in different timezones. The
;;validations below correct for this by taking into account the timezone of a
;;user, but use by default the Amsterdam timezone. But this does mean that a
;;user in Bonaire for example might be unexpectedly shut out of his account at
;;6pm on the last day of the user's sub till we take into account the user's
;;timezone (they're 6 hours behind in Bonaire). There's an extra
;;to-local-date-in-tz fn in app.time.

;;Instead of timezones we might bypass the whole issue by giving everybody a
;;grace day before and after the subscription's official start and end date.

;;Sometimes you want to immediately disable a user's subscription. Setting the
;;end date to today is not enough, since it'll stay valid till 12pm that day. By
;;setting the invalidated flag to true as well for that subscription and having
;;any auth fn take that flag into account as well a user can be 'kicked out' on
;;the spot. This has not effect on calculating active users btw.

;;When a subscription entry-date is in the future it's possible to delete it by
;;setting a deleted flag. Invalidating is not appropriate because it would still
;;be counted in terms of billable days.

;;TODO
;;- DONE script to write current-subscriptions from deleted flags on users
;;- DONE add hook on save-user for when deleted flag is updated that updates subscriptions.
;;- add authorization based on subs table to aum and chinchilla


;;An alternative to relying on the subs table getting updated via a hook on
;;save-user is to enable event store first. Then the subs table (for calculation
;;purposes, perhaps just in memory) could in theory be then recreated from that
;;at any point in time while we are flipping the deleted flag.

;;Let's say on some date later we add subs data to a user,recorded on the user
;;itself. We set this subs data using the deleted flag as it is set or not on
;;that date. This will be all recorded in the event store. We could then update
;;this sub record using the similar validations as below. Authorization becomes
;;rather straightforward then, just pull up the sub data for a user. For
;;calculation purposes we'd have to search for all sub updates for a user in the
;;event store, accounted by entry-date, and use the latest one per entry-date.
;;Again, this relies on validation so that expiry dates are never set in the
;;past for example, amongst other things. This all presupposes a working event
;;store. With some effort you could also introduce this in steps. But it won't
;;be as straightforward to pull out active users data.

;;I have also a feeling that it might be a good idea to leave the eventstore for
;;auditing/historical/debugging purposes, and not use it to
;;reconstruct/calculate/deduce necessarily entities from it in the daily course
;;of business, and instead if we want to keep track of something to explicitly
;;create a data store for it. In this case a subscriptions table.

;; (dev/get-all-columns-of-table "subscriptions")

(def schema {:id :int
             :user-id :int
             :deleted {:type :boolean}
             :entry-at :date ;;grant access from 12am on this date
             :expired-at :date ;;deny access from 12am on this date
             ;; TODO: Or set it on account/user. But we will need a timezone. To decide on access
             ;; offset utc with the timezone.
             :account-id :int
             :invalidated {:type :boolean} ;;expired_at is only a date (without time), you might want to turn off access immediately
             :updated-at :date-time
             :created-at :date-time})

(def config
  {:root true
   :schema schema
   :columns (keys schema)
   :read {:role {"super-admin" {:blacklist []
                                :scope not-deleted-scope}
                 "master-account-admin" {:blacklist []
                                     :scope [:and [not-deleted-scope
                                                   master-account-admin-scope]]}
                 "account-admin" {:blacklist []
                                :scope [:and [not-deleted-scope
                                              account-admin-scope]]}}}
   :create {:role {"super-admin" {:blacklist [:updated-at :created-at :deleted]}
                   "master-account-admin" {:blacklist [:updated-at :created-at :deleted]}
                   "account-admin" {:blacklist [:updated-at :created-at :deleted]}}}
   :update {:role {"super-admin" {:whitelist [:deleted :entry-at :expired-at :invalidated]
                                  :scope not-deleted-scope}
                   "master-account-admin" {:whitelist [:deleted :entry-at :expired-at :invalidated]
                                       :scope [:and [not-deleted-scope
                                                     master-account-admin-scope]]}
                   "account-admin" {:whitelist [:deleted :entry-at :expired-at :invalidated]
                                  :scope [:and [not-deleted-scope
                                                account-admin-scope]]}}}})

(defn get-subscription-user
  "Retrieve the user whose subscription we're trying to create/update"
  [env {:keys [user-id]}]
  (let [where-clause ["where `id` = ?" user-id]
        params {:table :user
                :cols [:id :account-id :time-zone-id]
                :where-clause where-clause}
        result (sql env :get-cols-from-table params)]
    (first result)))

;;TODO: make sure subs don't overlap!!!!

(defn validate-new [env {:keys [time-zone-id]
                         :or {time-zone-id "Europe/Amsterdam"}
                         user-id :id
                         :as subscription-user}
                    {:keys [entry-at expired-at invalidated deleted] :as new-subscription}]

  (let [today-in-tz (time/today-in-tz time-zone-id)
        entry-at (time/sql-date-to-local-date entry-at)
        expired-at (time/sql-date-to-local-date expired-at)]

    (Rules

     ;;We're allowed to create a new subscription, but it still has to adhere to some rules:
     (rule/require-keys new-subscription [:user-id :entry-at] {:table :subscription})

     (rule
      (and (some? entry-at)
           (not= entry-at :invalid-date))
      (str "entry-at it should be a valid date string (such as \"2019-02-03\"),"
           "a java.sql.Date or a joda localDate")
      {:new-subscription new-subscription})

     (rule
      (or (nil? expired-at)
          (not= expired-at :invalid-date))
      (str "expired-at it should be nil, a valid date string (such as \"2019-02-03\")"
           ", a java.sql.Date or a joda localDate")
      {:new-subscription new-subscription})

     (rule (not invalidated)
           "You can't invalidate a new subscription"
           {:new-subscription new-subscription})

     (rule (not deleted)
           "You can't delete a new subscription"
           {:new-subscription new-subscription})

     (let [valid-subscription
           (subs/get-valid-subscription env {:user-id user-id
                                             :start entry-at :end expired-at})]
       (rule
        (nil? valid-subscription)
        "There's an existing subscription that overlaps this new subscription."
        {:subscription valid-subscription
         :new-subscription new-subscription}))

     (rule (or (nil? expired-at)
               (not (t/before? expired-at entry-at)))
           "expired-at should be equal or after entry-at"
           ;;(unless we want people to be billed for when they where NOT subscribed :->)
           {:entry-at entry-at :today-in-tz today-in-tz})

     (rule (not (t/before? entry-at today-in-tz))
           "Not allowed to set entry-at in the past"
           ;;(unless we want people to be billed for when they where NOT subscribed :->)
           {:entry-at entry-at :today-in-tz today-in-tz}))))


(defn validate-update [env {:keys [time-zone-id]
                            user-id :id
                            :or {time-zone-id "Europe/Amsterdam"}
                            :as subscription-user}
                       subscription mods modded-subscription]

  ;; (timbre/info :#pp {:current-sub subscription
  ;;                    :mods mods
  ;;                    :modded-sub modded-subscription})

  (let [today-in-tz (time/today-in-tz time-zone-id)
        {:keys [entry-at expired-at invalidated]} subscription
        entry-at (time/sql-date-to-local-date entry-at)
        expired-at (time/sql-date-to-local-date expired-at)
        invalidated (boolean invalidated)

        {updated-entry-at :entry-at
         updated-expired-at :expired-at
         updated-invalidated :invalidated} modded-subscription
        updated-entry-at (time/sql-date-to-local-date updated-entry-at)
        updated-expired-at (time/sql-date-to-local-date updated-expired-at)
        updated-invalidated (boolean updated-invalidated)]

    (Rules
     ;;entry-at should be a valid date and not nil
     (rule
      (and (some? updated-entry-at)
           (not= updated-entry-at :invalid-date))
      (str
       "entry-at it should be a valid date string (such as \"2019-02-03\")"
       " or a java.sql.Date or a joda localDate")
      {:updates mods})

     ;;expired-at should be nil or a date
     (rule
      (or
       (nil? updated-expired-at)
       (not= updated-expired-at :invalid-date))
      (str "Expired-at should be nil or a valid date string (such as \"2019-02-03\")"
           " or a java.sql.Date or a joda localDate")
      {:updates mods})

     ;;The updated subscription should not overlap with any existing subscription
     (let [valid-subscription
           (subs/get-valid-subscription env {:user-id user-id
                                             :start updated-entry-at
                                             :end updated-expired-at})]
       (rule
        (or (nil? valid-subscription)
            (= (:id valid-subscription)
               (:id subscription)))
        "There's an existing subscription that overlaps this updated subscription."
        {:existing-subscription valid-subscription
         :updated-subscription modded-subscription}))

     (rule
      (or (nil? expired-at)
          (time/equal-or-after? expired-at today-in-tz))
      "You can't update a subscription that is expired"
      {:subscription subscription})

     ;;One of the following two sets of rules will logically apply:
     ;;1.Subscription is current
     (when (time/equal-or-before? entry-at today-in-tz)

       ;;Check the four updateable props: entry-at, expired-at, deleted and invalidated
       (Rules
        (rule (not (contains? mods :deleted))
              "You can't update deleted since the subscription is not in the future!!!"
              {:mods mods})
        (rule (not (contains? mods :entry-at))
              "You can't update entry-at since the subscription is not in the future!!!"
              {:mods mods})

        ;;We're trying to set expired-at to a valid date:
        (when (and (contains? mods :expired-at)
                   (some? updated-expired-at))
          (Rules
           (rule (time/equal-or-after? updated-expired-at today-in-tz) ;;so also >= entry-at!!
                 "expired-at should be set equal to or to a date after today!!"
                 {:subscription subscription :mods mods})

           (when (not= invalidated updated-invalidated) ;;NOTE: may be too strict
             ;;We can invalidate a subscription, but only if expired-at is set
             ;;to today. Otherwise we mess with our record keeping, a user would
             ;;be paying for an invalidated subscription. Undoing an
             ;;invalidation also only makes sense to do when subscription
             ;;expires today.
             (rule (t/equal? updated-expired-at today-in-tz)
                   "When invalidated is modified expired-at should be set to today!!"
                   {:subscription subscription
                    :mods mods}))))))

     ;;2. Subscription is scheduled for in the future
     (when (t/after? entry-at today-in-tz)

       ;;Check the four updateable props: entry-at, expired-at, deleted and invalidated
       (Rules
        (when (contains? mods :entry-at)
          (Rules
           (rule (time/equal-or-after? updated-entry-at today-in-tz)
                 "You can not set entry at to a date in the past"
                 {:subscription subscription :mods mods})

           (when (some? updated-expired-at)
             (rule (time/equal-or-before? updated-entry-at updated-expired-at)
                   "You can not set entry-at to a date after expired-at"
                   {:subscription subscription :mods mods}))))

        (rule (not (contains? mods :invalidated))
              "You can't update invalidated for a subscription that's in the future"
              {:subscription subscription :mods mods})

        (when (and (contains? mods :expired-at)
                   (some? updated-expired-at))
          (rule (time/equal-or-after? updated-expired-at updated-entry-at)
                "You can not set expired-at to a date before entry-at"
                {:subscription subscription :mods mods})))

       ;;We don't care about the deleted prop getting updated.
       ))))

;;SUPER-ADMIN
(defmethod bv/validate ["super-admin" :create :subscription]
  [_ _ env record new-record modded-record]
  (let [subscription-user (get-subscription-user env new-record)]
    (Rules
     (rule (some? (:id subscription-user))
           "This new subscription is for a non-existing user"
           {:new-subscription new-record})
     (validate-new env subscription-user new-record))))

(defmethod bv/validate ["super-admin" :update :subscription]
  [_ _ env record mods modded-record]
  (let [subscription-user (get-subscription-user env modded-record)]
    (Rules
     (validate-update env subscription-user record mods modded-record))))


;;MASTER-ACCOUNT-ADMIN
(defmethod bv/validate ["master-account-admin" :create :subscription]
  [_ _ {:keys [user] :as env} record new-record modded-record]
  (let [subscription-user (get-subscription-user env new-record)]
    (Rules
     (validate-master-account-admin-access user subscription-user)
     (validate-new env subscription-user new-record))))

(defmethod bv/validate ["master-account-admin" :update :subscription]
  [_ _ {:keys [user] :as env} record mods modded-record]
  (let [subscription-user (get-subscription-user env modded-record)]
    (Rules
     (validate-master-account-admin-access user subscription-user)
     (validate-update env subscription-user record mods modded-record))))


;;ACCOUNT-ADMIN
(defmethod bv/validate ["account-admin" :create :subscription]
  [_ _{:keys [user] :as env} record new-record modded-record]
  (let [subscription-user (get-subscription-user env new-record)]
    (Rules
     (validate-account-admin-access user subscription-user)
     (validate-new env subscription-user new-record))))

(defmethod bv/validate ["account-admin" :update :subscription]
  [_ _ {:keys [user] :as env} record mods modded-record]
  (let [subscription-user (get-subscription-user env modded-record)]
    (Rules
     (validate-account-admin-access user subscription-user)
     (validate-update env subscription-user record mods modded-record))))
