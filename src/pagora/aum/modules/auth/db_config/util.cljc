(ns pagora.aum.modules.auth.db-config.util
  #?(:cljs (:require-macros [pagora.aum.database.validate.Rule :refer [rule]]))
  (:require
   #?(:clj [pagora.aum.database.validate.Rule :refer [rule]])
   [pagora.clj-utils.core :as cu]
   [pagora.aum.database.query :refer [sql]]
   ))

(def master-account-admin-scope
  [:or [[:account-id := :u/account-id]
        [:account-id :in :u/subaccount-ids]]])

(def account-admin-scope
  [:account-id := :u/account-id])

(def not-deleted-scope
  [:or [[:deleted := 0] [:deleted :is :null]]])


(defn validate-master-account-admin-access [user record]
  (rule
   (let [{:keys [account-id subaccount-ids] :as user} (if (fn? user) (user) user)
         record (if (fn? record) (record) record)]
     (or (= account-id (:account-id record))
         (cu/includes? subaccount-ids (:account-id record))))
        "Can't access accounts other than own account or subaccounts"
        (let [{:keys [account-id subaccount-ids] :as user} (if (fn? user) (user) user)]
          {:record record
           :user-account-id account-id
           :user-subaccount-ids subaccount-ids})))

(defn validate-account-admin-access [user record]
  (rule
   (let [user (if (fn? user) (user) user)
         record (if (fn? record) (record) record)]
     (= (:account-id user) (:account-id record)))
   "Can't access accounts other than own account"
   {:record record
    :user-account-id (:account-id user)}))


(defn account-id-has-to-be-valid [env {:keys [account-id] :as mods}]
  (let [account (first (sql env :get-cols-from-table {:table :account :cols [:id]
                                                    :where-clause ["where `id` = ?" account-id]}))]
    (rule (some? account)
          "When creating this record, account-id has to be set to existing account"
          {:mods mods})))
