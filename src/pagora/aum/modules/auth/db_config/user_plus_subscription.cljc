(ns pagora.aum.modules.auth.db-config.user-plus-subscription
  (:require
   ;; [database.table.user :as user]
   ;; [database.table.subscription :as subscription]
   [taoensso.timbre :as timbre]
   ))

(def config
  {:root true
   :backend :virtual
   ;; :joins {:subscription {:type :has-many :foreign-key :user-id}}

   :tables [:user :subscription] ;;blacklist's are combined from these tables
   :read {:role {"super-admin" {}
                 "supergroup-admin" {:scope [:or [[:user.group-id := :u/group-id]
                                                  [:user.group-id :in :u/subgroup-ids]]]}
                 "group-admin" {:scope [:user.group-id := :u/group-id]}}}})
