(ns pagora.aum.modules.events.db-config
  (:require
   [taoensso.timbre :as timbre]
   [pagora.aum.modules.auth.db-config.util :refer [master-account-admin-scope account-admin-scope]]))

(def config
  {:root true
   :table-name :event_store
   :joins {:event-user {:alias-for-table :user
                        :type :belongs-to :foreign-key :user-id}
           :event-group {:alias-for-table :group
                         :type :belongs-to :foreign-key :group-id}}
   :columns [:user-id :sequence-no :entity-id :group-id :id
             :data-version :name :created-at :data :entity-type
             :invitation-id]
   :read {:role {"super-admin" {:blacklist []}
                 "supergroup-admin" {:blacklist []
                                     :scope (fn [env {:keys [t1 t1-foreign-key] :as join-info}]
                                              ;; (timbre/info :#pp join-info)
                                              (if (= t1-foreign-key :entity-id) ;;scoping is on source table
                                                [:entity-type := (name t1)]
                                                master-account-admin-scope))}
                 "group-admin" {:blacklist []
                                :scope (fn [env {:keys [t1 t1-foreign-key] :as join-info}]
                                         (if (= t1-foreign-key :entity-id) ;;scoping is on source table
                                           [:entity-type := (name t1)]
                                           account-admin-scope))}}}})
