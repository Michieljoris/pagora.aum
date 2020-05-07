;; TODO-aum: this is a copy from old dc-admin, update from templates brach
;; TODO-aum: hide remember-token in new record in case it gets sent to frontend in error!!
(ns pagora.aum.modules.auth.db-config.user
  #?(:cljs (:require-macros [pagora.aum.database.validate.Rule :refer [rule]]))
  (:require [pagora.aum.database.validate.core :as bv :refer [Rules]]
            [pagora.aum.database.validate.rules :as rule :refer [require-keys]]
            #?(:clj [pagora.aum.database.validate.Rule :refer [rule]])
            [pagora.aum.modules.auth.db-config.util :refer [master-account-admin-scope account-admin-scope
                                                            not-deleted-scope
                                                            validate-master-account-admin-access
                                                            validate-account-admin-access
                                                            account-id-has-to-be-valid
                                                            ]]
            [pagora.aum.database.query :refer [sql]]
            [taoensso.timbre :as timbre :refer [error info]]))

;;TODO-aum: app.config
;; [app.config :refer [app-config]]
(def app-config nil)

;; (dev/get-all-columns-of-table "users")

;; (def schema {:account-id            :int
;;              :encrypted-password  :text
;;              :id                  :int
;;              :gender              :text
;;              :email               :text
;;              :name                :text
;;              :locale              :text
;;              :function            :text
;;              :updated-at          :date-time
;;              :confirmation-token  :text
;;              :remember-token      :text
;;              :password-expires-at :date-time
;;              :phone               :text
;;              :account-admin         {:type :boolean} ;;tinyint(1)
;;              :deleted             {:type :boolean}
;;              :effacts-synced      {:type :boolean}
;;              :guest               {:type :boolean}
;;              :inactive            {:type :boolean}
;;              :created-at          :date-time
;;              :tent-subject-id     :int
;;              :effacts-subject-id  :int
;;              :company-id          :int
;;              :time-zone-id :text
;;              ;; :last-updated-user-id :int
;;              ;; :created-user-id :int
;;              })

(def schema {:id :int
             :name :text
             :email :text
             :active {:type :boolean}
             :locale :text
             :remember-token :text
             :confirmation-token :text
             :encrypted-password :text})

(def config {:root true
             :schema schema
             :columns (keys schema)
             ;; :joins {:dossier-type {:type :has-many  :foreign-key :updated-by-id}
             ;;         }
             ;;Specifying belongs-to for account otherwise aum picks the many-many
             ;;accounts-users join table
             :joins {:account {:type :belongs-to}
                     :admin-for-accounts {:alias-for-table :account :type :many-to-many :join-table :admins}
                     :event {:alias-for-table :event-store
                             :type :has-many :foreign-key :entity-id}}

             ;; :updated-by {:alias-for-table :user :type :belongs-to  :foreign-key :updated-by-id}
             :read {:role {"master--admin" {:blacklist [:encrypted-password :confirmation-token]}
                           "master-account-admin" {:blacklist [:encrypted-password :confirmation-token]
                                               :scope [:and [master-account-admin-scope]]}
                           "account-admin" {:blacklist [:encrypted-password :confirmation-token]
                                          :scope [:and [account-admin-scope]]}}
                    ;;No access for non-admins, so commented out
                    ;; :whitelist [:id :name :email :phone :locale :gender :account-id :password-expires-at]
                    ;; :blacklist [:encrypted-password]
                    ;; :scope [:id := :u/id] ;by default user can only get its own data
                    }
             :create {:role {"master--admin" {:whitelist [:name :email :phone :function :locale :gender :account-id :company-id
                                                        :inactive :deleted :tent-subject-id  :password-expires-at
                                                        :remember-token]}
                             "master-account-admin" {:whitelist [:name :email :phone :locale :function :gender :account-id :company-id
                                                             :inactive :deleted :tent-subject-id  :password-expires-at
                                                             :remember-token]}
                             "account-admin" {:whitelist [:name :email :phone :locale :function :gender :account-id :company-id
                                                        :inactive :deleted :tent-subject-id  :password-expires-at
                                                        :remember-token]}}}
             :update {:role {"master--admin" {:whitelist [:name :email :phone :function :locale :gender :inactive :deleted
                                                        :tent-subject-id :password-expires-at]}
                             "master-account-admin" {:whitelist [:name :email :function :phone :locale :gender :inactive :deleted
                                                             :tent-subject-id :password-expires-at]
                                                 :scope [:and [master-account-admin-scope]]}
                             "account-admin" {:whitelist [:name :email :phone :function :locale :gender :inactive :deleted
                                                        :tent-subject-id :password-expires-at]
                                            :scope [:and [account-admin-scope]]}}}})

(defn company-id-has-to-be-valid [env {:keys [company-id] :as mods}]
  (when company-id
    (let [company (first (sql env :get-cols-from-table {:table :company :cols [:id]
                                                        :where-clause ["where `id` = ?" company-id]}))]
      (rule (some? company)
            "When creating a user, and setting company-id, it has to be set to existing company"
            {:mods mods}))))

(defn validate-user-required-keys [record]
  (rule/require-keys record [:name :email :account-id] {:table :user}))

(defn validate-email [{:keys [email deleted]}]
  (rule
   (re-matches (:email-regex app-config) email)
   ;; (or deleted (re-matches (:email-regex app-config/config) email))
        "Email is not valid"
        {:email email}))

;; (defn validate-delete-user [env id]
;;   (Rules
;;    (rule/validate-not-in-use env :activity-event :user-id id)
;;    (rule/validate-not-in-use env :event-store :user-id id)
;;    (rule/validate-not-in-use env :checklists-users :user-id id)
;;    (rule/validate-not-in-use env :dossiers-users :user-id id)
;;    dashboard_filters
;;    dashboard_landing_pages
;;    documents
;;    locations
;;    notes
;;    people
;;    pictures
;;    remarks
;;    tasks
;;    templates_users
;;    values


;;    (rule/validate-not-in-use env :dossier :user-id id)
;;    (rule/validate-not-in-use env :checklist :user-id id))
;;    (rule/validate-not-in-use env :answer :user-id id)
;;   )

;;Master--admin ==================================================
(defmethod bv/validate ["master--admin" :create :user]
  [_ _ env _ new-record _]
  (Rules
   (account-id-has-to-be-valid env new-record)
   (company-id-has-to-be-valid env new-record)
   (validate-user-required-keys new-record)
   (validate-email new-record)
   ;; (validate-phone new-record)
   ))

(defmethod bv/validate ["master--admin" :update :user]
  [_ _ env record mods modded-record]
  (Rules
   (validate-user-required-keys modded-record)
   (validate-email modded-record)
   ;; (validate-phone new-record)
   ))

(defmethod bv/validate ["master--admin" :bulk-update :user]
  [_ _ env record mods modded-record]
  (Rules
   (rule  (= (keys mods) [:password-expires-at])
          "Only allowed to bulk update password-expires-at"
          {:mods mods})))

;; (defmethod bv/validate ["master--admin" :delete :user]
;;   [_ _ env record mods modded-record]
;;   (validate-delete-user env (:id record)))


;;Master-account-admin ==================================================
(defmethod bv/validate ["master-account-admin" :create :user]
  [_ _ {:keys [user]} _ new-record _]
  (Rules
   (validate-master-account-admin-access user new-record)
   (validate-user-required-keys new-record)
   (validate-email new-record)
   ))

(defmethod bv/validate ["master-account-admin" :update :user]
 [_ _ {:keys [user]} record mods modded-record]
  (Rules
   (validate-master-account-admin-access user modded-record) ;;scope already disallows this
   (validate-user-required-keys modded-record)
   (validate-email modded-record)
   ))

(defmethod bv/validate ["master-account-admin" :bulk-update :user]
  [_ _ env record mods modded-record]
  (Rules
   (rule  (= (keys mods) [:password-expires-at])
          "Only allowed to bulk update password-expires-at"
          {:mods mods})))

;; (defmethod bv/validate ["master-account-admin" :delete :user]
;;   [_ _ env record mods modded-record]
;;   (validate-delete-user env (:id record)))

;;Account-admin ==================================================
(defmethod bv/validate ["account-admin" :create :user]
  [_ _ {:keys [user]} _ new-record _]
  (Rules
   (validate-account-admin-access user new-record)
   (validate-user-required-keys new-record)
   (validate-email new-record)
   ))

(defmethod bv/validate ["account-admin" :update :user]
 [_ _ env record mods modded-record]
  (Rules
   (validate-account-admin-access (:user env) modded-record) ;;scope already disallows this
   (validate-user-required-keys modded-record)
   (validate-email modded-record)
   ))

(defmethod bv/validate ["account-admin" :bulk-update :user]
  [_ _ env record mods modded-record]
  (Rules
   (rule  (= (keys mods) [:password-expires-at])
          "Only allowed to bulk update password-expires-at"
          {:mods mods})))

;; (defmethod bv/validate ["account-admin" :delete :user]
;;   [_ _ env record mods modded-record]
;;   (validate-delete-user env (:id record)))
