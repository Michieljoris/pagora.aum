;; TODO-aum: this is a copy from old dc-admin, update from templates brach
;; TODO-aum: hide remember-token in new record in case it gets sent to frontend in error!!
(ns pagora.aum.modules.auth.db-config.user
  #?(:cljs (:require-macros [bilby.database.validate.Rule :refer [rule]]))
  (:require [bilby.database.validate.core :as bv :refer [Rules]]
            [bilby.database.validate.rules :as rule :refer [require-keys]]
            #?(:clj [bilby.database.validate.Rule :refer [rule]])
            [database.table.util :refer [supergroup-admin-scope group-admin-scope
                                         not-deleted-scope
                                         validate-supergroup-admin-access
                                         validate-group-admin-access
                                         group-id-has-to-be-valid
                                         ]]
            [app.config :as app-config]
            [bilby.database.query :refer [sql]]
            [digicheck.common.util :as du]
            [clojure.pprint :refer [pprint]]
            [taoensso.timbre :as timbre :refer [error info]]
            [cuerdas.core :as str]))

;; (dev/get-all-columns-of-table "users")

(def schema {:group-id            :int
             :encrypted-password  :text
             :id                  :int
             :gender              :text
             :email               :text
             :name                :text
             :locale              :text
             :function            :text
             :updated-at          :date-time
             :confirmation-token  :text
             :remember-token      :text
             :password-expires-at :date-time
             :phone               :text
             :group-admin         {:type :boolean} ;;tinyint(1)
             :deleted             {:type :boolean}
             :effacts-synced      {:type :boolean}
             :guest               {:type :boolean}
             :inactive            {:type :boolean}
             :created-at          :date-time
             :tent-subject-id     :int
             :effacts-subject-id  :int
             :company-id          :int
             :time-zone-id :text
             ;; :last-updated-user-id :int
             ;; :created-user-id :int
             })

(def config {:root true
             :schema schema
             :columns (keys schema)
             ;; :joins {:dossier-type {:type :has-many  :foreign-key :updated-by-id}
             ;;         }
             ;;Specifying belongs-to for group otherwise bilby picks the many-many
             ;;groups-users join table
             :joins {:group {:type :belongs-to}
                     :admin-for-groups {:alias-for-table :group :type :many-to-many :join-table :admins}
                     :event {:alias-for-table :event-store
                             :type :has-many :foreign-key :entity-id}}

             ;; :updated-by {:alias-for-table :user :type :belongs-to  :foreign-key :updated-by-id}
             :read {:role {"super-admin" {:blacklist [:encrypted-password :confirmation-token]}
                           "supergroup-admin" {:blacklist [:encrypted-password :confirmation-token]
                                               :scope [:and [supergroup-admin-scope]]}
                           "group-admin" {:blacklist [:encrypted-password :confirmation-token]
                                          :scope [:and [group-admin-scope]]}}
                    ;;No access for non-admins, so commented out
                    ;; :whitelist [:id :name :email :phone :locale :gender :group-id :password-expires-at]
                    ;; :blacklist [:encrypted-password]
                    ;; :scope [:id := :u/id] ;by default user can only get its own data
                    }
             :create {:role {"super-admin" {:whitelist [:name :email :phone :function :locale :gender :group-id :company-id
                                                        :inactive :deleted :tent-subject-id  :password-expires-at
                                                        :remember-token]}
                             "supergroup-admin" {:whitelist [:name :email :phone :locale :function :gender :group-id :company-id
                                                             :inactive :deleted :tent-subject-id  :password-expires-at
                                                             :remember-token]}
                             "group-admin" {:whitelist [:name :email :phone :locale :function :gender :group-id :company-id
                                                        :inactive :deleted :tent-subject-id  :password-expires-at
                                                        :remember-token]}}}
             :update {:role {"super-admin" {:whitelist [:name :email :phone :function :locale :gender :inactive :deleted
                                                        :tent-subject-id :password-expires-at]}
                             "supergroup-admin" {:whitelist [:name :email :function :phone :locale :gender :inactive :deleted
                                                             :tent-subject-id :password-expires-at]
                                                 :scope [:and [supergroup-admin-scope]]}
                             "group-admin" {:whitelist [:name :email :phone :function :locale :gender :inactive :deleted
                                                        :tent-subject-id :password-expires-at]
                                            :scope [:and [group-admin-scope]]}}}})

(defn company-id-has-to-be-valid [env {:keys [company-id] :as mods}]
  (when company-id
    (let [company (first (sql env :get-cols-from-table {:table :company :cols [:id]
                                                        :where-clause ["where `id` = ?" company-id]}))]
      (rule (some? company)
            "When creating a user, and setting company-id, it has to be set to existing company"
            {:mods mods}))))

(defn validate-user-required-keys [record]
  (rule/require-keys record [:name :email :group-id] {:table :user}))

(defn validate-email [{:keys [email deleted]}]
  (rule
   (re-matches (:email-regex app-config/config) email)
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

;;Super-admin ==================================================
(defmethod bv/validate ["super-admin" :create :user]
  [_ _ env _ new-record _]
  (Rules
   (group-id-has-to-be-valid env new-record)
   (company-id-has-to-be-valid env new-record)
   (validate-user-required-keys new-record)
   (validate-email new-record)
   ;; (validate-phone new-record)
   ))

(defmethod bv/validate ["super-admin" :update :user]
  [_ _ env record mods modded-record]
  (Rules
   (validate-user-required-keys modded-record)
   (validate-email modded-record)
   ;; (validate-phone new-record)
   ))

(defmethod bv/validate ["super-admin" :bulk-update :user]
  [_ _ env record mods modded-record]
  (Rules
   (rule  (= (keys mods) [:password-expires-at])
          "Only allowed to bulk update password-expires-at"
          {:mods mods})))

;; (defmethod bv/validate ["super-admin" :delete :user]
;;   [_ _ env record mods modded-record]
;;   (validate-delete-user env (:id record)))


;;Supergroup-admin ==================================================
(defmethod bv/validate ["supergroup-admin" :create :user]
  [_ _ {:keys [user]} _ new-record _]
  (Rules
   (validate-supergroup-admin-access user new-record)
   (validate-user-required-keys new-record)
   (validate-email new-record)
   ))

(defmethod bv/validate ["supergroup-admin" :update :user]
 [_ _ {:keys [user]} record mods modded-record]
  (Rules
   (validate-supergroup-admin-access user modded-record) ;;scope already disallows this
   (validate-user-required-keys modded-record)
   (validate-email modded-record)
   ))

(defmethod bv/validate ["supergroup-admin" :bulk-update :user]
  [_ _ env record mods modded-record]
  (Rules
   (rule  (= (keys mods) [:password-expires-at])
          "Only allowed to bulk update password-expires-at"
          {:mods mods})))

;; (defmethod bv/validate ["supergroup-admin" :delete :user]
;;   [_ _ env record mods modded-record]
;;   (validate-delete-user env (:id record)))

;;Group-admin ==================================================
(defmethod bv/validate ["group-admin" :create :user]
  [_ _ {:keys [user]} _ new-record _]
  (Rules
   (validate-group-admin-access user new-record)
   (validate-user-required-keys new-record)
   (validate-email new-record)
   ))

(defmethod bv/validate ["group-admin" :update :user]
 [_ _ env record mods modded-record]
  (Rules
   (validate-group-admin-access (:user env) modded-record) ;;scope already disallows this
   (validate-user-required-keys modded-record)
   (validate-email modded-record)
   ))

(defmethod bv/validate ["group-admin" :bulk-update :user]
  [_ _ env record mods modded-record]
  (Rules
   (rule  (= (keys mods) [:password-expires-at])
          "Only allowed to bulk update password-expires-at"
          {:mods mods})))

;; (defmethod bv/validate ["group-admin" :delete :user]
;;   [_ _ env record mods modded-record]
;;   (validate-delete-user env (:id record)))
