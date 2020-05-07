;; TODO-aum: this is a copy from old dc-admin, update from templates brach
(ns pagora.aum.modules.auth.db-config.account
  #?(:cljs (:require-macros [pagora.aum.database.validate.Rule :refer [rule]]))
  (:require
   [pagora.aum.database.validate.core :as bv :refer [Rules]]
   #?(:clj [pagora.aum.database.validate.Rule :refer [rule]])
   [pagora.aum.database.validate.rules :as rule :refer [require-keys]]
   ;; [pagora.aum.database.query :refer [sql]]
   [pagora.clj-utils.core :as cu]
   [taoensso.timbre :as timbre :refer [error info]]
))

;;(dev/get-all-columns-of-table "accounts")

(def master-account-admin-scope
  [:or [[:id := :u/account-id]
        [:account-id := :u/account-id]]])

(def account-admin-scope
  [:id := :u/account-id])

(def config
  {:root true
   :columns [:id :name :email :account-id]
   ;; :columns [:user-id :password-validity-period-active :account-id :id :password-validity-period-days :name
   ;;           :master-account :updated-at :effacts-synced :sharing-enabled :pdf-options :has-dossiers :created-at
   ;;           :effacts-subject-id :password-validity-period-retention
   ;;           :app-logo-file-name :app-logo-content-type :app-logo-file-size :app-logo-updated-at
   ;;           :app-brand-file-name :app-brand-content-type :app-brand-file-size :app-brand-updated-at
   ;;           :app-brand-text
   ;;           :pdf-logo-file-name :pdf-logo-content-type :pdf-logo-file-size :pdf-logo-updated-at
   ;;           :show-logo-on-pdf
   ;;           :account-cost-yearly :account-cost-monthly :extra-account-cost
   ;;           :password-complexity-requirements-active]
   ;; :joins {:dossier :has-many
   ;;         :account :belongs-to
   ;;         :event {:alias-for-table :event-store
   ;;                 :type :has-many :foreign-key :entity-id}
   ;;         :subaccount {:alias-for-table :account :type :has-many :foreign-key :account-id}}
   :read {:role {"master-admin" {:blacklist []}
                 "master-account-admin" {:blacklist []
                                         :scope master-account-admin-scope}
                 "account-admin" {:blacklist [:master-account]
                                  :scope [:id := :u/account-id]}}}
   :create {:role {"master-admin" {:blacklist [:id :updated-at :created-at]}
                   "master-account-admin" {:blacklist [:id :updated-at :created-at :master-account
                                                       :account-cost-monthly :account-cost-yearly :extra-account-cost
                                                       :effacts-synced :effacts-subject-id]}}}
   :update {:role {"master-admin" {:blacklist [:id :updated-at :created-at]}
                   "master-account-admin" {:blacklist [:id :updated-at :created-at :master-account :account-id
                                                   :account-cost-monthly :account-cost-yearly :extra-account-cost
                                                   :effacts-synced :effacts-subject-id]
                                       :scope master-account-admin-scope}
                   "account-admin" {:blacklist [:id :updated-at :created-at :account-id :master-account
                                              :account-cost-monthly :account-cost-yearly :extra-account-cost
                                              :has-dossiers :effacts-synced :effacts-subject-id
                                              ;;Perhaps allow:
                                              :sharing-enabled :name
                                              :password-complexity-requirements-active]
                                  :scope account-admin-scope}}}
   :delete {:role {"master-admin" {}
                   ;;can only delete its own subaccounts
                   "master-account-admin" {:scope [:account-id := :u/account-id]}
                   }}
   })

(defn validate-delete-account [env id]
  (Rules
   (rule/validate-not-in-use env :account :account-id id)
   (rule/validate-not-in-use env :user :account-id id)
   ;; (rule/validate-not-in-use env :template :account-id id)
   ;; (rule/validate-not-in-use env :category :account-id id)
   (rule/validate-not-in-use env :dossier-type :account-id id)

   ;;These can only be created when above exists, but just checking to be sure
   (rule/validate-not-in-use env :dossier :account-id id)
   (rule/validate-not-in-use env :checklist :account-id id)
   (rule/validate-not-in-use env :person :account-id id)
   (rule/validate-not-in-use env :webhook :account-id id))
  )

(defn validate-master-account-admin-access [user record]
  (rule
   (let [{:keys [account-id] :as user} (if (fn? user) (user) user)
         record (if (fn? record) (record) record)]
     (or (= account-id (:id record))
         (= account-id (:account-id record))))
        "Can't access accounts other than own account or subaccounts"
        (let [{:keys [account-id subaccount-ids] :as user} (if (fn? user) (user) user)]
          {:record record
           :user-account-id account-id
           :user-subaccount-ids subaccount-ids})))

(defn validate-account-admin-access [user record]
  (rule
   (let [user (if (fn? user) (user) user)
         record (if (fn? record) (record) record)]
     (= (:account-id user) (:id record)))
   "Can't access accounts other than own account"
   {:record record
    :user-account-id (:account-id user)}))

(defn validate-unique-name [{:keys [user] :as env} old-name new-name]
  ;;TODO: Remove if we really don't care if account name is not unique.
  ;; (when (and new-name
  ;;            (or (nil? old-name)
  ;;                (not= old-name new-name)))
  ;;   (let [account-with-same-name (first (sql env :get-cols-from-table {:table :account :cols [:name]
  ;;                                                                    :where-clause ["where `name` = ?" new-name]}))]

  ;;     (rule (nil? account-with-same-name)
  ;;           "Account name is not unique"
  ;;           {:name new-name
  ;;            :error {:type :validation
  ;;                    :table :account
  ;;                    :column :name
  ;;                    :error :not-unique}})))
  )

;;master-admin ==================================================
(defmethod bv/validate ["master-admin" :create :account]
  [_ _ env _ _ {:keys [master-account account-id name] :as new-record}]
  (Rules
   (rule/require-keys new-record [:name] {:table :account})
   (validate-unique-name env nil name)
   (rule (not (and master-account account-id))
         "A account cannot be both master-account and subaccount"
         {:account-id account-id
          :master-account master-account})
   ))

(defmethod bv/validate ["master-admin" :update :account]
  [_ _ env record mods {:keys [master-account account-id] :as modded-record}]
  (Rules
   (rule (not (and master-account account-id))
         "A account cannot be both master-account and subaccount"
         {:id (:id record)
          :account-id account-id
          :master-account master-account})
   (rule/require-keys modded-record [:name] {:table :account})
   (validate-unique-name env (:name record) (:name mods))))

(defmethod bv/validate ["master-admin" :delete :account]
  [_ _ env record mods modded-record]
  (validate-delete-account env (:id record)))

;;Master-Account-admin ==================================================
(defmethod bv/validate ["master-account-admin" :create :account]
  [_ _ {:keys [user] :as env} _ mods {:keys [id name] :as modded-record}]
  (Rules
   (rule/require-keys modded-record [:name] {:table :account})
   (validate-unique-name env nil name)
   (rule (= (:account-id modded-record) (:account-id user))
         "Account-id of new account has to be account-id of user"
         {:user-account-id (:account-id user)
          :account-id (:account-id modded-record)
          :mods mods})
   ))

(defmethod bv/validate ["master-account-admin" :update :account]
  [_ _ {:keys [user] :as env} record mods {:keys [id name] :as modded-record}]
  (Rules
   (validate-master-account-admin-access user modded-record)
   (rule/require-keys modded-record [:name] {:table :account})
   (validate-unique-name env (:name record) (:name mods))))

(defmethod bv/validate ["master-account-admin" :delete :account]
  [_ _ env record mods modded-record]
  (validate-delete-account env (:id record)))

;; Account-admin ==================================================
(defmethod bv/validate ["account-admin" :update :account]
  [_ _ {:keys [user] :as env} record mods modded-record]
  (Rules
   ;;scope already disallows this
   (rule (= (:account-id user) (:id record))
         "Not allowed to update an inaccessible account"
         {:user-account-id (:account-id user)
          :updated-account-id (:id record)
          :mods mods})
   (rule/require-keys modded-record [:name] {:table :account})
   (validate-unique-name env (:name record) (:name mods))))


;;Files ========================================

(defn validate-file-format [{:keys [file-path id format]}]
  (rule (= format "original")
        "format should be original"
        {:format format :file-path file-path}))

(defn validate-file-type-for-account [{:keys [file-path type]}]
  (rule (contains? #{:app-logo :app-brand :pdf-logo} type)
        "Can only retrieve/upload app-logo, app-brand or pdf-logo for account"
        {:type type
         :file-path file-path}))

(defn validate-file-upload-for-account
  [env {:keys [file-path type id file-name] :as file-info}]
  (Rules
   (rule (and (number? (cu/parse-natural-number id))
              (not (empty? file-name)))
         "table id should be set properly and file-name should be set"
         {:id id :file-path file-path :file-name file-name})
   (validate-file-type-for-account file-info)))

(defn validate-master-account-access-for-file [{:keys [user] :as env} {:keys [file-path id] :as file-info}]
  (let [id (cu/parse-natural-number id)]
    (rule (or (= (:account-id user) id)
              (cu/includes? (:subaccount-ids user) id))
          "account id or one of subaccount ids of user has to be same as id of the account associated with the file"
          {:file-path file-path :id id :user-account-id (:account-id user)
           :user-subaccount-ids (:subaccount-ids user)})))

(defn validate-account-admin-access-for-files [{:keys [user] :as env} {:keys [id file-path] :as file-info}]
  (rule (= id (:account-id user))
        "account id of user has to be same as id of the account associated with the file"
        {:file-path file-path :id id :user-account-id (:account-id user)})
  )

;;UPLOAD
(defmethod bv/validate ["master-admin" :file-upload :account]
  [_ _ env file-info _ _]
  (validate-file-upload-for-account env file-info))

(defmethod bv/validate ["master-account-admin" :file-upload :account]
  [_ _ env file-info _ _]
  (Rules
   (validate-file-upload-for-account env file-info)
   (validate-master-account-access-for-file env file-info))
  )

(defmethod bv/validate ["account-admin" :file-upload :account]
  [_ _ env file-info _ _]
  (validate-file-upload-for-account env file-info)
  (validate-account-admin-access-for-files env file-info)
  )


;; DOWNLOAD
(defmethod bv/validate ["master-admin" :file-download :account]
  [_ _ {:keys [user]} {:keys [file-path file-path-vector table id type format path] :as file-info} _ _]
  (Rules
   (validate-file-type-for-account file-info)
   (validate-file-format file-info)
   ))

(defmethod bv/validate ["master-account-admin" :file-download :account]
  [_ _ {:keys [user] :as env} {:keys [file-path-vector id file-path] :as file-info} _ _]
  (Rules
   (validate-file-type-for-account file-info)
   (validate-file-format file-info)
   (validate-master-account-access-for-file env file-info)
   ))

(defmethod bv/validate ["account-admin" :file-download :account]
  [_ _ {:keys [user] :as env} {:keys [file-path-vector id file-path] :as file-info} _ _]
  (Rules
   (validate-file-type-for-account file-info))
  (validate-file-format file-info)
  (validate-account-admin-access-for-files env file-info))
