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

;;(dev/get-all-columns-of-table "groups")

(def supergroup-admin-scope
  [:or [[:id := :u/group-id]
        [:group-id := :u/group-id]]])

(def group-admin-scope
  [:id := :u/group-id])

(def config
  {:root true
   :columns [:user-id :password-validity-period-active :group-id :id :password-validity-period-days :name
             :supergroup :updated-at :effacts-synced :sharing-enabled :pdf-options :has-dossiers :created-at
             :effacts-subject-id :password-validity-period-retention
             :app-logo-file-name :app-logo-content-type :app-logo-file-size :app-logo-updated-at
             :app-brand-file-name :app-brand-content-type :app-brand-file-size :app-brand-updated-at
             :app-brand-text
             :pdf-logo-file-name :pdf-logo-content-type :pdf-logo-file-size :pdf-logo-updated-at
             :show-logo-on-pdf
             :account-cost-yearly :account-cost-monthly :extra-account-cost
             :password-complexity-requirements-active]
   :joins {:dossier :has-many
           :group :belongs-to
           :event {:alias-for-table :event-store
                   :type :has-many :foreign-key :entity-id}
           :subgroup {:alias-for-table :group :type :has-many :foreign-key :group-id}}
   :read {:role {"super-admin" {:blacklist []}
                 "supergroup-admin" {:blacklist []
                                     :scope supergroup-admin-scope}
                 "group-admin" {:blacklist [:supergroup]
                                :scope [:id := :u/group-id]}}}
   :create {:role {"super-admin" {:blacklist [:id :updated-at :created-at]}
                   "supergroup-admin" {:blacklist [:id :updated-at :created-at :supergroup
                                                   :account-cost-monthly :account-cost-yearly :extra-account-cost
                                                   :effacts-synced :effacts-subject-id]}}}
   :update {:role {"super-admin" {:blacklist [:id :updated-at :created-at]}
                   "supergroup-admin" {:blacklist [:id :updated-at :created-at :supergroup :group-id
                                                   :account-cost-monthly :account-cost-yearly :extra-account-cost
                                                   :effacts-synced :effacts-subject-id]
                                       :scope supergroup-admin-scope}
                   "group-admin" {:blacklist [:id :updated-at :created-at :group-id :supergroup
                                              :account-cost-monthly :account-cost-yearly :extra-account-cost
                                              :has-dossiers :effacts-synced :effacts-subject-id
                                              ;;Perhaps allow:
                                              :sharing-enabled :name
                                              :password-complexity-requirements-active]
                                  :scope group-admin-scope}}}
   :delete {:role {"super-admin" {}
                   ;;can only delete its own subgroups
                   "supergroup-admin" {:scope [:group-id := :u/group-id]}
                   }}
   })

(defn validate-delete-group [env id]
  (Rules
   (rule/validate-not-in-use env :group :group-id id)
   (rule/validate-not-in-use env :user :group-id id)
   ;; (rule/validate-not-in-use env :template :group-id id)
   ;; (rule/validate-not-in-use env :category :group-id id)
   (rule/validate-not-in-use env :dossier-type :group-id id)

   ;;These can only be created when above exists, but just checking to be sure
   (rule/validate-not-in-use env :dossier :group-id id)
   (rule/validate-not-in-use env :checklist :group-id id)
   (rule/validate-not-in-use env :person :group-id id)
   (rule/validate-not-in-use env :webhook :group-id id))
  )

(defn validate-supergroup-admin-access [user record]
  (rule
   (let [{:keys [group-id] :as user} (if (fn? user) (user) user)
         record (if (fn? record) (record) record)]
     (or (= group-id (:id record))
         (= group-id (:group-id record))))
        "Can't access groups other than own group or subgroups"
        (let [{:keys [group-id subgroup-ids] :as user} (if (fn? user) (user) user)]
          {:record record
           :user-group-id group-id
           :user-subgroup-ids subgroup-ids})))

(defn validate-group-admin-access [user record]
  (rule
   (let [user (if (fn? user) (user) user)
         record (if (fn? record) (record) record)]
     (= (:group-id user) (:id record)))
   "Can't access groups other than own group"
   {:record record
    :user-group-id (:group-id user)}))

(defn validate-unique-name [{:keys [user] :as env} old-name new-name]
  ;;TODO: Remove if we really don't care if group name is not unique.
  ;; (when (and new-name
  ;;            (or (nil? old-name)
  ;;                (not= old-name new-name)))
  ;;   (let [group-with-same-name (first (sql env :get-cols-from-table {:table :group :cols [:name]
  ;;                                                                    :where-clause ["where `name` = ?" new-name]}))]

  ;;     (rule (nil? group-with-same-name)
  ;;           "Group name is not unique"
  ;;           {:name new-name
  ;;            :error {:type :validation
  ;;                    :table :group
  ;;                    :column :name
  ;;                    :error :not-unique}})))
  )

;;Super-admin ==================================================
(defmethod bv/validate ["super-admin" :create :group]
  [_ _ env _ _ {:keys [supergroup group-id name] :as new-record}]
  (Rules
   (rule/require-keys new-record [:name] {:table :group})
   (validate-unique-name env nil name)
   (rule (not (and supergroup group-id))
         "A group cannot be both supergroup and subgroup"
         {:group-id group-id
          :supergroup supergroup})
   ))

(defmethod bv/validate ["super-admin" :update :group]
  [_ _ env record mods {:keys [supergroup group-id] :as modded-record}]
  (Rules
   (rule (not (and supergroup group-id))
         "A group cannot be both supergroup and subgroup"
         {:id (:id record)
          :group-id group-id
          :supergroup supergroup})
   (rule/require-keys modded-record [:name] {:table :group})
   (validate-unique-name env (:name record) (:name mods))))

(defmethod bv/validate ["super-admin" :delete :group]
  [_ _ env record mods modded-record]
  (validate-delete-group env (:id record)))

;;Supergroup-admin ==================================================
(defmethod bv/validate ["supergroup-admin" :create :group]
  [_ _ {:keys [user] :as env} _ mods {:keys [id name] :as modded-record}]
  (Rules
   (rule/require-keys modded-record [:name] {:table :group})
   (validate-unique-name env nil name)
   (rule (= (:group-id modded-record) (:group-id user))
         "Group-id of new group has to be group-id of user"
         {:user-group-id (:group-id user)
          :group-id (:group-id modded-record)
          :mods mods})
   ))

(defmethod bv/validate ["supergroup-admin" :update :group]
  [_ _ {:keys [user] :as env} record mods {:keys [id name] :as modded-record}]
  (Rules
   (validate-supergroup-admin-access user modded-record)
   (rule/require-keys modded-record [:name] {:table :group})
   (validate-unique-name env (:name record) (:name mods))))

(defmethod bv/validate ["supergroup-admin" :delete :group]
  [_ _ env record mods modded-record]
  (validate-delete-group env (:id record)))

;; Group-admin ==================================================
(defmethod bv/validate ["group-admin" :update :group]
  [_ _ {:keys [user] :as env} record mods modded-record]
  (Rules
   ;;scope already disallows this
   (rule (= (:group-id user) (:id record))
         "Not allowed to update an inaccessible group"
         {:user-group-id (:group-id user)
          :updated-group-id (:id record)
          :mods mods})
   (rule/require-keys modded-record [:name] {:table :group})
   (validate-unique-name env (:name record) (:name mods))))


;;Files ========================================

(defn validate-file-format [{:keys [file-path id format]}]
  (rule (= format "original")
        "format should be original"
        {:format format :file-path file-path}))

(defn validate-file-type-for-group [{:keys [file-path type]}]
  (rule (contains? #{:app-logo :app-brand :pdf-logo} type)
        "Can only retrieve/upload app-logo, app-brand or pdf-logo for group"
        {:type type
         :file-path file-path}))

(defn validate-file-upload-for-group
  [env {:keys [file-path type id file-name] :as file-info}]
  (Rules
   (rule (and (number? (cu/parse-natural-number id))
              (not (empty? file-name)))
         "table id should be set properly and file-name should be set"
         {:id id :file-path file-path :file-name file-name})
   (validate-file-type-for-group file-info)))

(defn validate-supergroup-access-for-file [{:keys [user] :as env} {:keys [file-path id] :as file-info}]
  (let [id (cu/parse-natural-number id)]
    (rule (or (= (:group-id user) id)
              (cu/includes? (:subgroup-ids user) id))
          "group id or one of subgroup ids of user has to be same as id of the group associated with the file"
          {:file-path file-path :id id :user-group-id (:group-id user)
           :user-subgroup-ids (:subgroup-ids user)})))

(defn validate-group-admin-access-for-files [{:keys [user] :as env} {:keys [id file-path] :as file-info}]
  (rule (= id (:group-id user))
        "group id of user has to be same as id of the group associated with the file"
        {:file-path file-path :id id :user-group-id (:group-id user)})
  )

;;UPLOAD
(defmethod bv/validate ["super-admin" :file-upload :group]
  [_ _ env file-info _ _]
  (validate-file-upload-for-group env file-info))

(defmethod bv/validate ["supergroup-admin" :file-upload :group]
  [_ _ env file-info _ _]
  (Rules
   (validate-file-upload-for-group env file-info)
   (validate-supergroup-access-for-file env file-info))
  )

(defmethod bv/validate ["group-admin" :file-upload :group]
  [_ _ env file-info _ _]
  (validate-file-upload-for-group env file-info)
  (validate-group-admin-access-for-files env file-info)
  )


;; DOWNLOAD
(defmethod bv/validate ["super-admin" :file-download :group]
  [_ _ {:keys [user]} {:keys [file-path file-path-vector table id type format path] :as file-info} _ _]
  (Rules
   (validate-file-type-for-group file-info)
   (validate-file-format file-info)
   ))

(defmethod bv/validate ["supergroup-admin" :file-download :group]
  [_ _ {:keys [user] :as env} {:keys [file-path-vector id file-path] :as file-info} _ _]
  (Rules
   (validate-file-type-for-group file-info)
   (validate-file-format file-info)
   (validate-supergroup-access-for-file env file-info)
   ))

(defmethod bv/validate ["group-admin" :file-download :group]
  [_ _ {:keys [user] :as env} {:keys [file-path-vector id file-path] :as file-info} _ _]
  (Rules
   (validate-file-type-for-group file-info))
  (validate-file-format file-info)
  (validate-group-admin-access-for-files env file-info))
