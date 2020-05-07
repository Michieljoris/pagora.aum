(ns pagora.aum.modules.translations.db-config
  #?(:cljs (:require-macros [pagora.aum.database.validate.Rule :refer [rule]]))
  (:require
   [pagora.aum.database.validate.core :as bv :refer [Rules]]
   #?(:clj [pagora.aum.database.validate.Rule :refer [rule]])
   [pagora.aum.modules.auth.db-config.util :refer [master-account-admin-scope account-admin-scope
                                                   validate-master-account-admin-access
                                                   validate-account-admin-access]]
   [pagora.aum.database.query :refer [sql]]
   [clojure.pprint :refer [pprint]]
   [taoensso.timbre :as timbre :refer [error info]]
   [cuerdas.core :as str]))

;; (dev/get-all-columns-of-table "translations")

(def config
  {:root      true
   :limit-max nil ;;we want to be able to get all translations for the app itself
   :columns   [:id :key :en :nl :de :account-id :translation-id]
   :event {:alias-for-table :event-store
           :type :has-many :foreign-key :entity-id}
   ;; :joins {:translation :belongs-to}
   :read      {:role {"super-admin"      {:blacklist []}
                      "master-account-admin" {:blacklist []
                                          :scope     [:or [[:account-id :is :null]
                                                           master-account-admin-scope]]
                                          }
                      "account-admin"      {:blacklist []
                                          :scope     [:or [[:account-id :is :null]
                                                           account-admin-scope]]}}}
   :create    {:role {"super-admin"      {:blacklist []}
                      ;;These admins can only make shadow translations
                      "master-account-admin" {:blacklist [:key]}
                      "account-admin"      {:blacklist [:key]}}}
   :update    {:role {"super-admin"      {:whitelist [:key :en :nl :de]}
                      "master-account-admin" {:whitelist [:en :nl :de]
                                          :scope     master-account-admin-scope}
                      "account-admin"      {:whitelist [:en :nl :de]
                                          :scope     account-admin-scope}}}
   :delete    {:role {"super-admin"      {}
                      "master-account-admin" {:scope master-account-admin-scope}
                      "account-admin"      {:scope account-admin-scope}}}})

(defn is-root-translation [{:keys [key account-id translation-id] :as translation}]
  (and (seq key) (nil? account-id) (nil? translation-id)))

(defn is-shadow-translation [{:keys [key account-id translation-id] :as translation}]
  (and (nil? key) (some? account-id) (some? translation-id)))

(defn validate-is-root-translation
  ([translation] (validate-is-root-translation translation nil))
  ([translation msg]
   (rule (is-root-translation translation)
         (or msg "should be a root translation")
         {:translation translation})))

(defn validate-is-shadow-translation [translation]
  (rule (is-shadow-translation translation)
        "should be a shadow translation"
        {:translation translation}))

(defn validate-key [env record mods {:keys [key] :as modded-record}]
  (let [method (if record :update :create)]
    (Rules
     (rule (let [key-length (count key)]
             (< key-length 512))
           "Key should be shorter than 512 characters."
           {:key key :length (count key) :max-length 511})
     (rule (let [duplicate-keys (sql env :get-cols-from-table {:table :translation :cols [:key]
                                                               :where-clause ["where `key` = BINARY ?" key]})
                 max-duplicates (condp = method
                                  :create 0
                                  :update (if (= key (:key record)) 1 0))]
             (= (count duplicate-keys) max-duplicates))
           "Duplicate key!"
           modded-record)
     (rule (let [lower-cased-key (apply str (str/lower (str (first key))) (rest key))]
             (= lower-cased-key key))
           "Key should have lowercased first letter"
           {:key key}))))

(defn validate-translation-id [env record]
  (let [root-translation (sql env :get-cols-from-table
                              {:table :translation
                               :where-clause ["where `id` = ?" (:translation-id record)]})]
    (Rules
     (rule (= (count root-translation) 1)
           "translation-id of shadow translation should point to an existing root translation"
           {:translation record})
     (validate-is-root-translation (first root-translation) "shadow translation should point to a root translation"))))


;; Super admin ==================================================
(defmethod bv/validate ["super-admin" :create :translation]
  [_ _ env _ new-record _]
  (let [root-translation? (is-root-translation new-record)
        shadow-translation? (is-shadow-translation new-record)]
    (Rules
     (rule (or root-translation? shadow-translation?)
           "Has to be root or shadow translation, so either key is set, and
           account-id and translation-id are nil, or vice versa." new-record)

     (when root-translation?
       (validate-key env nil new-record new-record))

     (when shadow-translation?
       (Rules
        (validate-translation-id env new-record)
        (let [account (sql env :get-cols-from-table
                         {:table :account
                          :where-clause ["where `id` = ?" (:account-id new-record)]})]
          (rule (= (count account) 1)
                "account-id of shadow translation should point to an existing account"
                {:new-record new-record})))))))

(defmethod bv/validate ["super-admin" :update :translation]
  [_ _ env record mods modded-record]
  (Rules
   (when (is-root-translation record)
     (validate-key env record mods modded-record))
   (when (is-shadow-translation record)
     (rule (nil? (:key modded-record))
           "Can't set key prop of shadow translation"
           {:mods mods
            :translation record}))))

(defmethod bv/validate ["super-admin" :delete :translation]
  [_ _ env record mods modded-record])

(defmethod bv/validate ["super-admin" :bulk-create :translation]
  [_ _ env record mods modded-record]
  )

;; Master-Account admin ==================================================
(defmethod bv/validate ["master-account-admin" :create :translation]
  [_ _ {:keys [user] :as env} _ new-record _]
  (Rules
   (validate-is-shadow-translation new-record)
   (validate-translation-id env new-record)
   (validate-master-account-admin-access user new-record)))

(defmethod bv/validate ["master-account-admin" :update :translation]
  [_ _ env record mods modded-record]
  (Rules
   (validate-is-shadow-translation record)
   (validate-master-account-admin-access (:user env) record))
  )

(defmethod bv/validate ["master-account-admin" :delete :translation]
  [_ _ env record mods modded-record]
  (Rules
   (validate-is-shadow-translation record)
   (validate-master-account-admin-access (:user env) record)))

;; Account admin ==================================================
(defmethod bv/validate ["account-admin" :create :translation]
  [_ _ env record new-record modded-record]
  (Rules
   (validate-is-shadow-translation new-record)
   (validate-translation-id env new-record)
   (validate-account-admin-access (:user env) new-record)))

(defmethod bv/validate ["account-admin" :update :translation]
  [_ _ env record mods modded-record]
  (Rules
   (validate-is-shadow-translation record)
   (validate-account-admin-access (:user env) record))
  )

(defmethod bv/validate ["account-admin" :delete :translation]
  [_ _ env record mods modded-record]
  (Rules
   (validate-is-shadow-translation record)
   (validate-account-admin-access (:user env) record)
   ))
