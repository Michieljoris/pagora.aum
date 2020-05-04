(ns pagora.aum.modules.translations.db-config
  #?(:cljs (:require-macros [bilby.database.validate.Rule :refer [rule]]))
  (:require
   [bilby.database.validate.core :as bv :refer [Rules]]
   #?(:clj [bilby.database.validate.Rule :refer [rule]])
   [database.table.util :refer [supergroup-admin-scope group-admin-scope
                                validate-supergroup-admin-access
                                validate-group-admin-access]]
   [bilby.database.query :refer [sql]]
   [bilby.database.validate.rules :as rule]
   [digicheck.common.util :as du]
   #?(:clj [clojure.pprint :refer [pprint]])
   #?(:cljs [cljs.pprint :refer [pprint]])
   [taoensso.timbre :as timbre :refer [error info]]
   [cuerdas.core :as str]))

;; (dev/get-all-columns-of-table "translations")

(def config
  {:root      true
   :limit-max nil ;;we want to be able to get all translations for the app itself
   :columns   [:id :key :en :nl :de :group-id :translation-id]
   :event {:alias-for-table :event-store
           :type :has-many :foreign-key :entity-id}
   ;; :joins {:translation :belongs-to}
   :read      {:role {"super-admin"      {:blacklist []}
                      "supergroup-admin" {:blacklist []
                                          :scope     [:or [[:group-id :is :null]
                                                           supergroup-admin-scope]]
                                          }
                      "group-admin"      {:blacklist []
                                          :scope     [:or [[:group-id :is :null]
                                                           group-admin-scope]]}}}
   :create    {:role {"super-admin"      {:blacklist []}
                      ;;These admins can only make shadow translations
                      "supergroup-admin" {:blacklist [:key]}
                      "group-admin"      {:blacklist [:key]}}}
   :update    {:role {"super-admin"      {:whitelist [:key :en :nl :de]}
                      "supergroup-admin" {:whitelist [:en :nl :de]
                                          :scope     supergroup-admin-scope}
                      "group-admin"      {:whitelist [:en :nl :de]
                                          :scope     group-admin-scope}}}
   :delete    {:role {"super-admin"      {}
                      "supergroup-admin" {:scope supergroup-admin-scope}
                      "group-admin"      {:scope group-admin-scope}}}})

(defn is-root-translation [{:keys [key group-id translation-id] :as translation}]
  (and (seq key) (nil? group-id) (nil? translation-id)))

(defn is-shadow-translation [{:keys [key group-id translation-id] :as translation}]
  (and (nil? key) (some? group-id) (some? translation-id)))

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
           group-id and translation-id are nil, or vice versa." new-record)

     (when root-translation?
       (validate-key env nil new-record new-record))

     (when shadow-translation?
       (Rules
        (validate-translation-id env new-record)
        (let [group (sql env :get-cols-from-table
                         {:table :group
                          :where-clause ["where `id` = ?" (:group-id new-record)]})]
          (rule (= (count group) 1)
                "group-id of shadow translation should point to an existing group"
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

;; Supergroup admin ==================================================
(defmethod bv/validate ["supergroup-admin" :create :translation]
  [_ _ {:keys [user] :as env} _ new-record _]
  (Rules
   (validate-is-shadow-translation new-record)
   (validate-translation-id env new-record)
   (validate-supergroup-admin-access user new-record)))

(defmethod bv/validate ["supergroup-admin" :update :translation]
  [_ _ env record mods modded-record]
  (Rules
   (validate-is-shadow-translation record)
   (validate-supergroup-admin-access (:user env) record))
  )

(defmethod bv/validate ["supergroup-admin" :delete :translation]
  [_ _ env record mods modded-record]
  (Rules
   (validate-is-shadow-translation record)
   (validate-supergroup-admin-access (:user env) record)))

;; Group admin ==================================================
(defmethod bv/validate ["group-admin" :create :translation]
  [_ _ env record new-record modded-record]
  (Rules
   (validate-is-shadow-translation new-record)
   (validate-translation-id env new-record)
   (validate-group-admin-access (:user env) new-record)))

(defmethod bv/validate ["group-admin" :update :translation]
  [_ _ env record mods modded-record]
  (Rules
   (validate-is-shadow-translation record)
   (validate-group-admin-access (:user env) record))
  )

(defmethod bv/validate ["group-admin" :delete :translation]
  [_ _ env record mods modded-record]
  (Rules
   (validate-is-shadow-translation record)
   (validate-group-admin-access (:user env) record)
   ))
