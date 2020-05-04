(ns pagora.aum.modules.events.create
  (:require
   ;; [clj-time.core :as t]
   [digicheck.common.util :as du]
   [app.config :refer [config]]
   #?@(:clj
       [[clj-time.core :as t]
        [clj-time.coerce :as c]
        [clj-time.format :as f]])
   #?(:clj [clojure.data.json :as json])
   [clojure.pprint :refer [pprint]]
   [taoensso.timbre :as timbre :refer [error info]]
   [clojure.pprint :refer [pprint]]
   #?(:clj [jansi-clj.core :as jansi])
   [bilby.database.query :refer [sql]]))

(defn find-highest-seq-no
  "Given event-name returns highest sequence number for that event, or
  nil if none found"
  [env {:keys [entity-id entity-type]}]
  ;;TODO remove when event-store is added in chin master
  (try
    (->> {:table :event-store
          :cols [:sequence-no]
          :where-clause ["where `entity_id` = ? AND `entity_type` = ?" entity-id entity-type]
          :order-by-clause ["order by sequence_no desc"]
          :limit-clause ["limit 1"]}
         (sql env :get-cols-from-table)
         first
         :sequence-no)
    #?(:clj (catch Exception e
              (timbre/error "Event store table doesn't exist!!!!" e)))
     #?(:cljs (catch :default e
              (timbre/error "Event store table doesn't exist!!!!")))
    ))

(defn validate-event [{:keys [name entity-id entity-type data-version]}]
  (and (string? name)
       (string? entity-type)
       (number? entity-id)
       (= data-version 1)))

(defn write-event
  "Actually writes event to event-store table"
  [{:keys [parser-config] :as env} event]
  (try
    (cond
      (:event-store-disabled parser-config)
      (timbre/info "Event (not recorded, disabled in config):" event)
      (not (validate-event event))
      (timbre/info "Event (not recorded, event failed validation):" event)
      :else (let [last-seq-number (or (find-highest-seq-no env event) 0)
                  event (assoc event :sequence-no (inc last-seq-number))]
              (sql env :insert-event {:table :event-store
                                      :skip-validate? true
                                      :mods event})
              ;; (timbre/info (:name event))
              )
      )
    #?(:clj (catch Exception e
              (timbre/error :#r "Event store table doesn't exist!!!!" e)))
    #?(:cljs (catch :default e
               (timbre/error "Event store table doesn't exist!!!!")
               ))

    ))

(defmulti prepare-for-json
  #(type %))

(defmethod prepare-for-json :default [x]
  x)

#?(:clj
   (defmethod prepare-for-json java.sql.Timestamp [x]
     ;; (f/unparse (f/formatters :mysql) (c/from-sql-time x))
     (.toString (.toInstant x))
     ))

(defn make-event
  "Returns an event map that can be written to the event-store"
  [{:keys [user] :as env} event-name table id data]
  (let [data (into {} (map (fn [[k v]]
                             [(du/hyphen->underscore (name k)) (prepare-for-json v)])
                           data))]
    {:name event-name
     :data
     #?(:clj (json/write-str data))
     #?(:cljs (JSON.stringify data))
     :data-version 1
     :entity-type table
     :entity-id id
     :user-id (:id user)
     :group-id (:group-id user)}))

(defn make-generic-event
  "Returns a generic record-created, record-updated or record-deleted
  event map."
  [env method table id data]
  (let [event-name (str (name table) "-" (condp = method
                                           :create "created"
                                           :update "modified"
                                           :delete "deleted"
                                           (name method)))]
    (make-event env event-name (name table) id data)))

(defmulti event
  "By default created a generic create/update/delete event in event-store table"
  (fn [env method table id data]
    [method table]))

(defmethod event :default
  [env method table id data]
  ;; TODO-CLJC
  (write-event env (make-generic-event env method (name table) id data)))

(defn action-event
  [env action table id data]
  (let [event-name (str (name table) "-" (name action))
        event (make-event env event-name (name table) id data)]
    (write-event env event)))

;; Some sample code to create more specific events:
;; (defmethod event [:update :dossier-type]
;;   [env method table id data]
;;   (let [event-name "some-specific-event"
;;         data  {:whatever :data :you :want :to :add}
;;         event (make-event env event-name table id data)]
;;     (write-event env event))
;;   ;;...
;;   ;;Write generic-event (or not)
;;   (write-event env (make-generic-event env method table id data))
;;   )
