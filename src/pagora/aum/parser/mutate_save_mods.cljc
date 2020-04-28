(ns pagora.aum.parser.mutate-save-mods
   #?(:cljs (:require-macros [pagora.clj-utils.macros :refer [assert-x]]))
  (:require [pagora.aum.database.query :refer [sql]]
            [pagora.aum.security :refer [get-whitelist]]
            [pagora.aum.database.inspect :as db-inspect]

            [pagora.clj-utils.core :as cu]

            [pagora.aum.util :as bu]
            #?(:clj
               [pagora.clj-utils.macros :refer [assert-x]])

            #?(:clj [clojure.java.jdbc  :as jdbc])
            [clojure.set :as set]
            [taoensso.timbre :as timbre :refer [info warn spy]]))

;;TODO don't save if mods don't change the record in the db!!!!
(defn commit-mod
  "Saves or updates a record (mods) to table according to method. If
  method is :create a mapping of id to the db id is added to tempids
  atom. If method is :update current-record is expected to contain the
  record. Filters keys of updates as set in database config. Throws
  error if fails. Returns id of updated/created record if successful."
  [{:keys [state user db-config tempids] :as env} {:keys [id table current-record updates method skip-validate?]}]
  ;; (timbre/info :#r "in commit-mod")

  (let ;; Add whitelisted original prop values for updates to state. If there's an
      ;; exception these get sent back to the client as table-data. Client can then restore the
      ;; record if they wish.
      [natural-number-id (cu/parse-natural-number id)
       whitelist (get-whitelist env method table user)
       updates  (assoc updates :last-updated-user-id (:id user))
       whitelisted-mods (select-keys updates whitelist)
       disallowed-keys (set/difference (set (keys updates)) (set whitelist))]
    (when (pos? (count disallowed-keys))
      (info :#r "Keys not allowed to set: " (vec disallowed-keys)))

    (assert-x (or (= method :create) natural-number-id) "id of record to update is not a natural number" {:id id})

    (if (zero? (count whitelisted-mods))
      (let [message "Either no key/value pairs to save are received, or none are allowed to be saved, not saving record"
            context {:id id :table table :mods updates :user-id (:id user)}]
        (timbre/warn message context)
        (swap! state update :warnings (fnil conj []) {:message message :context context :fn "commit-mod"})
        natural-number-id)

      (let [
            id
            (condp = method
              :create (sql env :insert-record  {:table          table
                                                :skip-validate? skip-validate?
                                                :mods           whitelisted-mods})
              :update (and (= 1 (sql env :update-record {:table          table :id natural-number-id
                                                         :skip-validate? skip-validate?
                                                         :current-record current-record
                                                         :mods           whitelisted-mods}))
                           natural-number-id))]
        (assert-x (and (number? id) (pos? id)) "Saving of record failed" {:table table :id id})
        id))))

(defn validate-foreign-key [env t1 t2 fk]
  (let [{belongs-to :join-type
         fk1 :t1-foreign-key :as t1-info} (db-inspect/get-join-info env t1 t2)
        {has-many :join-type
         fk2 :t1-foreign-key :as t2-info} (db-inspect/get-join-info env t2 t1)]
    ;; (timbre/info "INFO" t1 t2 fk)
    ;; (timbre/info :#pp info)
    ;; (timbre/info :#pp  (db-inspect/get-join-info env t2 t1))


    (assert-x (or (= belongs-to :belongs-to)
                  (= has-many :has-many))
              "Found a join to a newly created record, but join is not of belongs to type!"
              {:t1 t1 :t2 t2 :t1-info t1-info :t2-info t2-info})
    (assert-x (or (= fk fk1) (= fk fk2))
              "Found a join to a newly created record, but foreign-key is not what it should be!"
              {:found-fk fk
               :t1 t1 :t2 t2 :t1-info t1-info :t2-info t2-info})))

;; (swap! state update :_original-table-data #(u/deep-merge-concat-vectors % {table-by-id {natural-number-id current-record}}))

(defn process-mod
  [{:keys [new-mods-by-id state] :as env} {:keys [table method id query mod-id] :as mod} ret]
  (try
    ;;Before we save the mod itself, let's see if there's any belongs-to foreign
    ;;keys that need to resolved to real db-id's first:
    (let [{:keys [modded-tables mods-processed updated-foreign-keys errors]}
          (->> (:updates mod)
               (reduce (fn [ret [fk tempid]] ;;well, they might be a fk and tempid..
                         (let [belongs-to-mod (get new-mods-by-id tempid)]
                           (if (some? belongs-to-mod) ;;they have to be!!
                             (do
                               ;;We've got a prop that's has the value of a tempid of a
                               ;;new record!. This is only possible if it's a foreign
                               ;;key. Let's make sure that join-type and foreign key
                               ;;are as they should be:
                               (validate-foreign-key env table (:table belongs-to-mod) fk)

                               (let [ret (cond->> ret
                                           (not (contains? (:mods-processed ret) (:mod-id belongs-to-mod)))
                                           ;;We haven't seen this belongs-to mod
                                           ;;before, so let's save that one first:
                                           (process-mod env belongs-to-mod))
                                     ;;mods-processed should really contain this belongs-to mod now:
                                     {:keys [db-id]} (get (:mods-processed ret) (:mod-id belongs-to-mod))]
                                 ;;We update the fk tempid to a real db-id
                                 (assoc-in ret [:updated-foreign-keys fk] db-id)))
                             ret)))
                       ret))
          ;;Let's update our mod's foreign keys to the proper db-id's
          mod (update mod :updates merge updated-foreign-keys)

          ;;For validating purposes we fetch any existing record for this mod first:
          {current-record :record} (when (= method :update)
                                     (bu/read-record (assoc-in env [:parser-config :allow-root] true)
                                                     {:table table :id id :query ['*]}))
          ;;And save the mod itself:
          record-id (commit-mod env (assoc mod :current-record current-record))
          modded-tables (cond-> modded-tables
                          record-id ((fnil conj #{}) table))
          {query-table-data :table-data} (try (bu/read-record env {:table table :id record-id :query query})
                                              (catch #?(:clj Exception
                                                        :cljs :default) e
                                                (timbre/info e)))]
      (swap! state update :table-data #(cu/deep-merge-concat-vectors
                                        % query-table-data
                                        ;; {(bu/table->table-by-id table)
                                        ;;    {record-id queried-updated-record}}
                                        ))
      {:modded-tables modded-tables
       :errors errors
       :mods-processed (assoc mods-processed mod-id (assoc mod :db-id record-id))})
    (catch
        #?@(:clj [Exception e])
        #?@(:cljs [:default e])
      (let [{:keys [msg context]} (cu/parse-ex-info e)
            msg (or msg (.toString e))]
        (update ret :errors (fnil conj []) (assoc mod :error {:msg msg
                                                              :content context}))))))

(defn is-uuid? [id]
  (string? id)
  ;; (cu/is-uuid? id)
  )

(defn save-mods [env {:keys [mods queries]}]
  ;;The mods come in like this:
  ;; {:some-table {"some-id" {:updates {:some :mods}
  ;;                          :query [:some :query]}}}
  ;;Let's make a vector of mods out this first:
  ;; (timbre/info :#pp mods)
  ;; (timbre/info :#pp queries)

  (let [mods           (reduce (fn [acc [table mods-by-id]]
                                 (into acc (reduce (fn [acc [id mods-map]]
                                                     (let [{:keys [updates query]} mods-map]
                                                       (conj acc {:table  table :updates updates :id id
                                                                  :method (if (is-uuid? id) :create :update)
                                                                  :query  (or query (get queries table))
                                                                  })))
                                                   [] mods-by-id)))
                               [] mods)
        ;;and index them
        mods           (map-indexed #(assoc %2 :mod-id %1) mods)
        ;;Any mods that that have a tempid for id are new records:
        new-mods-by-id (reduce (fn [new-mods-by-id {:keys [id method] :as mod}]
                                 (cond-> new-mods-by-id
                                   (= method :create) (assoc id mod)))
                               nil mods)
        ;;For easy lookup add this to the env:
        env            (assoc env :new-mods-by-id new-mods-by-id)]

    ;; (timbre/info :#pp mods)

    ;;Save all the mods. In the ret value we track results as we move from mod
    ;;to mod sequentially and recursively:
    (loop [ret  nil
           mods mods]
      (if (seq mods)
        (let [{:keys [mod-id] :as mod} (first mods)
              mods                     (rest mods)
              {:keys [mods-processed]} ret]
          ;;but skip the ones that we've processed already (when we followed a
          ;;join):
          (if (contains? mods-processed mod-id)
            (recur ret mods)
            (let [ret (process-mod env mod ret)]
              (recur ret mods))))
        ret))))

(defn cleanup-result [{:keys [errors mods-processed]}]
  {:errors (mapv (fn [{:keys [table id error updates]}]
                   {:table table :id id :error error :updates updates})
                 errors)
   :saved (mapv (fn [{:keys [table id method updates]}]
                  {:table table :id id :method method :updates updates})
                (vals mods-processed))})

(defn save-mods-transaction
  [{:keys [db-conn] :as env} {:keys [atomic? simulate?] :as params}]
  ;;http://clojure-doc.org/articles/ecosystem/java_jdbc/using_sql.html#using-transactions
  ;; (timbre/info "PARAMS")
  ;; (timbre/info :#pp params)
  #?(:clj
     (do
       (jdbc/with-db-transaction [tx db-conn]
         (when simulate? (jdbc/db-set-rollback-only! tx))
         ;; (timbre/info "is-rollback-only?" (jdbc/db-is-rollback-only tx))
         (let [result (save-mods (cond-> env
                                   (or simulate? atomic?) (assoc :db-conn tx)) params)]
           (when (seq (:errors result))
             (when atomic?
               (throw (ex-info "Something failed to save, so we rollback all saves!"
                               (cleanup-result result)))

               ;; (throw (ex-info (str "Some records failed to save"
               ;;                      (when simulate? ", all mods to db are rolled back since simultate? is true"))
               ;;                 (cleanup-result result)))
               ))

           ;;TODO: return tempids when atomic? is false and some records did save even when others didn't!!!
           (let [{:keys [modded-tables mods-processed]} result
                 mods-processed (vals mods-processed)
                 tempids (->> mods-processed
                              (filter #(= (:method %) :create))
                              (reduce (fn [acc {:keys [id db-id db-id table]}]
                                        (let [table-by-id (bu/table->table-by-id table)]
                                          (assoc acc [table-by-id id] [table-by-id db-id])))
                                      {}))]
             (when simulate? (timbre/info :#r "Rolling back any mods to db since simulate? is true"))
             (let [{:keys [errors saved]} (cleanup-result result)
                   {created true updated false} (group-by #(= (:method %) :create) saved)]
               {:value (cond-> {:tempids tempids
                                :keys (into [] modded-tables)}
                         simulate? (assoc :simulated? true)
                         (seq errors) (assoc :errors errors)
                         (seq created) (assoc :created (mapv #(select-keys % [:table :id]) created))
                         (seq updated) (assoc :updated (mapv #(dissoc % :method) updated)))})))))))


(comment
  (binding [aum.security/*schema-warnings* false]
    (let [mods  {:qbucket { "41ab77e4-bb4c-41cf-add2-aef4f5144ea8" {:updates {:name "foo"
                                                                              :group-id 10
                                                                              :type "template"
                                                                              }}}
                 :qbucket-qbucket {"c4af5bfb-a9e4-486b-a1f3-38901ba0dbaa" {:updates {:qbucket-id 435,
                                                                                     :type nil,
                                                                                     :order 0.5,
                                                                                     ;; :sub-type "category"
                                                                                     :unlinked false,
                                                                                     :sub-qbucket-id "41ab77e4-bb4c-41cf-add2-aef4f5144ea8"}}}
                 ;; :dossier-type {
                 ;;                "dt-tempid-1" {:updates
                 ;;                               {:name "some dossier-type" ; :user-id "user-temp-id 1"
                 ;;                                :group-id "10"
                 ;;                                }
                 ;;                               :query [:updated-at]}}
                 ;; :group {"group-tempid-1" {:updates {:name "admin" :supergroup true :group-id 10}
                 ;;                           :query   [:created-at :updated-at :deleted]}}
                 ;; :field {"field-tempid-1" {:updates {:label "field 1" :type "string" :order 1
                 ;;                                     :dossier-type-id "dt-tempid-1"}}}
                 ,

                 ;; :zed {"tempid-1" {:type :zed, :name "zed-name" :zex-id "tempid-2"}}
                 ;; :zex {"tempid-2" {:type :zex, :name "zex-name"}}
                 }
          query '[(admin/save-mods
                   {:mods    mods
                    :queries {:user [:email],
                              }})]

          do-query (fn [query]
                     (println "++++++++++++++++++++++++++++++++++++++++++++++++++++++")

                     (let [db-conn    database.connection/db-conn
                           db-config  database.config/db-config
                           raw-schema (aum.database.schema/get-schema db-conn)
                           schema     (aum.database.schema/make-condensed-schema raw-schema)
                           ;; _ (pprint (get schema "users"))
                           state      (atom {:status :ok})
                           config     dc-admin.backend.app.config/config
                           env        {:parser-config (merge config {:allow-root true :print-exceptions true})
                                       :aum-keys [:db-config :parser-config :sql :db-conn :schema :cb :subquery-path aum-keys]
                                       :db-conn       db-conn
                                       :db-config     db-config
                                       :schema        (aum.security/secure-schema schema db-config)
                                       :raw-schema    raw-schema
                                       :state         state
                                       :parser        parser.core/parser
                                       :user          {:id 1990 :some-user "afoobar" :role "super-admin" :group-id 10 :subgroup-ids [-1]}}
                           ;; result     (parser.core/parser env query)
                           ret (save-mods-transaction env {:mods mods
                                                           ;; :atomic? true
                                                           :simulate? true
                                                           })
                           ;; ret (perform-query env [{:dossier-type [:updated-at]}])
                           ]
                       (timbre/info ret)
                       ;; (timbre/info "join-info!!" (db-inspect/_get-join-info env :user :template))
                       ;; (timbre/info "join-info!!" (db-inspect/_get-join-info env :user :group))
                       (info "State:")
                       (pprint @state)
                       ;; (println "----------------------------------------")
                       ;; (info "Result:")
                       ;; (pprint result)
                       ))]
      (do-query query)
      )

    ))


;; (require
;;  '[aum.app-config :refer [config]]
;;  '[clojure.test :refer [deftest is]]
;;  '[aum.test.util :as tu :refer [debug-tests unload-all-tests unmap-all-interns in-context truthy?
;;                                   make-context query]]
;;  )


;; (def rows
;;   [{:id 1 :title "foo" :published true :updated-at nil :created-at nil}
;;    ])

;; (def parser-config
;;   {;; Om-next parser
;;    :limit-max 100 ;with joins query time can grow exponentially, so impose top limit on number of queried records
;;    :derive-join-type-from-schema true ;if keys and table names are regular saves configuring db
;;    :om-process-roots false          ;whether to ignore keys that are not a table
;;    :normalize true                  ;whether to embed joins or store in a map

;;    :sql-log true                        ;print actual sql queries being made
;;    :query-log true
;;    :validate-log true

;;    :simulate-network-latency false
;;    :latency 2000

;;    ;;Event store is disabled since it's not yet in master
;;    :event-store-disabled false;; we might also want to disable this in testing for instance

;;    :print-exceptions true
;;    })

;; (def job-offer-config
;;   {:root true
;;    :columns [:department-description :department-id :id :candidate-description :published :location :updated-at :checklist-id :dc-description :offer-description :title :created-at :creator-id :procedure-description :publish-at]
;;    :joins {:user {:t1-foreign-key :creator-id}}
;;    :read {:role {"super-admin" {:blacklist []}}}
;;    :update {:role {"super-admin" {:blacklist [:id :updated-at :created-at :creator-id]}}}
;;    :create {:role {"super-admin" {:blacklist [:id :updated-at :created-at]}}}})

;; (def db-config (select-keys database.config/db-config [:job-offer]))

;; (pprint db-config)
;; (pprint {:job-offer job-offer-config})
;; (def context
;;   (make-context
;;    {:db-config {:job-offer job-offer-config}


;;     :parser-config (merge parser-config {:allow-root true :print-exceptions true
;;                                          :event-store-disabled true})
;;     :fixtures   {:job-offers {:rows rows :options {:id-primary-key? true}
;;                               :schema {:id :int :title :text :published :boolean
;;                                        :updated-at :date-time :created-at :date-time}
;;                               }}}))

;; (def user {:id 1990 :some-user "afoobar" :role "super-admin" :group-id 10 :subgroup-ids [-1]})

;; (in-context context
;;   (perform-query (assoc tu/*env*
;;                         :user user
;;                         :parser tu/*parser*)
;;                  [{:job-offer [:id]}]))
