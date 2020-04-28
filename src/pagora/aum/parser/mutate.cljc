(ns pagora.aum.parser.mutate
  (:require
   [pagora.aum.database.query :refer [sql]]
   [pagora.aum.security :refer [get-validation-fun get-whitelist logout]]
   [pagora.clj-utils.core :as cu]
   [pagora.aum.parser.mutate-save-mods :refer [save-mods-transaction]]
   [pagora.aum.util :as au]
   [clojure.set :as set]
   [taoensso.timbre :as timbre]))

;;for repl-ing
;;(ns-unmap *ns* 'mutate))

(defmulti mutate (fn [_ k _] k))

;;The aum mutate throws on error, here we make sure we return an error key
;;in the value map.
(defn mutate-handle-errors
  "Includes error handling. To deal with exceptions yourself use mutate*"
  [{:keys [state] :as env} key params]
  (timbre/info :#r "Mutate handle errors foo")
  (let [{:keys [param-keys] :as post-remote} (:_post-remote params)
        params (dissoc params :_post-remote)
        post-remote-params (merge (select-keys params param-keys)
                                  (:params post-remote))
        post-remote (dissoc post-remote :param-keys :params)
        mutation-result (try
                          (let [{{:keys [tempids errors]} :value
                                 :as result} (mutate env key params)]
                            (timbre/info :#pp result)
                            (timbre/info tempids)
                            (when (some? tempids)
                              (swap! state update :tempids #(conj (or % {}) tempids)))
                            (timbre/info @state)
                            (when (some? errors)
                              (swap! state assoc :errors errors))
                            {:value result})
                          (catch
                              #?@(:clj [Exception e])
                              #?@(:cljs [:default e])
                              (let [{:keys [msg context stacktrace]} (cu/parse-ex-info e)
                                    keys (or (:keys @state) [])]
                                (timbre/info e)
                                (swap! state update :status (constantly :error))
                                (swap! state update
                                       :original-table-data #(cu/deep-merge-concat-vectors % (:_original-table-data @state)))
                                (swap! state dissoc :keys :_original-table-data)
                                {:value {:keys keys :error
                                         {:message (or msg (.toString e)) :context context
                                          ;; :stacktrace #?(:clj :not-returned
                                          ;;                :cljs stacktrace)
                                          }
                                         }})))
        mutation-result (cond->> mutation-result
                          (not (map? mutation-result)) (hash-map :mutation-result))]

 ;; (timbre/info "mutation-result" mutation-result)
    (cond-> mutation-result
      (some? post-remote) (assoc-in  [:value :post-remote] (assoc post-remote
                                                                  :params post-remote-params)))))

(defmethod mutate :default
  [{:keys [user]} key params]
  (throw (ex-info "Unknown mutation" {:key key :params  params})))

(defn save-record!
  "Saves or updates a record (mods) to table according to method. If
  method is :create a mapping of id to the db id is added to tempids
  atom. If method is :update table-data is expected to contain the
  record on the path of [record/by-id id]..Filters keys record as set
  in database config. Throws error if fails. Returns id of
  updated/created record if successful."
  [{:keys [state user table-data tempids] :as env} {:keys [id table mods]} {:keys [method skip-validate?]}]
  ;; (timbre/info :#r "in saverecord" table-data id table mods (string? id))
  (let ;; Add whitelisted original prop values for mods to state. If there's an
      ;; exception these get sent back to the client as table-data. Client can then restore the
      ;; record if they wish.
      [natural-number-id (cu/parse-natural-number id)
       whitelist (get-whitelist env method table user)
       whitelisted-mods (select-keys mods whitelist)
       disallowed-keys (set/difference (set (keys mods)) (set whitelist))
       table-by-id (keyword (str (name table) "/by-id"))]

    (swap! state assoc :keys [table])

    ;; Let the side effects begin!!!
    (when (pos? (count disallowed-keys))
      (timbre/info :#r "Keys not allowed to set: " (vec disallowed-keys)))

    (when (= method :update)
      (if-not natural-number-id (throw (ex-info "id of record to update is not a natural number"
                                                {:id id
                                                 :table table}))))
    (if (zero? (count whitelisted-mods))
      (let [message "Either no key/value pairs to save are received, or none are allowed to be saved, not saving record"
            context {:id id :table table :mods mods :user-id (:id user)}]
        (timbre/warn message context)
        (swap! state update :warnings #(conj (or % []) {:message message :context context :fn "save-record!"}))
        natural-number-id)
      (let [id
            (condp = method
              :create (let [new-id
                            (try
                              (sql env :insert-record  {:table table
                                                        :skip-validate? skip-validate?
                                                        :mods whitelisted-mods})
                              (catch
                                  #?@(:clj [Exception e])
                                  #?@(:cljs [:default e])
                                (timbre/info e)
                                (throw e)))]
                        (swap! tempids assoc [table-by-id id] [table-by-id new-id])
                        new-id)
              :update (let [current-record (get-in table-data [table-by-id natural-number-id])
                            original-record (select-keys current-record (keys whitelisted-mods))]
                        (swap! state update :_original-table-data #(cu/deep-merge-concat-vectors % {table-by-id {natural-number-id original-record}}))
                        ;; (timbre/info "save-record" table-data)
                        (if (= 1 (sql env :update-record {:table table :id natural-number-id
                                                          :skip-validate? skip-validate?
                                                          :current-record current-record
                                                          :mods whitelisted-mods}))
                          natural-number-id -1)))]
        (when-not (and (number? id) (pos? id))
          (throw (ex-info "Saving of record failed" {:table table :id id})))
        id))))

(defn mutate-save-record
  [{:keys [user parser state] :as env} {:keys [id table mods query] :as params}]
  (let [method (if (cu/is-uuid? id) :create :update)
        tempids (atom nil)
        table-data (when (= method :update)
                     (au/perform-query env [(list {table ['*]} {:where [:id := id]})]))
        ;; _ (timbre/info "mutate-save-record" table-data)
        env (assoc env
                   :tempids tempids
                   :table-data table-data)
        record-id (save-record! env params {:method method})]
    {:tempids @tempids
     :keys (if query [table] [])
     :record-id record-id
     :method method}))

(defmethod mutate 'aum/save-record
  [{:keys [user parser state] :as env} _ params]
  (let [{:keys [record-id] :as result} (mutate-save-record env params)
        {:keys [table-data]} (au/read-record env (assoc params :id record-id))]
    (swap! state update :table-data #(cu/deep-merge-concat-vectors % table-data))
    (select-keys result [:tempids :keys])))

"Retrieves current record and calls sql fn to delete it."
(defmethod mutate 'aum/delete-record
  [{:keys [user parser] :as env} key {:keys [table id]}]
  (let [table-data  (au/perform-query env [(list {table ['*]} {:where [:id := id]})])
        natural-number-id (cu/parse-natural-number id)
        table-by-id (keyword (str (name table) "/by-id"))
        current-record (get-in table-data [table-by-id natural-number-id])]
    {:count (sql env :delete-record {:table table :id id :current-record current-record})}))


;; (defmethod mutate 'aum/save-records
;;   [{:keys [user parser state] :as env} _ params]
;;   (let [{:keys [record-id] :as result} (mutate-save-record env params)
;;         {:keys [table-data]} (au/read-record env (assoc params :id record-id))]
;;     (swap! state update :table-data #(cu/deep-merge-concat-vectors % table-data))
;;     (select-keys result [:tempids :keys])))

(defmethod mutate 'aum/save-records
  [env _ params]
  (timbre/info :#r "aum/save-mods")
  (save-mods-transaction env params)

  ;; {:tempids @tempids
  ;;  :keys (if query [table] [])
  ;;  :record-id record-id
  ;;  :method method}
  )
