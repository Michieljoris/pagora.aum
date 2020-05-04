(ns pagora.aum.frontend.parser.mutate-record
  (:require [pagora.aum.util :as au]
            [pagora.clj-utils.core :as cu :refer [deep-merge string->number]]
            [pagora.aum.om.next :as om]
            [cljs.pprint :refer [pprint]]
            [pagora.aum.frontend.parser.mutate :refer [mutate post-remote]]
            [pagora.aum.frontend.parser.mutate-helpers :refer [is-dirty? modify-record update-unsaved-records]]
            [pagora.aum.frontend.parser.calc-mods :refer [calc-mods]]
            [taoensso.timbre :as timbre :refer-macros [info]])
  (:require-macros
   [pagora.clj-utils.macros :refer [if-let*]]))

(defn id->real-id
  "This just makes sure that id is either a number or a tempid, since
  it can be a string that contains a number or a uuid string"
  [id]
  (when id
    (let [number-id (string->number id)]
      (if (nil? number-id)    ;tempid is a string, not a tempid type object
        (au/create-tempid id)
        number-id))))


(defn process-error [{:keys [context message] :as error} params]
  (cond
    (= (-> context :error :type) :validation) {:client/warning {:message message}}
    (re-matches #"Duplicate entry.*" message) {:client/warning {:message message}}
    :else {:client/error (assoc error
                                :mutation "save-record"
                                :params params)}
    )
  )
;; (def some-error {:message
;;                  "Duplicate entry 'test@axion5.net' for key 'index_users_on_email'",
;;                  :context nil,
;;                  :mutation "save-record",
;;                  :params
;;                  {:table :user,
;;                   :id 2,
;;                   :query [:updated-at :created-at :password-expires-at],
;;                   :mods {:email "test@axion5.net"}}})


(defmethod post-remote :default [mutate-symbol state result]
  (timbre/info "no post-remote for " mutate-symbol))

(defn post-save-record [{:keys [error]
                         {:keys [keys]} :value
                         {{:keys [query table id] :as params} :params} :post-remote
                         :as result}]
  ;; (timbre/info :#r "post-remote-record for save-record")
  ;; (timbre/info result)
  (let [id (id->real-id id)
        method (if (om/tempid? id) :create :update)
        table-by-id (au/table->table-by-id table)
        result-map (if error
                     {:value (process-error error params)}
                     {:keys keys
                      :records-to-process [{:table table :id id
                                            :dissoc-key :is-dirty?
                                            :synced? true
                                            :reset-history? (boolean (seq query))}]})]
    ;; (timbre/info :#pp result-map)
    (cond-> result-map
      (some? error) (update-in [:table-data table-by-id id] assoc :is-dirty? :error)
      :else (assoc-in [:value :client/event-record-saved] {:table table :id id
                                                           :method method}))
    ))

(defmethod mutate 'admin/modify-record
  [{:keys [reconciler state]} _ params]
  {:action (fn []
             (modify-record reconciler state params is-dirty?))})


;;TODO replace with version in mutate_save_record.cljc
(defmethod post-remote 'admin/save-record [_ state value]
  (post-save-record value))

(defn save-record
  "Generic save record. Only sends props that have actually changed to
  the backend"
  [{:keys [state ast reconciler target remote-key]} {:keys [table record query]}]
  ;; If fields as listed in page-state.validate are invalidated,:invalidated-fields key of state of
  ;; this component is set to map of invalidated fields, and no save is sent to
  ;; server
  (let [validate (au/get-key-in-page-state @state table :validate)
        invalidated-fields (au/calc-invalidations record validate)]
    ;; (timbre/info "invalidated-fields" invalidated-fields)
    ;; (timbre/info :#pp record)
    (if (seq invalidated-fields)
      {:action (fn []
                 (au/set-key-in-page-state state table :invalidated-fields invalidated-fields))}
      (let [record-mods (calc-mods table record)
            id (:id record)
            app-state @state
            {:keys [app/page]} app-state
            mods (into {} (remove
                           (fn [[k _]]
                             (au/client-only-key? k))
                           record-mods))
            id (cond-> (:id record)
                 (om/tempid? id) (au/tempid->string))
            params {:table table
                    :id id
                    :query query
                    :mods mods}
            remote (when (seq mods) (assoc ast :params params))]
        (au/set-key-in-page-state state table :invalidated-fields nil)
        {remote-key remote
         :post-remote {;; :params {:foo :bar}
                       :param-keys [:id :table :query]}
         :action (fn []

                   (if remote
                     (modify-record reconciler state {:table table
                                                      :id (:id record)
                                                      :no-history-entry? true}
                                    (constantly :saving))
                     (modify-record reconciler state {:table table
                                                      :id (:id record)
                                                      :no-history-entry? true}
                                    is-dirty?)))}))))

(defmethod mutate 'admin/set-is-dirty-to-saving
  [{:keys [reconciler state]} _ {:keys [table record]}]
  {:action (fn []
             (modify-record reconciler state {:table table
                                              :id (:id record)
                                              :no-history-entry? true}
                            (constantly :saving)))})


(defmethod mutate 'admin/set-is-dirty-to-error
  [{:keys [reconciler state]} _ {:keys [table record]}]
  {:action (fn []
             (modify-record reconciler state {:table table
                                              :id (:id record)
                                              :no-history-entry? true}
                            (constantly :error)))})

(defmethod mutate 'admin/set-is-dirty
  [{:keys [reconciler state]} _ {:keys [table record]}]
  {:action (fn []
             (modify-record reconciler state {:table table
                                              :id (:id record)
                                              :no-history-entry? true}
                            is-dirty?))})

;;TODO replace with version in mutate_save_record.cljc
(defmethod mutate 'admin/save-record
  [env _ params]
  (save-record env params))

(defmethod mutate 'admin/set-query-key
  [{:keys [state component]} _ {:keys [key path value] :as params}]
  {:action (fn []
             (let [{:keys [om-path]} (meta (om/props component))
                   path (if key [key] path)]
               (swap! state assoc-in (into om-path path) value)))})

(defmethod mutate 'admin/update-query-key
  [{:keys [state component]} _ {:keys [key path update-fn] :as params}]
  {:action (fn []
             (let [{:keys [om-path]} (meta (om/props component))
                   path (if key [key] path)]
               (swap! state update-in (into om-path path) update-fn)))})

(defmethod mutate 'admin/set-params
  [{:keys [state]} _ params]
  (let [{:keys [app/page]} @state
        params-path [:client/page-state page :params]]
    {:action (fn []
               (swap! state update-in params-path merge params))}))

(defmethod mutate 'admin/set-params-for-route
  [{:keys [state]} _ {:keys [params route]}]
  (let [namespace (namespace route)
        route (if (= namespace "route") route (keyword (str "route/" (name route))))
        params-path [:client/page-state route :params]]
    {:action (fn []
               (swap! state update-in params-path merge params))}))

(defmethod mutate 'admin/revert-record
  [{:keys [reconciler state] :as env} _ {:keys [table record ignore-tempid]}]
  {:action (fn []
             (if-let [reverted-record (:record (meta record))]
               (modify-record reconciler state {:id (:id record)
                                                :table table
                                                :ignore-tempid ignore-tempid
                                                :mods reverted-record
                                                :replace? true}
                              is-dirty?)))})

(defmethod mutate 'admin/undo-redo-record
  ;; Retrieves state from history by the next or prev uuid as recorded
  ;; in the meta data of the record at [table/by-id id]. It then
  ;; swaps in the record.
  [{:keys [reconciler state]} _ {:keys [table id undo-or-redo is-dirty-fn ignore-tempid]
                                 :or {is-dirty-fn is-dirty?}}]
  {:action
   (fn []
     (let [app-state @state
           next-or-prev-uuid (if (= undo-or-redo :undo) :prev-uuid :next-uuid)
           table-by-id (au/table->table-by-id table)
           record (get-in app-state [table-by-id id])
           other-uuid (get (meta record) next-or-prev-uuid)]
       (when (some? other-uuid) ;;really shouldn't call this fn if this is nil
         (let [other-state (om/from-history reconciler other-uuid)
               other-record (get-in other-state [table-by-id id])
               uuid-trail (or (:uuid-trail (meta record)) [])
               other-record (-> (condp = undo-or-redo
                                  :redo (let [uuid-trail (pop uuid-trail)
                                              next-uuid (last uuid-trail)]
                                          (vary-meta other-record assoc
                                                     :prev-uuid (last (au/mutation-history reconciler))
                                                     :next-uuid next-uuid
                                                     :uuid-trail uuid-trail))
                                  :undo (let [next-uuid (or (:uuid (meta record))
                                                            (last (au/mutation-history reconciler)))]
                                          (vary-meta other-record assoc
                                                     :next-uuid next-uuid
                                                     :uuid-trail (conj uuid-trail next-uuid))))
                                ;; Set the meta record so we can call is-dirty-fn
                                (vary-meta assoc
                                           :uuid other-uuid
                                           ;; A redo or undo away from a clean record
                                           ;; needs to set the meta record
                                           :record (or (:record (meta record)) record)))
               ;; Some overlap with modify-record here, but calling modify-record
               ;; couples it too closely perhaps.
               is-dirty? (is-dirty-fn table other-record)
               ;; Set is-dirty? on record, and remove meta record if not is-dirty?
               other-record (cond-> (assoc other-record :is-dirty? is-dirty?)
                              (not is-dirty?) (vary-meta dissoc :record))]

           ;; Swap the record in app state for the other record
           (swap! state assoc-in [table-by-id id] other-record)
           (timbre/info "other-record" other-record)

           ;; Update the :client/dirty-record map in app state
           (update-unsaved-records state {:table table
                                          :id id
                                          ;;We might want to ignore that a
                                          ;;record is a new record for the
                                          ;;purposes of updating unsaved record.
                                          :add-or-remove (if (or (and (not ignore-tempid) (om/tempid? id))
                                                                 is-dirty?)
                                                           :add :remove)})))

       ;; Debug;
       ;; (info "other record:" other-record)
       ;; (info :#g is-dirty?)
       ;; (info "in app-state:" (get-in @state [table-by-id id]))
       ;; (info "meta:")
       ;; (pprint (meta other-record))
       ;; (info (calc-mods table other-record))
       ;; Debug;

       record))})


(defn new-record
  [{:keys [reconciler state] :as env} {:keys [table mods]}]
  {:action (fn []
             (let [id (om/tempid)
                   ident [(au/table->table-by-id table) id]]
               (modify-record reconciler state
                              {:table table :id id :no-history-entry? true
                               :mods (merge mods {:id id })}
                              (constantly false))
               (au/update-page-state state table
                                  (fn [{:keys [new-records] :as table-state }]
                                    (merge table-state {:selected-id id
                                                        :invalidated-fields nil
                                                        :new-records (into [] (cons ident new-records))})))
               id))}) ;return id for testing

(defmethod mutate 'admin/new-record
  [{:keys [state] :as env} _ {:keys [table current-user] :as params}]
  ;;TODO: merge template-user : remove from aum-next!!!!
  (let [all-mods (condp = table
                   ;; If a group is selected set group id of user to that group
                   :user (let [group-id  (if (= (:role current-user) "group-admin")
                                           (:group-id current-user)
                                           (au/get-key-in-page-state @state :group :selected-id))]
                           {:mods (merge {:deleted false}
                                         (when group-id
                                           {:group-id group-id
                                            :group    [[:group/by-id group-id]]}))}

                           )
                   :job-offer {:mods {:creator-id (:id current-user)
                                      :published false
                                      :title "<No value>"
                                      :department-id 4
                                      :user [:user/by-id (:id current-user)]
                                      }}
                   nil)]
    (new-record env (merge params all-mods))))

(defn post-remote-delete-record
  [{:keys [error keys]
    {{:keys [table id] :as params} :params} :post-remote
    :as result}]
  (let [id (id->real-id id)]
    (if error
      {:keys keys
       :records-to-process [{:id id :table table
                             ;; This key normally is not existant, so we need
                             ;; to remove it all together to keep the
                             ;; is-dirty? mechanism working properly
                             :dissoc-key :client-prop/deleted
                             :recalc-is-dirty? true}]
       :value {:client/warning (assoc error
                                      :message "Not allowed to delete this record(s)"
                                      :server-message (:message error)
                                      :mutation "delete-record"
                                      :params params)}}
      {:keys keys
       :records-to-process [{:id id :table table
                             :reset-history? true
                             :synced? true
                             :dissoc-key :is-dirty?}]})))

(defmethod post-remote 'admin/delete-record [_ state result]
  (post-remote-delete-record result))

(defn delete-record
  [{:keys [reconciler state remote-key] :as env} {:keys [table id] :as params}]
  (let [new-record? (om/tempid? id)]
    {remote-key (not new-record?)
     :post-remote {:param-keys [:table :id]}
     :action (fn []
               (modify-record reconciler state {:table table :id id
                                                :no-history-entry? true
                                                :mods {:client-prop/deleted true}})
               (when new-record?
                 (update-unsaved-records state {:table table :id id :add-or-remove :remove})))}))

(defmethod mutate 'admin/delete-record
  [env _ params]
  (delete-record env params))

(defmethod mutate 'admin/set-filter-params
  [{:keys [state]} _ {:keys [table filter-params]}]
  {:action #(au/reset-item-batch state table {:filter-params  filter-params
                                              :page 1
                                              :cached-unsaved-records (unsaved-records->cached-unsaved-records @state table)})})
