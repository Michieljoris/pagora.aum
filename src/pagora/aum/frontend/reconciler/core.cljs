(ns pagora.aum.frontend.reconciler.core
  (:require-macros
   [cljs.core.async.macros :refer [go]]
   [pagora.clj-utils.macros :refer [if-let*]])
  (:require [pagora.aum.frontend.reconciler.migrate :as aum-migrate]
            [pagora.aum.om.next :as om]
            [cljs.core.async :as async :refer [<! alts! chan put!]]
            [pagora.aum.om.next.protocols :as om-next-protocols]
            [pagora.aum.om.util :refer [mutation?]]
            [taoensso.timbre :as timbre]
            [pagora.aum.util :refer [om-deep-merge table->table-by-id]]
            [pagora.aum.frontend.reconciler.debug :as d]
            [pagora.clj-utils.core :as cu]
            [pagora.aum.frontend.parser.calc-mods :refer [calc-mods]]
            [pagora.aum.frontend.parser.mutate :as mutate]
            [pagora.aum.frontend.parser.mutate-helpers
             :refer [is-dirty? modify-record update-unsaved-records]]))

(defn call-post-remote-hooks
  "Calls post-remote hook for target for every mutation in mutations
  with :value of response and deep merges result back into response."
  [{:keys [target] :as om-request} reconciler
   {:keys [value] :as response}]
  (let [;; mutations (into {} (filter #(symbol? (first %)) value))
        {queries false mutations true} (group-by #(symbol? (first %)) value)
        mutations (into {} mutations)
        queries (into {} queries)]
    ;; (timbre/info "MUTATIONS" mutations)
    ;; (timbre/info "QUERIES" queries)
    ;; (timbre/info :#pp {:target target :mutations mutations :response response})
    (->> mutations
         (reduce (fn [response [mutation-symbol mutation-result]]
                   (let [mutation-results (if (sequential? mutation-result) mutation-result [mutation-result])]
                     ;; (timbre/info "mutation-results" mutation-results)
                     (->> mutation-results
                          (reduce (fn [response {:keys [post-remote] :as mutation-result}]
                                    (let [mutation-symbol (or (get-in mutation-result [:post-remote :redirect])
                                                                mutation-symbol)]
                                      ;; (timbre/info :#g "post-remote" post-remote mutation-symbol)
                                      ;;post-remote gets reconciler, and the mutation-result
                                      ;;The mutation-result is a map with some or all of these keys:
                                      ;;[:tempids, keys, error, created, updated]
                                      ;;The post-remote method should return a response map with some or all of the following keys:
                                      ;;[status, value, table-data, keys, records-to-process]
                                      ;;value and table-data will get deep-merged with the app-state
                                      ;;TODO: concat the :keys values
                                      (cu/deep-merge-concat-vectors response (mutate/post-remote mutation-symbol reconciler mutation-result)))
                                    ;; (cond-> response
                                    ;;   (or (some? post-remote))
                                    ;;   (deep-merge (mutate/post-remote mutation-symbol reconciler mutation-result)))
                                    )
                                  response ))))
                 (assoc response :value queries)))))

(defn dirty-record-mods
  "Transforming unsaved-records from a mapping of table to a set of
  dirty record ids to a mapping of tables to ids to mods. Returns a
  kind of table data with only the mods"
  [app-state calc-mods]
  (let [{:keys [client/unsaved-records]} app-state]
    (reduce (fn [unsaved-records [table ids]]
              (merge unsaved-records
                     {table             ;by table, not table-by-id!
                      (reduce (fn [m id]
                                (if-let* [record (get-in app-state [(table->table-by-id table) id])
                                          mods (calc-mods table record)]
                                  (merge m {id mods})
                                  m))
                              {} (remove om/tempid? ids))}))
            {} unsaved-records)))

(defn restore-mods-params
  "Returns params to pass to modify record to restore mods-to-restore"
  [table-data mods-to-restore]
  (reduce (fn [v [table mods-by-id]]
            (let [table-by-id (table->table-by-id table)
                  data-by-id (get table-data table-by-id)
                  mods-by-id (select-keys mods-by-id (keys data-by-id))]
              (into v (reduce (fn [v [id mods]]
                                (conj v {:id id :table table
                                         ;; meta-mods merge with the meta record, if it exists
                                         ;; This resets the meta to whatever record is after
                                         ;; the merge of data from the server
                                         :meta-mods (get-in table-data [table-by-id id])
                                         ;; mods only get applied to the record itself
                                         :mods mods }))
                              [] mods-by-id))))
          [] mods-to-restore))

(defn restore-mods
  "Reapplies user made changes."
  [reconciler state table-data mods-to-restore]
  (doseq [params (restore-mods-params table-data mods-to-restore)]
    (modify-record reconciler state (assoc params :no-history-entry? true) is-dirty?)))

;; This is a mechanism to have some control over app state -after- a mutation
;; has gone to a remote and response has come back.A map with any of :status :value
;; :table-data :keys and :records-to-process can be returned from a post-remote hook in
;; a mutation. Value and table-date will be deep merged with app-state. Keys
;; will be given to om next to queue and records-to-process is processed by
;; this function.
(defn process-records
  "Every record should have a :table and :id key. State for that
  record is modified according to the various flags set"
  [reconciler state records]
  (when (seq records)
    (timbre/info "Processing records:" records))
  (doseq [{:keys [id table dissoc-key] :as record} records]
    (let [record-ident [(table->table-by-id table) id]
          flags (select-keys record [:dissoc-key :recalc-is-dirty? :synced? :reset-history?])
          true-flags (keys (filter (fn [[k v]] v) flags))]
      (doseq [flag true-flags]
        (case flag
          ;; Server can return extra fields on a save as set by the :query param.
          ;; This will have been merged by now by the om-callback. However since
          ;; record changed (likely) we need to reset history (for saves that had
          ;; a query). like we do in modify-record, TODO: have server return query
          ;; data under a separate key, then use modify-record to modify app-state
          :reset-history? (swap! state update-in record-ident
                                 (fn [record]
                                   (vary-meta record dissoc :prev-uuid :next-uuid :uuid :uuid-trail)))

          ;; On a successful save we need to update the meta record since that
          ;; always should be nil if there are no mods
          :synced? (swap! state update-in record-ident vary-meta dissoc :record)
          :dissoc-key (swap! state update-in record-ident dissoc dissoc-key)
          :recalc-is-dirty? (modify-record reconciler state
                                           {:id id :table table :mods {}
                                            :no-history-entry? true} is-dirty?)
          )))))

;;Handles response for -one- om-request
(defn handle-success-response
  "Merges any table-data in response with the value in response Sets
  client/error is status != :ok and is not already set. Updates
  client/unsaved-records if there are any successfully saved records.
  Call om-callback to merge :value, but preserves any dirty record
  mods. Updates records depending on any post mutation action to be
  taken, and finally queues keys to rerender "
  ([reconciler om-request response]
   (handle-success-response reconciler om-request response {}))
  ([reconciler {:keys [om-callback state] :as om-request} response {:keys [value-merge-hook]}]
   (let [state (or state (om/app-state reconciler))
         ;; _ (timbre/info :#r "Calling post-remote-hooks")
         ;; call-post-remote-hooks is able to response
         ;; for instance setting :is-dirty? to false in
         ;; table-data for some table on successful save
         ;; This calls any post-remote hooks for mutations
         response (call-post-remote-hooks om-request reconciler response)

         ;;value is a mergable app state map. However, any root keys that are
         ;;symbols have as value result map of mutation.
         ;;An example return map for save-dossier-type is:
         ;; {:tempids nil,
         ;;  :keys [:dossier-type],
         ;;  :post-remote
         ;;  {:params
         ;;   {:id "1",
         ;;    :query [:updated-at :created-at {:last-updated-user [:name]}],
         ;;    :field-ids (3)}}}
         {:keys [status value table-data keys ;original keys of response map
                 records-to-process tempids] ;added keys
          } response

         value (merge value table-data)

         ;; When status != :ok, and value doesn't have a :client/error or
         ;; :client/warning key, assign the whole value to the error key. These
         ;; errors should be handled by call-post-remote-hooks as much as possible
         ;; though. This throws up a error dialog.
         value (cond-> value
                 (and (not= status :ok)
                      (and (nil? (:client/error value))
                           (nil? (:client/warning value))))
                 (assoc :client/error value))

         ;; To be able to send tempids over the wire I
         ;; transform them into uuid strings. This fixes it
         ;; TODO: use tempid reader macro on server side, that would make this step unnecessary.
         ;; This is done somewhere else now, see below.
         ;; value (aum-migrate/transform-server-response-uuid->tempid value) ;replace tempid strings with tempids
         ]

     (when (not= status :ok)
       (timbre/info :#r (or (:client/error value) (:client/warning value))))

     ;; Remove ids of clean (saved) records from :client/unsaved-records key in page state
     (timbre/info :#pp records-to-process)

     (timbre/info "Saved records" (filter :synced? records-to-process))
     (timbre/info "Removing saved records from unsaved-records map in app state")
     (update-unsaved-records state (map #(assoc % :add-or-remove :remove) (filter :synced? records-to-process)))

     ;; Let's remember what user has actually changed sofar, so we can apply
     ;; these changes again after we merged any (table-) data returned from the
     ;; server. This only stores mods of unsaved-records, so if they have been
     ;; removed (see update-unsaved-records call above) they won't be stored and
     ;; restored. This clobbering happens for instance when setting a filter or doing a
     ;; search. Table data (possibly partial) may come in and clobber existing
     ;; edits. Same will have to happen when we start accepting data pushed from
     ;; the server. Or add a button to let the user refresh for instance form
     ;; data from the server
     (let [mods-to-restore (dirty-record-mods @state calc-mods)]

       (timbre/info "Merging value:")
       (timbre/info :#cp value)

       ;;This also calls my om-deep-merge and migrate, it will
       ;;reset the meta of any records that are
       ;;migrating from a tempid to a db id

       (om-callback (cond-> value
                      value-merge-hook value-merge-hook))

       (process-records reconciler state records-to-process)

       ;;We've set migrate to nil in reconciler config, and not doing a
       ;;transform-server-response-uuid->tempid anymore. Server returns all
       ;;tempids generated from mutation under the one tempids key in the
       ;;response. So we call migrate on state and these tempids directly
       (reset! state (aum-migrate/migrate @state nil
                                            ;;Server still returns tempids as strings, but they're
                                            ;;still om-tempids in our app state
                                            (aum-migrate/string-tempids-mapping->tempids-mapping tempids)
                                            :id))

       ;;And restore any mods again
       (when (seq mods-to-restore)
         (timbre/info "Restoring mods and recalculating is-dirty? for all unsaved records")
         (timbre/info :#pp mods-to-restore)
         (restore-mods reconciler state table-data mods-to-restore)))



     (timbre/info "Queueing keys:" (into [(:app/page @state)
                                          ] keys))
     ;; TODO: queuing key for route for now. Perhaps could be smarter
     (into [(:app/page @state)
            ] keys))))

(defn handle-error-response
  "Just sets client/error key for now"
  [reconciler {:keys [query om-callback state mutations]} response options]
  (let [state (or state (om/app-state reconciler))]
    (timbre/info :#r "error sending query/mutation:" query)
    (timbre/info :#r "error response:" query)
    ;; TODO: don't leave users hanging here, connection is probably down, or
    ;; server websockets are buggered. Perhaps set some "connection down" marker,
    ;; eg big marker on app bar or something.
    ;; response can be :chsk/timeout, :chsk/closed
    (swap! state assoc :client/error
           {:error "Error sending query/mutation"
            :query query :response response})
    [(:app/page @state)]))

(defn handle-response
  [reconciler
   {:keys [target success? state] :as om-request}
   response
   {:keys [verbose? start-state queue-keys] :as options}]
  ;; (timbre/info :#r "Received response-------------------------------" response)
  (d/debug-response verbose? response target)
  (let [state (or state (om/app-state reconciler))
        keys-to-queue (if (success? response)
                        (handle-success-response reconciler om-request response options)
                        (handle-error-response reconciler om-request response options))]
    ;; Record state to reset app to for debugging
    ;; (when (nil? @start-state)
    ;;   (reset! start-state @state))
    (queue-keys reconciler keys-to-queue)
    (swap! state assoc :client/query-pending? false)))

(defn make-send-fn
  "This returns a send fn that can be given to a reconciler. It puts
  incoming queries on a queue"
  [{:keys [success? reads-queue mutations-queue]}]
  (fn [remote-queries om-callback]
    ;; (timbre/debug "Remotes put on queue")
    ;; (timbre/debug :#pp remote-queries)
    (doseq [[target query] remote-queries]
      (let [mutations (filter mutation? query)
            ;;Remove :post-remote-hooks key from all mutation params
            query (mapv (fn [q]
                          (if (and (mutation? q) (second q))
                            (list (first q) (dissoc (second q) :post-remote-hooks))
                            q))
                        query)]

        (let [queue (if (seq mutations)
                      mutations-queue
                      reads-queue)
              om-request {:query query
                          :target target
                          :success? success?
                          :om-callback om-callback}]
          (put! queue om-request))))))

(defn process-remote-queries
  "This is where the actual call to a server is made."
  [{:keys [reconciler reads-queue mutations-queue remotes
           handle-response]
    {:keys [verbose?] :as options} :options}]
  (go
    (loop [[{:keys [target query] :as om-request} channel] (alts! [mutations-queue reads-queue] :priority true)]
      ;; (swap! state assoc :client/query-pending? true)
      (if-let* [send-fn (get remotes target)
                send-blocker (chan)]
        (do
          (timbre/debug "target" target)
          (d/debug-send verbose? query target)
          (send-fn query
                   (fn [response]
                     (handle-response reconciler om-request response options)
                     (put! send-blocker :unblock!)))
          (d/info-send verbose? (str "sent msg to " target ", now waiting"))

          ;; Seemingly synchronous blocking code!!!
          ;; But these get transmorgified into a callback in javascript..
          (alts! [send-blocker (async/timeout 10000)])
                                        ;; blocks till response is received
          (d/info-send verbose? "Unblocked!!! Looping and waiting for next lot of send data"))
        (timbre/error "No server-send fn for target " target))
      (recur (alts! [mutations-queue reads-queue] :priority true)))))

(defn spy-parser [parser]
  (fn self
    ([env query] (self env query nil))
    ([env query target]
     (let [color (if target :#magenta :#b)]
       (timbre/info color ">> spy-parser is called, target is " target)
       (timbre/info "query:")
       (timbre/info :#pp query)
       (let [result (parser env query target)]
         (timbre/info color "-------------- Result of parse of :" query " with target " target)
         (timbre/info :#pp result)
         (timbre/info color "<<<<<<<< " target)
         result)))))

(defn make-reconciler [{:keys [success? state remotes spy-parser? verbose? start-state value-merge-hook
                               remotes logger parser]
                        :as reconciler-config
                        :or {start-state (atom nil)}}]
  (let [queues {:reads-queue (chan)
                :mutations-queue (chan)
                :send-blocker (chan)}
        remote-keywords (keys remotes)
        reconciler-config (merge {:pathopt true
                                  :merge-tree cu/deep-merge-concat-vectors
                                  ;; om-deep-merge
                                  :merge-ident cu/deep-merge-maps
                                  :migrate (fn [app-state _ _] app-state);;see above
                                  :id-key :id
                                  :normalize false
                                  :easy-reads true
                                  ;;The returned fn puts om-requests on queues
                                  :send (make-send-fn (merge queues {:success? success?}))
                                  :parser (if spy-parser? (spy-parser parser) parser)
                                  :remotes remote-keywords}
                                 (dissoc reconciler-config
                                         :remotes :success? :start-state :verbose?))
        ;;Bit more sane than default om.next behaviour. If logger is falsey than
        ;;don't include key. Om.next turns logger on if just key
        ;;exists (contains?), even when nil or false.
        ;; reconciler-config (cond-> reconciler-config
        ;;                     (not logger) (dissoc :logger))
        ;; _ (timbre/info "easy-reads:" (:easy-reads reconciler-config))
        reconciler (om/reconciler reconciler-config)]
    ;;This fn actually processes teh queues
    (process-remote-queries (merge queues {:reconciler reconciler
                                           :handle-response handle-response
                                           :remotes remotes
                                           :options {:value-merge-hook value-merge-hook
                                                     :start-state start-state
                                                     :queue-keys om-next-protocols/queue!
                                                     :verbose? verbose?}}))
    reconciler))


;; DEBUG ****************************************************************************************************
;; (do
;;   (defn test-parser [query]
;;     (let [state reconciler.app-state/app-state]

;;       (timbre/info :#p "DEBUG >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
;;       (timbre/info "Query:")
;;       (timbre/info :#g query)

;;       (timbre/info "Parsing result ")
;;       (let [result (parser {:state state}
;;                            query
;;                            ;;:remote
;;                       )]
;;         (pprint result)
;;         )

;;       (timbre/info :#p "DEBUG <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"))
;;     )
;;   (test-parser [:app/page]);; )
;;   ;; (test-parser '[(debug/bla {:some-param :foo})])
;;   )

;; TODO: alternatively for debugging/testing transact! on the reconciler (after
;; making one, so in a consuming repo).



;; The following processes responses in the order they've been sent, but better is to queue sends, and prioritize mutations
;; (def in (chan))
;; (def out (chan))
;; (def parallelism 1000)                     ;what does this do?

;; (defn wait-for-response-and-put [om-request ch]
;;   (let [{:keys [query om-callback]} om-request]
;;     (chsk-send! [:admin/query query] 5000
;;                 (fn [cb-reply]
;;                   (let [response {:query query :response cb-reply :om-callback om-callback}]
;;                     (cond
;;                       (cb-success? cb-reply) (put! ch response)
;;                       (cb-error? cb-reply) (put! ch (assoc response :error "chsk-send! timed out"))))
;;                   (close! ch)))))

;; (pipeline-async parallelism out wait-for-response-and-put in)

;; (go-loop [response (<! out)]
;;   (let [ {:keys [response om-callback error]} response]
;;     (if (nil? response)
;;       (info "This shouldn't happen!!!") ;only when in channel is closed.
;;       (do
;;         (condp = error
;;           nil (do
;;                 (info  "Response:" response)
;;                 (om-callback response)
;;                   )
;;           (do
;;             (error response)
;;             ))
;;         ;; (info  "Waiting for a response")
;;         (recur (<! out))))))


;; (defn send [remotes om-callback]
;;   (let [query (:remote remotes)]
;;     (info "Sending to remote: " query)
;;     (put! in {:query query :om-callback om-callback}))
;;   )
