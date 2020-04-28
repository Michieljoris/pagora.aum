(ns pagora.aum.frontend.parser.mutate-page-route)

;; (defmethod mutate 'admin/set-key-in-page-state
;;   [{:keys [state] :as env} _ {:keys [path key value]}]
;;   {:action (fn []
;;              (au/set-key-in-page-state state path key value))})


;; (defmethod mutate 'admin/set-route-to-fetch
;;   [{:keys [state]} _ {:keys [route]}]
;;   {:action (fn [] (swap! state assoc route :fetch))})

;; (defmethod mutate 'admin/set-page-to-fetch
;;   [{:keys [state]} _ {:keys [page]}]
;;   {:action (fn [] (swap! state assoc page :fetch))})

;; (defmethod mutate 'admin/select-group
;;   [{:keys [state]} _ _]
;;   {:action (fn [] (swap! state update :client/show-group-select? not))})

;; (defmethod mutate 'change/page!
;;   [{:keys [state component config] :as env} _ {:keys [new-page ignore-unsaved-records?] :as params}]
;;   (let [namespace (namespace new-page)
;;         new-page (if (= namespace "route") new-page (keyword (str "route/" (name new-page))))]
;;     {:action (fn []
;;                (let [app-state @state
;;                      current-page (:app/page app-state)]
;;                  (when (not= current-page new-page)
;;                    (if (and (not ignore-unsaved-records?) (page-dirty? app-state current-page config))
;;                      (do
;;                        (swap! state assoc :client/suspended-page-change params)
;;                        :route-change-suspended)
;;                      (do
;;                        (timbre/info "Page is now:" new-page)
;;                        ;; A page might want to initialize app-state somewhat.
;;                        (if-let [init (config {:page new-page})]
;;                          (init env))

;;                        ;; Set storage to the route so on refresh we load the same page again
;;                        (s/local-set :current-route new-page)
;;                        (s/local-set :current-page new-page)
;;                        ;; This is all that's needed to change the route, this key is
;;                        ;; queried for in the root component, so first a parse will be
;;                        ;; done of the root query and then a rerender of the root
;;                        ;; component.
;;                        (swap! state assoc :app/page new-page)
;;                        :page-changed)))))}))


;;Sets selected-id and resets invalidated-fields for table in page-state.
;; (defmethod mutate 'admin/select-item
;;   [{:keys [state om-path config] :as env} _ {:keys [table id form-ident]}]
;;   {:action (fn []
;;              (let [{:keys [app/page]} @state
;;                    {:keys [on-selected-id-change]} (config {:state state :page page :table table})
;;                    table-by-id (au/table->table-by-id table)]
;;                (when (fn? on-selected-id-change)
;;                  (on-selected-id-change env {:id id :om-path om-path}))

;;                (swap! state assoc-in (conj form-ident table) [table-by-id id])
;;                (au/update-page-state state table #(merge % {:invalidated-fields nil
;;                                                            :selected-id        id}))))})



;; Test and demo mutation

;; ========================================
;; Item-batch mutations

;;Sets page according to how many items there are in cache, then resets item
;;batch for table in page state
;; (defmethod mutate 'admin/next-page
;;   [{:keys [path state query-root query ast targe config] :as env} _ {:keys [table]}]
;;   {:action (fn []
;;              (let [;; page (bu/get-key-in-page-state @state table :page)
;;                    {:keys [page-size] :as config} (config)
;;                    cache (au/get-key-in-page-state @state table :cache)]
;;                (let [page (+ 2 (quot (count cache) page-size))] ;starts at 1 and one page is in item-batch
;;                  (au/reset-item-batch state table {:page page}))))})

;; (defn unsaved-records->cached-unsaved-records [app-state table]
;;   (->> (get-in app-state [:client/unsaved-records table])
;;        (remove om/tempid?)
;;        (mapv #(vector (au/table->table-by-id table) %))))

;; (defmethod mutate 'admin/set-sort
;;   [{:keys [state]} _ {:keys [table sort]}]
;;   {:action #(au/reset-item-batch state table {:sort sort
;;                                              :page 1
;;                                              :cached-unsaved-records (unsaved-records->cached-unsaved-records @state table)})})

;; (defmethod mutate 'admin/set-filters
;;   [{:keys [state]} _ {:keys [table search-term filter-map]}]
;;   {:action #(au/reset-item-batch state table {:filter-map filter-map
;;                                              :search-term search-term
;;                                              :page 1
;;                                              :cached-unsaved-records (unsaved-records->cached-unsaved-records @state table)})})


;; (defmethod mutate 'admin/reset-sort-filters
;;   [{:keys [state]} _ {:keys [table settings]}]
;;   {:action #(au/reset-item-batch state table (merge settings {:cached-unsaved-records (unsaved-records->cached-unsaved-records @state table)}))})
