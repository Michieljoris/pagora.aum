(ns pagora.aum.modules.events.frontend.components.record-history
  (:require
   [pagora.clj-utils.core :as cu]
   [cuerdas.core :as str]
   [clojure.set :as set]
   [pagora.aum.util :as au]
   [sablono.core :as html :refer-macros [html]]
   [pagora.aum.om.next :as om]
   ;; [common.i18n :refer [translate]]
   [taoensso.timbre :as timbre]
   [clojure.pprint :refer [pprint]]

   ;;TODO-aum
   ;; [cljs-react-material-ui.core :as ui]
   ;; [components.misc :refer [spinner spinner-markup]]
   ))

;;TODO-aum
(defn translate [_ _])

(def event-store-props [:id :name :entity-type :entity-id :sequence-no
                        :data :created-at :user-id {:event-user [:id :name]}])

(def event-store-query
  {:event-store event-store-props})

(defn event-query-for-table [table]
  `(~event-store-query
    {:order-by [[:id :desc]],
     :limit {:count 40}
     :where [:entity-type := ~(name table)]}))

(def event-query
  {:event event-store-props})

(defn event-query-for-record [table page page-size]
  (let [page-size (or page-size 20)]
    `(~event-query
      {:order-by [[:sequence-no :desc]]
       :limit ~{:count page-size :offset (* (or page 0) page-size)
                :from-single-entity true}
       :where [:entity-type := ~(name table)]})))

(defn event-query-for-prop [table cols]
  (let [json-prop #(str "%\"" % "\":%")
        make-like-clause #(vector :data :like %)
        xf (comp (map name)
                 (map cu/hyphen->underscore)
                 (map json-prop)
                 (map make-like-clause))
        like-clauses [:or (into [] xf cols)]]
    `(~event-query
      {:order-by [[:sequence-no :desc]]
       :where [:and [[:entity-type := ~(name table)]
                     ~like-clauses]]})))

(defn pprint-json [t locale cols json]
  (let [data  nil ;;(cu/json->clj json) ;;TODO-aum
        data (cu/transform-keys (comp keyword cu/underscore->hyphen name) data)
        props (keys data)
        props (sort props)
        cols (or cols props)
        cols (set/intersection (set cols) (set props))]

    (if (seq cols)
      (into [:div]
            (mapv (fn [k]
                    (let [v (get data k)]
                      (let [k-str (str/capital (str/replace (name k) "_" " "))
                            v (cond
                                (boolean? v) (str v)
                                (contains? #{:created_at :updated_at} k)
                                (cu/from-now t v (:locale locale))
                                ;; (sequential? v) (with-out-str (pprint v))
                                :else (with-out-str (pprint v)))
                            ]
                        [:div
                         [:span {:style {:padding-right 3
                                         :font-weight "bold"}} k-str] ": "
                         [:span {:style {:overflow-wrap "break-word"}} v]])))
                  cols))
      json)))

(defn history-row [this {:keys [event t locale cols]}]
  (let [{:keys [entity-type entity-id name sequence-no data created-at user-id event-user]} event
        event-type (cond
                     (str/includes? name "created") (str (t "was") " "  (t "created"))
                     (str/includes? name "modified") (str (t "was") " " (t "updated"))
                     :else (str ", " (str/replace name "-" " ")))
        created-at (cu/from-now t created-at (:locale locale))]
    [:div
     [:span (str (str/capital entity-type) " " (t "with id") " " entity-id " ")]
     [:span {:style {:font-style "italic"}} event-type]
     [:span (str
             ", " (str/lower created-at)
             (str " " (t "by") " "))]
     [:span {:style {:font-weight "bold"
                     :margin-right 3}} (or (:name event-user)
                                           (and user-id (str (t "user with id") " " user-id))
                                           (t "unknown user"))]
     [:span ":"]
     [:div {:style {:border "1px solid lightgrey"
                    :border-radius 3
                    :padding 5
                    :margin-top 5
                    :background-color "white"}}
      (pprint-json t locale cols data)
      ]]))

(defn history-block [this {:keys [events cols]}]
  (let [{:keys [locale]} (om/get-computed this)
        t (partial translate locale)]
    [:div {:style {:background-color "#f9f9f9"
                   :padding 10
                   }}
     ;; [:div {:style {:padding-bottom 10
     ;;                :font-weight "bold"
     ;;                :font-size 20}} (t "History")]
     (into [:div]
           (interpose [:div  {:style {:padding 5}}]
                      (mapv #(history-row this {:event %
                                                :cols cols
                                                :t t :locale locale})
                            events)))]))

(defn history-button [this {:keys [table record help fetch-events events left-or-right cols page-size]}]
  (let [{:keys [page]} (om/get-state this)
        {:keys [locale]} (om/get-computed this)
        t (partial translate locale)
        cached-events (or (:event (:cache (om/props this) [])))
        all-events (concat cached-events events)]

    [:div {:style {:font-size 15}}
     ;;TODO-aum
     ;; (ui/icon-button {:on-click (fn []
     ;;                              (om/update-state! this update :side-drawer-open? not)
     ;;                              (case fetch-events
     ;;                                :record-type-events
     ;;                                (om/transact! this `[(admin/set-params
     ;;                                                      ~{:event-store-params
     ;;                                                        {:order-by [[:id :desc]],
     ;;                                                         :limit {:count 40}
     ;;                                                         :where [:entity-type := (name table)]}})
     ;;                                                     :event-store])
     ;;                                :record-events
     ;;                                (do
     ;;                                  (om/update-state! this assoc :page 0)
     ;;                                  (om/transact! this `[(admin/clear-key {:key :load-data})
     ;;                                                       (admin/set-query-key {:key :cache :value nil})
     ;;                                                       {:load-data
     ;;                                                        [({~table
     ;;                                                           [:id ~(event-query-for-record table page page-size)]}
     ;;                                                          {:where [:id := ~(:id record)]})]}]))
     ;;                                :record-prop-events
     ;;                                (om/transact! this `[ (admin/clear-key {:key :load-data})
     ;;                                                     {:load-data
     ;;                                                      [({~table
     ;;                                                         [:id ~(event-query-for-prop table cols)]}
     ;;                                                        {:where [:id := ~(:id record)]})]}])

     ;;                                ))
     ;;                  :title help}
     ;;                 (ui/font-icon {:class-name "icon-clock-o"}))
     (let [open? (:side-drawer-open? (om/get-state this))]
       ;; (ui/drawer
       ;;  {:docked false
       ;;   :width (if (= left-or-right :left)
       ;;            "32%" "68%")
       ;;   :open-secondary (= left-or-right :right)
       ;;   :open open?
       ;;   :on-request-change #(om/update-state! this assoc :side-drawer-open? false)}
       ;;  ;; (timbre/info :#pp {:events events
       ;;  ;;                    :cached-events cached-events})

       ;;  (when open?
       ;;    (html
       ;;     (if (or (seq events) (seq cached-events))
       ;;       [:div
       ;;        (if (seq all-events)
       ;;          (history-block this {:events all-events :cols cols})
       ;;          (t "No result"))
       ;;        [:div {:style {:margin-bottom 30 :float "right" :margin-top 10 :margin-right 10}}
       ;;         (when (and
       ;;                (not (zero? (count events)))
       ;;                (zero? (rem (count events) page-size)))
       ;;           (if (sequential? events)
       ;;             (ui/flat-button
       ;;              {:label (t "More..")
       ;;               :on-click (fn []
       ;;                           (let [page (inc page)]
       ;;                             (om/update-state! this assoc :page page)
       ;;                             (om/transact! this `[(admin/clear-key {:key :load-data})
       ;;                                                  (admin/cache-records
       ;;                                                   {:get-records
       ;;                                                    ~(fn [app-state]
       ;;                                                       (get-in app-state [(au/table->table-by-id table)
       ;;                                                                          (:id record) :event]))
       ;;                                                    :update-cache
       ;;                                                    ~(fn [state om-path records]
       ;;                                                       (swap! state assoc-in [(au/table->table-by-id table)
       ;;                                                                              (:id record) :event] nil)
       ;;                                                       (swap! state update-in (conj om-path :cache :event) #(into (or % []) records))
       ;;                                                       )
       ;;                                                    })
       ;;                                                  {:load-data
       ;;                                                   [({~table
       ;;                                                      [:id ~(event-query-for-record table page page-size)]}
       ;;                                                     {:where [:id := ~(:id record)]})]}])))})
       ;;             (spinner-markup)))]]
       ;;       (spinner nil)))))
       )]))
