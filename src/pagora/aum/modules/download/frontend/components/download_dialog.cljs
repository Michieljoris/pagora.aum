(ns pagora.aum.modules.download.frontend.components.download-dialog
  (:require
   [pagora.aum.om.next :as om]
   ;; [common.i18n :refer [translate]]
   [cuerdas.core :as str]
   [pagora.aum.om.util :as om-util]
   ;; [app.storage :as s]
   ;; [cljs-react-material-ui.core :as ui]
   [pagora.aum.modules.download.frontend.core :as download]
   [sablono.core :as html :refer-macros [html]]
   [taoensso.timbre :as timbre]
   ;; [components.hover-input :refer [hover-input]]
   ;; [components.css-variables :as css]

   ))
(defn translate [_ _ _])

  ;;TODO
  ;;- Custom names for columns!!!

  (def download-count-max 1000)

;; (defn download-dialog [this {:keys [form-class table download-columns-hide
;;                                     download-dialog-title extra-columns]}]
;;     (let [handle-close (fn []
;;                          (om/update-state! this assoc :show-download-dialog? false))
;;           t (partial translate (:locale (om/get-computed this)))
;;           {:keys [client/page-state] :as props} (om/props this)
;;           {:keys [ sort filter-map search-term filter-params]
;;            {:keys [filters sorts]} :filter-search-sort-config} page-state
;;           filters (into {}
;;                         (mapv (fn [[k filter-config]]
;;                                 [k (if (fn? filter-config)
;;                                      (filter-config filter-params)
;;                                      filter-config)])
;;                               filters))
;;           _ (timbre/info :#pp {:filters filters
;;                                :filter-params filter-params})

;;           active-filters-string (str/join ", "
;;                                           (->> filter-map
;;                                                (filter (fn [[_ v]] v))
;;                                                (map (fn [[k _]]
;;                                                       (let [{:keys [label label-str]} (get filters k)]
;;                                                         (if label-str
;;                                                           (label-str this {:t t
;;                                                                            :filter-params filter-params})
;;                                                           (t label)))))))
;;           default-filename (str (t (name table))
;;                                 (when (seq search-term) (str "_" (t "search") " " search-term))
;;                                 (when (> (count active-filters-string) 0)
;;                                   (str "_" active-filters-string))
;;                                 "_" (t "sorted by") " " (str/lower (t (:label (get sorts sort)))))

;;           total-item-count (get-in props [:item-batch table :meta :count])
;;           form-query (->> (om/get-query form-class)
;;                           (some #(when (= (om-util/join-key %) table)
;;                                    (om-util/join-value %)))
;;                           (filterv #(not (or (not (keyword? %))
;;                                              (str/starts-with? "client" (namespace %))
;;                                              (= :is-dirty? %)
;;                                              (contains? (set download-columns-hide) %)))))
;;           form-query (into form-query extra-columns)
;;           ;;NOTE: Bit cumbersome to do it here iso putting more functionality in
;;           ;;init-local-state, but it makes this download functionality kinda
;;           ;;modular and contained in this fn. Getting local storage every time is speedy enough.
;;           cmp-columns-inclusion-state (:download-columns-inclusion-state (om/get-state this))
;;           persisted-columns-inclusion-state (s/local-get :download-columns)
;;           persisted-columns-inclusion-state-by-table (get persisted-columns-inclusion-state table)
;;           columns-inclusion-state (->> form-query
;;                                        (reduce (fn [acc column]
;;                                                  (let [cmp-state (get cmp-columns-inclusion-state column)
;;                                                        persisted-state (get persisted-columns-inclusion-state-by-table column)
;;                                                        state (cond
;;                                                                (boolean? persisted-state) persisted-state
;;                                                                (boolean? cmp-state) cmp-state
;;                                                                :else true)]
;;                                                    (assoc acc column state)))
;;                                                {}))
;;           columns-inclusion-state (assoc columns-inclusion-state sort true)
;;           ;; _ (timbre/info :#pp persisted-columns-inclusion-state)

;;           make-checkbox (fn [column]
;;                           (ui/checkbox {:checked (boolean (get columns-inclusion-state column))
;;                                         :label (name column)
;;                                         :disabled (= column sort)
;;                                         :on-click #(let [new-state (not (get columns-inclusion-state column))]
;;                                                      (om/update-state! this assoc-in [:download-columns  column] new-state) ;;trigger cmp update
;;                                                      (s/local-set :download-columns (assoc-in persisted-columns-inclusion-state
;;                                                                                               [table column] new-state)))}))
;;           query-columns (->> form-query
;;                              (filterv (fn [k] (get columns-inclusion-state k))))

;;           {:keys [download-status filename are-you-sure download-type!]} (om/get-state this)
;;           filename (if (or (nil? filename) (str/blank? filename))
;;                      default-filename filename)
;;           download! (fn [type]
;;                       (om/update-state! this assoc
;;                                         :download-status :starting
;;                                         :type type)
;;                       (download/download this {:filename filename
;;                                                :columns query-columns
;;                                                :on-progress (fn [info]
;;                                                               (om/update-state! this assoc :download-status info))
;;                                                :on-error (fn [info]
;;                                                            (timbre/info :#pp info))
;;                                                :type type})
;;                       ;; (handle-close)
;;                       )
;;           make-button (fn [type label]
;;                         (ui/raised-button
;;                          {:label  (str/capital label)
;;                           :style {:margin-right 10}
;;                           :primary true
;;                           :disabled (not-any? true? (vals columns-inclusion-state))
;;                           :on-click #(do
;;                                        (if (> total-item-count download-count-max)
;;                                          (om/update-state! this assoc :are-you-sure true
;;                                                            :download-type! (partial download! type))
;;                                          (download! type)))}))
;;           download-status (case (:status download-status)
;;                             :fetching-data (str "0/" total-item-count)
;;                             :fetched-chunk (str (:fetched download-status) "/" (:total download-status))
;;                             :creating-file (t "Creating " (name (:type (om/get-state this))))
;;                             :error (str "(" (:error-msg download-status) ")")
;;                             (str "(" total-item-count ")"))]

;;       (ui/dialog
;;        {:title  (str (t download-dialog-title) " " download-status)
;;         :modal false
;;         :open (boolean (:show-download-dialog? (om/get-state this)))
;;         :on-request-close handle-close
;;         :actions [(ui/flat-button
;;                    {:label  (t "Close")
;;                     :primary true
;;                     :on-click handle-close})
;;                   ]}
;;        ;; (timbre/info :#pp {:download-status (:download-status (om/get-state this))})

;;        (html
;;         (if are-you-sure
;;           [:div
;;            [:div {:style {:color "dimgray"} :class "pad-bot-10"}
;;             (t :download-limit-warning)]
;;            [:div
;;             (ui/raised-button
;;              {:label (t "Yes")
;;               :style {:margin-right 10}
;;               :primary true
;;               :on-click (fn []
;;                           (om/update-state! this assoc :are-you-sure nil)
;;                           (download-type!))})
;;             (ui/raised-button
;;              {:label (t "Cancel")
;;               :style {:margin-right 10}
;;               :primary true
;;               :on-click (fn []
;;                           (om/update-state! this assoc :are-you-sure nil))})]]
;;           [:div
;;            [:div {:style {:height 40 :font-size 18} :class "row"}
;;             [:div {:class "col-xs-2" :style {:color "dimgrey"}} (t "File name")]
;;             [:div {:class "col-xs-10"}
;;              (hover-input {:id "search-input"
;;                            :debounce-interval 300
;;                            :locale (:locale (om/get-computed this))
;;                            :value (or (:file-name (om/get-state this)) "")
;;                            :input-style {:fontFamily (:font-regular css/vars)
;;                                          :padding-bottom 3
;;                                          :margin-left 10}
;;                            :placeholder default-filename
;;                            :clear-button-on-hover true
;;                            :allow-empty-input true
;;                            :keep-focus-on-enter true
;;                            ;; :on-clear #(om/update-state! this assoc :filename nil)
;;                            ;; :on-change on-change
;;                            :callback (fn [value]
;;                                        (om/update-state! this assoc :filename value))})]]
;;            [:div
;;             (make-button :csv (t "Download csv"))]
;;            [:div {:class "pad-top-10"} (t "Included columns:")]
;;            (into [:div]
;;                  (mapv #(make-checkbox %) form-query))]))))

;;     )
