(ns pagora.aum.modules.events.frontend.components.events-page
  ;; (:require ;; om/next
  ;;  [goog.dom :as gdom]
  ;;  [app.util :refer [fuzzy-filter]]
  ;;  [components.misc :refer [spinner-centered spinner spinner-markup]]
  ;;  [components.pagination :refer [pagination]]
  ;;  [om.next :as om :refer-macros [defui]]
  ;;  [app.storage :as s]
  ;;  [sablono.core :as html :refer-macros [html]]
  ;;  [app.environment :refer [is-development?]]
  ;;  [digicheck.common.util :as du]
  ;;  [components.record-history :as history]
  ;;  [common.i18n :refer [translate]]
  ;;  [goog.functions]
  ;;  ;; material-ui components
  ;;  [cljs-react-material-ui.core :as ui]

  ;;  ;; DC ember app css variables
  ;;  [components.css-variables :as css]

  ;;  [cljs.pprint :refer [pprint]]
  ;;  ;; Components

  ;;  ;; logging
  ;;  [taoensso.timbre :as timbre]
  ;;  [cuerdas.core :as str]
  ;;  [clojure.set :as set])
  (:import [goog.async Debouncer]
           goog.Delay
           )
  )

;; TODO pagination of events on sidebar
;; DONE: Fix up inconsistency between admin and dc-app for created-at, updated-at etc dates
;; TODO there's a bug where the backend doesn't retrieve the user join in an event.

;;TODO later
;;- Show history of field of a record (double click on label or something?,
;;hover over and show history icon?)
;;- Update history, if showing on updates. Just load events from a certain
;;sequence no, and add them to the list..




;; (defn format-date [locale some-moment]
;;   (let [some-moment (js/moment some-moment)]
;;     (.locale js/moment (name (:locale locale)))
;;     (cond
;;       (not (.isValid some-moment)) ""
;;       :else (.format some-moment "LL"))))

;; (defn om-data [this]
;;   {:props (dissoc (om/props this) :om.next/computed)
;;    :state (om/get-state this)
;;    :computed (om/get-computed this)})

;; (defn where [search-term props-to-search]
;;   [:or
;;    (mapv #(vector % :like (str "%" search-term "%")) props-to-search)])

;; (defn on-update-input [this {:keys [search-term set-params-kw table props-to-search]}]
;;   (when (> (count search-term) 0)
;;     (om/transact! this `[(admin/set-params
;;                           ~{set-params-kw {:where (where search-term props-to-search)
;;                                           :limit {:count 10}}})
;;                          ~table])))

;; (def on-update-input-debounced
;;   (du/debounce on-update-input 300))

;; ;; (defn make-cmp [cmp-class & [options]]
;; ;;   (let [factory (om/factory cmp-class options)]
;; ;;     (if is-development?
;; ;;       (fn [this k & [computed]]
;; ;;         (let [{:keys [props]
;; ;;                cmp-computed :computed} (om-data this)
;; ;;               {:keys [client/reload-key]} props
;; ;;               computed-reload-key (:reload-key cmp-computed)
;; ;;               computed (assoc computed :reload-key
;; ;;                               (or reload-key computed-reload-key))]
;; ;;           (factory (om/computed (get props k) computed))))
;; ;;       (fn [this k & [computed]]
;; ;;         (let [{:keys [props]} (om-data this)]
;; ;;           (factory (om/computed (get props k) computed)))))))

;; ;; (defn load-events [this {:keys [event-type entity-type entity-id user-id group-id from till data-search-term page page-size]
;; ;;                          :as params}]
;; ;;   (let [where (cond-> []
;; ;;                 (keyword? entity-type) (conj [:entity-type := (name entity-type)])
;; ;;                 (vector? entity-type) (conj [:entity-type :in (map name entity-type)])
;; ;;                 (number? entity-id) (conj [:entity-id := entity-id])
;; ;;                 (number? user-id) (conj [:user-id := user-id])
;; ;;                 (number? group-id) (conj [:group-id := group-id])
;; ;;                 (and (string? event-type)
;; ;;                      (seq event-type)) (conj [:name :like (str "%" event-type "%")])
;; ;;                 (keyword? event-type) (conj [:name := (name event-type)])
;; ;;                 (some? from) (conj [:created-at :>= from])
;; ;;                 (some? till) (conj [:created-at :<= till])
;; ;;                 (seq data-search-term) [:data :like (str "%" data-search-term "%")])
;; ;;         where (when (seq where) [:and where])]
;; ;;     (om/transact! this `[(admin/set-params
;; ;;                           ~{:event-store-params
;; ;;                             {:order-by [[:id :desc]],
;; ;;                              :limit {:count page-size :offset (* page page-size)}
;; ;;                              :where where}})
;; ;;                          :event-store])))

;; (defn props->params [props]
;;   (-> props :client/route-page-state :params :event-store-params))

;; (defn update-params [this f & more]
;;   (let [params (-> this om/props props->params)
;;         params (apply f params more)]
;;     (om/transact! this `[(admin/set-params
;;                           ~{:event-store-params params})
;;                          ;; :event-store
;;                          ])))
;; (def entity-types
;;   [{:entity-type nil :label "Any entity"}
;;    {:entity-type :group :label "Groups"}
;;    {:entity-type :user :label "Users"}
;;    {:entity-type :scheduled-alert :label "Scheduled alerts"}
;;    {:entity-type :admin :label "Admins"}
;;    {:entity-type :dossier-type :label "Dossier types"}
;;    {:entity-type :field :label "Dossier type fields"}

;;    {:entity-type :company :label "Companies"}
;;    {:entity-type :translation :label "Translations"}
;;    {:entity-type :job-offer :label "Job offers"}
;;    {:entity-type :translation :label "Translations"}

;;    ;;join tables
;;    {:entity-type :field :label "Dossier type fields"}
;;    {:entity-type :schedule-alert :label "Reports"}
;;    {:entity-type :template-user :label "Template user link table"}
;;    {:entity-type :invitation :label "Invitations"}


;;    ;;DC app tables
;;    {:entity-type :dossier :label "Dossiers"}
;;    {:entity-type :value :label "Field value"}
;;    {:entity-type :checklist :label "Checklists"}
;;    {:entity-type :answer :label "Answers"}
;;    {:entity-type :people :label "Contacts"}
;;    {:entity-type :task :label "Tasks"}
;;    {:entity-type :notes :label "Notes"}
;;    {:entity-type :document :label "Documents"}

;;    ;;etc
;;    ])

;; (defn event-types->event-type-maps [event-types]
;;   (map (fn [e]
;;          {:event-type e
;;           :label (str/capital (str/replace (name e) "-" " "))})
;;        event-types))

;; (def event-types
;;   (let [entities (->> entity-types
;;                       (map :entity-type)
;;                       (remove nil?))
;;         standard-admin-event-types (flatten (map (fn [e]
;;                                               [{:event-type (keyword (str (name e) "-created"))
;;                                                 :label (str (str/capital (name e)) " created")}
;;                                                {:event-type (keyword (str (name e) "-modified"))
;;                                                 :label (str (str/capital (name e)) " modified")}])
;;                                             entities))
;;         event-types-from-local-storage (event-types->event-type-maps
;;                                         (s/local-get :event-types))]
;;     (into [] (concat standard-admin-event-types
;;                      event-types-from-local-storage))))

;; (defn all-event-types [event-types more-events]
;;   (let [event-type-keywords (set (map :event-type event-types))
;;         more-event-types (->> more-events
;;                               (map :name)
;;                               distinct
;;                               (map keyword)
;;                               set)
;;         extra-event-types (set/difference more-event-types
;;                                           (set event-type-keywords))
;;         all-event-types (if (seq extra-event-types)
;;                           (do
;;                             (s/local-set :event-types extra-event-types)
;;                             (into [] (concat event-types
;;                                              (event-types->event-type-maps extra-event-types))))
;;                           event-types)
;;         all-event-types (sort-by :label all-event-types)]
;;     (into [{:event-type nil :label "Any event"}] all-event-types)))

;; (defui ^:once Page
;;   static om/IQuery
;;   (query [this]
;;     [(list history/event-store-query {:set-params :event-store})
;;      `({:cache [~history/event-store-query]}
;;        {:set-params :exclude-from-remote})
;;      '({:user [:id :name :email]} {:set-params :user-search})
;;      '({:group [:id :name]} {:set-params :group-search})
;;      :client/route-page-state
;;      ])
;;   Object
;;   (render [this]
;;     (let [{:keys [props computed state]} (om-data this)
;;           {:keys [event-type entity-type entity-id user group from till] :as params} (props->params props)
;;           {:keys [locale]} computed
;;           t (partial translate locale)
;;           event-types (all-event-types event-types
;;                                        (:event-store props))]
;;       (timbre/info "Rendering events page")
;;       ;; (timbre/info :#pp event-types)

;;       ;; (timbre/info :#pp (select-keys props [:event-store]))

;;       (html [:div {:class "container"}
;;              (let [label-value (fn [{:keys [label value label-style style param-kw search-param-kw]}]
;;                                  [:div {:style (merge style {:position "relative"})
;;                                         :class "hover"}
;;                                   [:div {:style (merge {:display "inline-block"
;;                                                         :margin-right 10
;;                                                         :min-width 100} label-style)} label]
;;                                   [:div {:style {:display "inline-block" :max-width "40%"}}
;;                                    value]
;;                                   [:span {:style {
;;                                                   ;; :padding 12
;;                                                   :position "relative"
;;                                                   :top 1 :right 10
;;                                                   :float "right"
;;                                                   :cursor "pointer"
;;                                                   :min-width 50
;;                                                   ;; :margin-right 30
;;                                                  }
;;                                          :class "show-on-hover inline"
;;                                          :on-click #(do
;;                                                       (when search-param-kw
;;                                                         (om/update-state! this assoc search-param-kw nil))
;;                                                       (update-params this assoc param-kw nil))}
;;                                    (ui/icon-button {:style {:transform "scale(0.7)"
;;                                                             ;; :float "right"
;;                                                             }}
;;                                                    (ui/font-icon {:class-name "icon-clear"}))]])
;;                    font-size 18]

;;                [:div {:class "row"}
;;                 [:div {:class "col-xs-4"
;;                        :style {:font-size font-size}}
;;                  [:div {:style {:position "fixed" :width "31%"}}
;;                   [:div {:class "pad-bot-20"}
;;                    (ui/raised-button {:label "Clear all"
;;                                       :on-click  #(do
;;                                                     (om/update-state! this assoc :event-type-search nil :entity-type-search nil
;;                                                                       :user-search nil :group-search nil)
;;                                                     (update-params this (constantly nil))
;;                                                     (om/transact! this [:event-store]))})]
;;                   [:div {:class "row"}
;;                    [:div {:class "col-xs"}
;;                     (ui/select-field
;;                      {:floating-label-text "Entity type"
;;                       :value (or (:entity-type params) :any-entity)
;;                       :on-change (fn [_ index value]
;;                                    (update-params this assoc :entity-type (:entity-type (get entity-types index)))
;;                                    ;; (timbre/info :#pp :on-change)
;;                                    )
;;                       :children (mapv #(ui/menu-item {:key %2 :primary-text (:label %1) :value (or (:entity-type %1)
;;                                                                                                    :any-entity)}) entity-types (range))})]]
;;                   (when (some? entity-type)
;;                     [:div {:class "row"}
;;                      [:div {:class "col-xs"}
;;                       (label-value {:label "Entity id:"
;;                                     :style {:max-height 48}
;;                                     :param-kw :entity-id
;;                                     :value (ui/text-field { ;; :hint-text "entity id"
;;                                                            ;; :floating-label-text "Entity id"
;;                                                            ;; :floating-label-style {:fontSize font-size}
;;                                                            :style {:maxWidth 100}
;;                                                            :value (or entity-id "")
;;                                                            :on-change #(update-params this assoc :entity-id (du/parse-natural-number (aget % "target" "value")))})
;;                                     })]])
;;                   [:div {:class "row"}
;;                    [:div {:class "col-xs"}
;;                     (ui/select-field
;;                      {:floating-label-text "Event type"
;;                       :value (or (:event-type params) :any-event)
;;                       :on-change (fn [_ index value]
;;                                    (update-params this assoc :event-type (:event-type (get event-types index))))
;;                       :children
;;                       (mapv #(ui/menu-item {:key %2 :primary-text (:label %1) :value (or (:event-type %1)
;;                                                                                          :any-event)}) event-types (range))})
;;                     ]]
;;                   [:div {:class "row"}
;;                    [:div {:class "col-xs"}
;;                     (label-value {:label "From:"
;;                                   :param-kw :from
;;                                   :value (ui/date-picker {:hint-text "Select date"
;;                                                           ;; :container "inline"
;;                                                           :value from
;;                                                           :style {:width 20}
;;                                                           :format-date (partial format-date locale)
;;                                                           :text-field-style {:fontSize font-size :width 200}
;;                                                           :on-change #(update-params this assoc :from %2)
;;                                                           :auto-ok true})})
;;                     ]]

;;                   [:div {:class "row"}
;;                    [:div {:class "col-xs"}
;;                     (label-value {:label "Till:"
;;                                   :param-kw :till
;;                                   :value (ui/date-picker {:hint-text "Select date"
;;                                                           ;; :container "inline"
;;                                                           :value till
;;                                                           :format-date (partial format-date locale)
;;                                                           :text-field-style {:fontSize font-size :width 200}
;;                                                           :on-change #(update-params this assoc :till %2)
;;                                                           :auto-ok true})})
;;                     ]]
;;                   [:div {:class "row"}
;;                    [:div {:class "col-xs"}

;;                     (label-value {:label "By group"
;;                                   :label-style {:position "relative" :top -1}
;;                                   :style {:max-height 48}
;;                                   :param-kw :group
;;                                   :search-param-kw :group-search
;;                                   :value
;;                                   (let [table :group
;;                                         choice (get params table)
;;                                         set-params-kw :group-search
;;                                         hint-text "Search for a group"
;;                                         label-kw :name
;;                                         options (get props table)
;;                                         data-source (map (fn [{:keys [id name]}] (str id ": " name)) options)]

;;                                     (ui/auto-complete {:hint-text (or (:name choice) hint-text)
;;                                                        ;; :floating-label-text "Entity-type"
;;                                                        ;; :text-field-style {:fontSize "inherit" :marginTop 4}
;;                                                        :style {:width 200}
;;                                                        :full-width true
;;                                                        ;; :tab-index tab-index
;;                                                        :id :user-input
;;                                                        :search-text
;;                                                        ;; (or (:name choice) "")
;;                                                        (or (get state set-params-kw) "")
;;                                                        :on-new-request (fn [search-term index]
;;                                                                          (timbre/info :#r :on-new-request search-term index)
;;                                                                          (let [index (if (= index -1)
;;                                                                                        (let [shortlist (map  #(vector (fuzzy-filter
;;                                                                                                                        search-term %1) %2)
;;                                                                                                              data-source (range (count data-source)))]
;;                                                                                          (some (fn [e] (and (first e) (second e))) shortlist))
;;                                                                                        index)
;;                                                                                new-choice (get options index)]
;;                                                                            ;; (timbre/info new-id value)
;;                                                                            (when true ;; (and new-choice
;;                                                                              ;; (not= choice new-choice))
;;                                                                              (om/transact! this `[(admin/set-params
;;                                                                                                    ~{set-params-kw nil})])
;;                                                                              (update-params this assoc table new-choice))
;;                                                                            (om/update-state! this assoc set-params-kw (get new-choice label-kw))))
;;                                                        :open-on-focus true
;;                                                        ;; :error-text (when message (t message))
;;                                                        :on-update-input (fn [search-term]
;;                                                                           (om/update-state! this assoc set-params-kw search-term)
;;                                                                           (on-update-input-debounced this {:search-term search-term
;;                                                                                                            :props-to-search [:id :name]
;;                                                                                                            :table table
;;                                                                                                            :set-params-kw set-params-kw})

;;                                                                           )
;;                                                        ;; :underline-focus-style {;; :border-color (:mediumBlue css/vars)
;;                                                        ;;                         :display "none"}
;;                                                        ;; :underline-style (and error? {:borderColor "red"})
;;                                                        ;; :underline-show false
;;                                                        :max-search-results 15
;;                                                        ;; :filter fuzzy-filter
;;                                                        :dataSource data-source}))})]]

;;                   [:div {:class "row"}
;;                    [:div {:class "col-xs"}

;;                     (label-value {:label "By user"
;;                                   :label-style {:position "relative" :top -1}
;;                                   :style {:max-height 48}
;;                                   :param-kw :user
;;                                   :search-param-kw :user-search
;;                                   :value
;;                                   (let [table :user
;;                                         choice (get params table)
;;                                         set-params-kw :user-search
;;                                         hint-text "Search for a user"
;;                                         label-kw :name
;;                                         options (get props table)
;;                                         data-source (map (fn [{:keys [id name email]}] (str id ": " name " (" email ")")) options)
;;                                         ]
;;                                     (ui/auto-complete {:hint-text (or (:name choice) hint-text)
;;                                                        ;; :floating-label-text "Entity-type"
;;                                                        ;; :text-field-style {:fontSize "inherit" :marginTop 4}
;;                                                        :style {:width 200}
;;                                                        :full-width true
;;                                                        ;; :tab-index tab-index
;;                                                        :id :user-input
;;                                                        :search-text ;; (or (:name choice) "")
;;                                                        (or (get state set-params-kw) "")
;;                                                        :on-new-request (fn [search-term index]
;;                                                                          (let [index (if (= index -1)
;;                                                                                        (let [shortlist (map  #(vector (fuzzy-filter
;;                                                                                                                        search-term %1) %2)
;;                                                                                                              data-source (range (count data-source)))]
;;                                                                                          (some (fn [e] (and (first e) (second e))) shortlist))
;;                                                                                        index)
;;                                                                                new-choice (get options index)]
;;                                                                            ;; (timbre/info new-id value)
;;                                                                            (when true ;; (and new-choice
;;                                                                              ;; (not= choice new-choice))
;;                                                                              (om/transact! this `[(admin/set-params
;;                                                                                                    ~{set-params-kw nil})])
;;                                                                              (update-params this assoc table new-choice))
;;                                                                            (om/update-state! this assoc set-params-kw (get new-choice label-kw))))
;;                                                        :open-on-focus true
;;                                                        ;; :error-text (when message (t message))
;;                                                        :on-update-input (fn [search-term]
;;                                                                           (om/update-state! this assoc set-params-kw search-term)
;;                                                                           (on-update-input-debounced this {:search-term search-term
;;                                                                                                            :table table
;;                                                                                                            :props-to-search [:id :name :email]
;;                                                                                                            :set-params-kw set-params-kw})

;;                                                                           )
;;                                                        ;; :underline-focus-style {;; :border-color (:mediumBlue css/vars)
;;                                                        ;;                         :display "none"}
;;                                                        ;; :underline-style (and error? {:borderColor "red"})
;;                                                        ;; :underline-show false
;;                                                        :max-search-results 15
;;                                                        :filter fuzzy-filter
;;                                                        :dataSource data-source}))})]]


;;                   ;; [:div {:class "row"}

;;                   ;;                    [:div {:class "col-xs"}
;;                   ;;                     (label-value {:label "By group with id:"
;;                   ;;                                   :value
;;                   ;;                                   (ui/text-field { ;; :floating-label-text "Group id"
;;                   ;;                                                   ;; :floating-label-style {:fontSize font-size}
;;                   ;;                                                   :id :group-id-input
;;                   ;;                                                   :style {:maxWidth 100}
;;                   ;;                                                   :value (or (:id group) "")
;;                   ;;                                                   :on-change #(let [group-id (du/parse-natural-number (aget % "target" "value"))
;;                   ;;                                                                     group {:name (str "Group with id " group-id)
;;                   ;;                                                                            :id group-id}]
;;                   ;;                                                                 (om/update-state! this assoc :group-search (:name group))
;;                   ;;                                                                 (update-params this assoc :group group))
;;                   ;; })})]]

;;                   ;; [:div {:class "row"}
;;                   ;;  [:div {:class "col-xs"}
;;                   ;;   (label-value {:label "By user with id"
;;                   ;;                 :value
;;                   ;;                 (ui/text-field { ;; :floating-label-text "User id"
;;                   ;;                                 ;; :floating-label-style {:fontSize font-size}
;;                   ;;                                 :id :user-id-input
;;                   ;;                                 :style {:maxWidth 100}
;;                   ;;                                 :value (or (:id user) "")
;;                   ;;                                 :on-change #(let [user-id (du/parse-natural-number (aget % "target" "value"))
;;                   ;;                                                   user {:name (str "User with id " user-id)
;;                   ;;                                                         :id user-id}]
;;                   ;;                                               (om/update-state! this assoc :user-search (:name user))
;;                   ;;                                               (update-params this assoc :user user))})})]
;;                   ;;  ]
;;                   [:div {:class "row center-xs"
;;                          :style {:margin-top 20}}
;;                    ;; {:style {:display "flex"
;;                    ;;               :flex-direction "row"
;;                    ;;               :flex-wrap "wrap"
;;                    ;;               :justify-content "center"
;;                    ;;               :align-items "center"
;;                    ;;               :margin-top 30
;;                    ;;               }}
;;                    [:div {:class "col-xs-3"}
;;                     (if (nil? (:event-store props))
;;                       (spinner-markup)
;;                       (ui/raised-button {:label "Load"
;;                                          :style {:margin-left -40}
;;                                          :on-click #(do
;;                                                       (update-params this assoc :page 0)
;;                                                       (om/transact! this ['(admin/set-query-key {:key :event-store
;;                                                                                                  :value nil})
;;                                                                           '(admin/set-query-key {:key :cache
;;                                                                                                  :value nil})
;;                                                                           :event-store]))}))]]
;;                   (when (nil? (:event-store props))
;;                     [:div {:class "row mar-top-15"
;;                            :style {:font-style "italic"}}

;;                      "Trying to fetch events. "
;;                      [:br] [:br]
;;                      "If this times out (after 10 seconds) please make your search more specific for a quicker response."])]]
;;                 (let [events (:event-store props)
;;                       cached-events (or (:event-store (:cache props)) [])
;;                       all-events (into cached-events events)]
;;                   [:div {:class "col-xs-8"}
;;                    (if (seq all-events)
;;                      (history/history-block this {:events all-events})
;;                      "No results")
;;                    (when (and
;;                           (not (zero? (count events)))
;;                           (zero? (rem (count events) (:page-size params))))
;;                      [:div {:style {:margin-bottom 30 :float "right"}}
;;                       (ui/flat-button {:label (t "More..")
;;                                        :on-click (fn []
;;                                                    (update-params this update :page inc)
;;                                                    (om/transact! this `[(admin/cache-records {:query-key :event-store
;;                                                                                               :cached-query-key :cache})
;;                                                                         :event-store]))})])])])]))))
