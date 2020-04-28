(ns pagora.aum.frontend.core
  (:require
   [goog.dom :refer [getElement]]
   [pagora.aum.om.next :as om]
   [taoensso.timbre :as timbre]
   [pagora.aum.frontend.config :refer [make-app-config]]
   [pagora.aum.frontend.channel.core :refer [start-channel-listener! channel-msg-handler]]
   [pagora.aum.frontend.channel.msg-handler]
   [pagora.aum.frontend.reconciler.start :refer [start-reconciler] ]
   [pagora.aum.frontend.websockets.core :as websocket]
   [pagora.clj-utils.timbre :refer [console-appender]]
   ;; [components.root-component :refer [RootComponent]]
   ;; [pagora.aum.frontend.reconciler.core :refer [reconciler]]
   ))

(def config {:app :app})

(defonce root (atom nil))

(defn mount-app [reconciler App]
  (let [target (getElement "app")]
    (timbre/info "Mounting om app")
    (om/add-root! reconciler App target)
    (timbre/info "App mounted")
    (reset! root App)))

(defn mount-app-or-test-runner [reconciler RootComponent]
  (when (nil? @root)
    (condp = (:app config)
      ;; :test-runner (put! (get-or-make-channel) {:event :mount-test-runner! :data {:mount-app mount-app}}
      ;;                    #(timbre/info "Sent msg to start test-runner"))
      :app (mount-app reconciler RootComponent))))


(defonce aum-state (atom nil))

;; This handler is triggered when websocket is ready to send and receive msgs.
;; When this is the case we mount our om app.
(defmethod channel-msg-handler :ws-first-open
  [{:keys [aum-config] :as msg}]
  (timbre/info :#b "Websocket opened: " msg)
  ((:chsk-send! @websocket/websocket) [:aum/frontend-config nil] 8000
   (fn [resp]
     (let [{:keys [app-config RootComponent app-state]} (update aum-config :app-config merge resp)
           app-state (merge app-state {:client/csrf-token (get-in msg [:data :csrf-token])})
           reconciler (start-reconciler
                       {:app-config app-config
                        :app-state app-state})]
       (reset! aum-state {:reconciler reconciler
                          :RootComponent RootComponent
                          :app-state (om/app-state reconciler)
                          :app-config app-config})
       (mount-app-or-test-runner reconciler RootComponent)))))

(defn init [aum-config]
  (let  [app-config (make-app-config)]
    (set! *warn-on-infer* true)
    (enable-console-print!)
    (timbre/merge-config! {:level (get-in app-config [:debug :timbre-level])
                           :appenders {:console (console-appender)}})
    (assoc aum-config :app-config app-config)))

(defn go [aum-config]
  (timbre/info :#b "App started")
  (start-channel-listener!)
  (websocket/start! aum-config))
