(ns pagora.aum.frontend.websockets.core
  (:require
   [cljs.core.async :as async :refer (<! >! put! chan)]
   [pagora.aum.frontend.websockets.dispatcher :refer [websocket-msg-handler]]
   [pagora.aum.frontend.channel.core :refer [get-or-make-channel]] ;;so we can msg to frontend mock-backend
   [taoensso.sente  :as sente]

   ;; logging
   [taoensso.timbre :as timbre]))

(def path "app/")


;; Sente -----------------------------------------------
;; https://github.com/ptaoussanis/sente/issues/50
;; (def chsk-url-fn
;;   "(ƒ [path window-location websocket?]) -> server-side chsk route URL string.
;;     * path       - As provided to client-side `make-channel-socket!` fn
;;                    (usu. \"/chsk\").
;;     * websocket? - True for WebSocket connections, false for Ajax (long-polling)
;;                    connections.
;;     * window-location - Map with keys:
;;       :href     ; \"http://www.example.org:80/foo/bar?q=baz#bang\"
;;       :protocol ; \"http:\" ; Note the :
;;       :hostname ; \"example.org\"
;;       :host     ; \"example.org:80\"
;;       :pathname ; \"/foo/bar\"
;;       :search   ; \"?q=baz\"
;;       :hash     ; \"#bang\"
;;   Note that the *same* URL is used for: WebSockets, POSTs, GETs. Server-side
;;   routes should be configured accordingly."
;;   (fn [path {:as window-location :keys [protocol host pathname]} websocket?]
;;     (let [url (str (if (= websocket? :ajax) protocol (if (= protocol "https:") "wss:" "ws:"))
;;                    "//localhost:8080"  (or path pathname))]
;;       url
;;       )))

(def websocket (atom nil))

(defn make-websocket [{:keys [app-config]}]

  (let [ ;; For this example, select a random protocol:
        ;; rand-chsk-type (if (>= (rand) 0.5) :ajax :auto)
        ;; _ (infof "Randomly selected chsk type: %s" rand-chsk-type)

        ;; Serializtion format, must use same val for client + server:
        packer :edn  ; Default packer, a good choice in most cases
        ;; (sente-transit/get-transit-packer) ; Needs Transit dep
        _ (timbre/info :#pp {:app-config app-config})
        {:keys [chsk ch-recv send-fn state]} (sente/make-channel-socket-client!
                                              ;;TODO-aum: get app-path to frontend via macro!!! And other config as well.
                                              ;;Keep frontend-config-keys to get non aum config to frontend.
                                          (str (:app-path app-config) "app/chsk"); Must match server Ring routing URL
                                              ;; {:type :ajax
                                              {:type :auto
                                               ;; :chsk-url-fn chsk-url-fn
                                               :packer packer})]
    {:chsk chsk
     :state state
     :ch-chsk ch-recv           ; ChannelSocket's receive channel
     :chsk-state state             ; Watchable, read-only atom
     :chsk-send! send-fn}))

;;Single item queue should work because aum reconciler always sends a next
;;remote query not till after response from last one has been received.
(def queued-send (atom nil))

(defn process-queued-send []
  (let [ws-open? (:open? @(:state @websocket))
        send-fn @queued-send]
    ;; (timbre/info :#r "trying to dequeue send-fn" ws-open? send-fn)
    (when (and ws-open? send-fn)
      ;; (timbre/info :#r "executing queued send-fn")
      (reset! queued-send nil)
      (send-fn))))

(defn send-fn [query timeout cb]
  (let [{:keys [chsk-send!]} @websocket]
    ;; (timbre/info :#r "queueing send-fn")
    (reset! queued-send  #(chsk-send! query timeout cb))
    (process-queued-send)))

(def chsk-send! (atom send-fn))

(defn get-chsk-send!-fn [app-config]
  (if (:mock-backend? app-config)
    (let [channel (get-or-make-channel)]
      (fn [query timeout cb]
        (put! channel {:event :chsk-send!
                       :data {:sente-query query
                              :timeout timeout
                              :cb cb}}
              #(timbre/info :#r "Mocking backend!!" query))))
    @chsk-send!))

(defn stop-websocket! [stop-fn]
  ;; (.log js/console "stopping sente")
  (timbre/info stop-fn)
  (stop-fn))

(defn chsk-reconnect! []
  (sente/chsk-reconnect! (:chsk @websocket)))

(def reconnect!
  (atom chsk-reconnect!))

(defn chsk-connect! []
  (when (not (:open? @(:state @websocket)))
    (timbre/info "Trying to connect..")
    (sente/chsk-connect! (:chsk @websocket))))

(defn close-ws []
  (let [s (:socket_ (:chsk @websocket))
          s @s]
       (.close s)))

(defonce stop-fn (atom nil))

(defn stop! []
  (timbre/info "Stopping sente")
  (timbre/info @stop-fn)
  (@stop-fn))

(defn make-websocket-msg-handler [aum-config]
  (fn [ev-msg]
    (websocket-msg-handler ev-msg aum-config)))


(defn start! [aum-config]
  (let [websocket-msg-handler (make-websocket-msg-handler aum-config)]
    (reset! websocket (make-websocket aum-config))
    (reset! stop-fn  (sente/start-chsk-router! (:ch-chsk @websocket) websocket-msg-handler))))


;; (defn sente? []
;;   (sente/chsk-send! :sente/uid-1 [:some/request-id {:name "michiel"}])
;;   )
;; (sente?)

