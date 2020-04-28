(ns  pagora.aum.frontend.websockets.dispatcher
  (:require
   [cljs.core.async :as async :refer (<! >! put! chan)]
   [taoensso.encore :as encore :refer-macros (have have?)]
   [taoensso.timbre :as timbre]
   [pagora.aum.frontend.channel.core :refer [get-or-make-channel]]))

;; Sente multimethod event handler, dispatches on event-id
(defmulti websocket-msg-handler
  (fn [{:keys [id]} aum-config]
    id))

(defmethod websocket-msg-handler :default
  [{:as ev-msg :keys [id]} _]
  (timbre/debugf "Unhandled event: %s" id)
  {:test-feedback :default :ev-msg ev-msg})

;; Socket state changes
(defmethod websocket-msg-handler :chsk/state
  [{:as ev-msg :keys [?data]} aum-config]
  (let [[old-state-map new-state-map] (have vector? ?data)
        channel (get-or-make-channel)]
    (when (:first-open? new-state-map)
      (put! channel {:event :ws-first-open :data new-state-map :aum-config aum-config}
            #(timbre/debugf "Channel socket successfully established!: %s" new-state-map)))
    (put! channel {:event :ws-state-change :data new-state-map :aum-config aum-config}
          #(timbre/debugf "Channel socket state change: %s" new-state-map))
    ))

;; Handshake
(defmethod websocket-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]} _]
  (let [[?uid ?csrf-token ?handshake-data] ?data]
    (timbre/debugf "Handshake: %s" ?data)))

(defmethod websocket-msg-handler :chsk/ws-ping
  [{:as ev-msg :keys [?data]} _]
  (timbre/info :#p "websocket ping")
  {:test-feedback :chsk/ws-ping :ev-msg ev-msg})

;; Push event from server
(defmethod websocket-msg-handler :chsk/recv
  [{:as ev-msg :keys [id ?data]} aum-config]
  (put! (get-or-make-channel) {:event :ws-server-push :data ?data :aum-config aum-config}
        #(timbre/infof "Push event from server: %s" ?data))
  {:test-feedback :chsk/recv :ev-msg ev-msg})
