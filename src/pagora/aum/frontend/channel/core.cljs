(ns pagora.aum.frontend.channel.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core.async :as async :refer [<! >! put! chan close!]]
   [taoensso.timbre :as timbre]))

(def channel (chan))

(defn get-or-make-channel []
  (when (.-closed channel)
    (do
      (timbre/info "creating channel")
      (def channel (chan))))
  channel)

(defn close-channel []
  (when channel
    (timbre/info "closing channel")
    (close! channel))
  )

;; Listen to events originating from the web socket.
(defmulti channel-msg-handler :event)

(defmethod channel-msg-handler :default
  [msg]
  (timbre/info "Unknown channel msg" msg))


(defn start-channel-listener!
  "Listen for async msgs (websocket open, server pushes etc)"
  []
  (let [channel (get-or-make-channel)]
    (timbre/info "Started channel listener")
    (go-loop []
      (when-let [msg (<! channel)]
        (channel-msg-handler msg)
        (recur)))
    channel
    ))
