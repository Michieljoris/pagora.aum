(ns pagora.aum.frontend.channel.msg-handler
  (:require
   [pagora.aum.om.next :as om]
   [pagora.aum.om.next.protocols :as om-next-protocols]

   ;; Multimethod
   [pagora.aum.frontend.channel.core :refer [channel-msg-handler]]
   ;; [pagora.aum.frontend.reconciler.core :as aum]

   ;; Require reconciler so we can operate on its app-state
   ;; [reconciler.core :refer [reconciler value-merge-hook]]
   [pagora.aum.frontend.websockets.core :as websocket]
   [taoensso.timbre :as timbre]))


;; Websocket can go down, and up again. We can show the status in the ui. TODO:
;; this might be better as a mutation?
;; (defmethod channel-msg-handler :ws-state-change
;;   [msg]
;;   ;; TODO: Set a ui visual marker that websocket is down
;;   (info "Websocket state changed: " (:data msg))
;;   (om/merge! reconciler {:client/ws-open? (get-in msg [:data :open?])})
;;   )

;; Websocket can go down, and up again. We can show the status in the ui.
(defmethod channel-msg-handler :ws-state-change
  [msg]
  ;; TODO: Set a ui visual marker that websocket is down

;; TODO: enable again
  (timbre/info "Websocket state changed: " msg)
  ;; (om/transact! reconciler `[(app/merge-map {:map-to-merge
  ;;                                              {:client/ws-open? ~(get-in msg [:data :open?])}})
  ;;                            :client/ws-open?])
  (websocket/process-queued-send))


;; Server might push data to us.
(defmethod channel-msg-handler :ws-server-push
  [{:keys [data aum-config] :as msg}]

  (let [[event response] data]
    (condp = event
      :app/broadcast (let [om-request {:target :app
                                         :success? (constantly true)
                                         :om-callback (fn [to-be-merged]
                                                        ;; (timbre/info "to be merged from push:")
                                                        ;; (timbre/info :#pp to-be-merged)
                                                        ;; (om/merge! reconciler to-be-merged)
                                                        )
                                         ;; :mutations mutations
                                         }
                             ;; options {:verbose? true
                             ;;          :queue-keys om-next-protocols/queue!
                             ;;          :value-merge-hook value-merge-hook
                             ;;          }
                             ]

                         ;; TODO: enable again
                         ;; (aum/handle-response reconciler om-request
                         ;;                        (assoc-in response
                         ;;                                  [:value :pushed] true) options)
                         )
      nil))
  )
