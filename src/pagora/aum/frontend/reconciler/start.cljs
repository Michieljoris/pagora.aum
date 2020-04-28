(ns pagora.aum.frontend.reconciler.start
  (:require
   [taoensso.sente :refer [cb-success?]]
   [pagora.aum.frontend.reconciler.core :as aum]
   [pagora.clj-utils.core :as cu]
   [goog.log :as glog]
   [pagora.aum.om.next :as om]
   [pagora.aum.frontend.parser.read :as read]
   [pagora.aum.frontend.parser.mutate :as mutate]
   [pagora.aum.frontend.websockets.core :as websocket]
   )
  )

;; read* and mutate* both process the params somewhat before they go the
;; multimethods read and mutate. Both can be extended by requiring
;; pagora.aum.frontend.parser.read/mutate and extending read and mutate methods
;; respectively
(defn make-parser [app-config]
  (let [default-remote (or (:default-remote app-config) :aum)]
    (om/parser {:read (read/make-read-fn {:default-remote default-remote
                                          :app-config app-config})
                :mutate (mutate/make-mutate-fn {:default-remote default-remote
                                                :app-config app-config})})))

(def default-app-state
  {:client/reload-key :foo})

(defn make-aum-remote [app-config]
  (fn [query response-cb]
    (let [chsk-send! (websocket/get-chsk-send!-fn app-config)]
      (chsk-send! [:aum/query query] (:websocket-timeout app-config 8000)
                  response-cb))))

(defn make-value-merge-hook [hooks]
  (fn [value]
    (reduce (fn [value hook]
              (cu/deep-merge-maps value (hook value)))
            value hooks)))

(defn start-reconciler
  [{:keys [app-state app-config]}]
  (let [{:keys [parser remotes value-merge-hooks]} app-config]
    (aum/make-reconciler {:state (atom (merge default-app-state app-state))
                          :verbose? (-> app-config :debug :send)
                          :easy-reads true
                          :pathopt true
                          ;; Network level success status, not response status
                          :success? cb-success?
                          :logger (when-not (get-in app-config [:debug :disable-om-logger])
                                    (glog/getLogger "om.next3"))
                          :value-merge-hook (make-value-merge-hook value-merge-hooks)
                          :parser (or parser (make-parser app-config))
                          ;; :tx-listen (fn [env {:keys [tx ret sends]}]
                          ;;              (let [{mutations true read-keys false} (group-by om-util/mutation? tx)]
                          ;;                (timbre/info :#g "TRANSACTION:mutations and read keys")
                          ;;                (js/console.log (str "%c" (with-out-str (pprint {:mutations mutations
                          ;;                                                                 :read-keys read-keys}))) "color:green")
                          ;;                ;; (js/console.log (str "%c" (with-out-str (pprint read-keys))) "color:green")
                          ;;                ;; (js/console.log read-keys)
                          ;;                )
                          ;;              ;; (timbre/info :#pp ret)
                          ;;              ;; (timbre/info :#g "SENDS")
                          ;;              ;; (js/console.log sends)
                          ;;              ;; (timbre/info :#g "<<<<<")
                          ;;              )
                          ;; Use this instead of the debug statements in components
                          ;; We can store it all in app state to review with inspector
                          ;; Or set some atom with what's enabled what's not, and print props of
                          ;; enabled components out on console
                          ;; :instrument (fn [{:keys [props children class factory] :as m}]
                          ;;               (timbre/info (class->fqn class))
                          ;;               (timbre/info :#cp props)
                          ;;               ;; (timbre/info :#cp children) ;;always nil???
                          ;;               (apply factory props children)
                          ;;               )
                          ;;:instrument (plomber/instrument)
                          :history-size (:history-size app-config)
                          :remotes (merge {:aum (make-aum-remote app-config)}
                                          (into {} (map (fn [[r f]] ;;TODO-aum test!!!!
                                                          [r (f app-config)])
                                                        remotes)))
                          :shared {:debug (:debug app-config)}})))
