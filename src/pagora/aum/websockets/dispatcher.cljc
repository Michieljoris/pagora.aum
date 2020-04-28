(ns pagora.aum.websockets.dispatcher
  (:require
   #?@(:clj [;; [app.integrations :as integrations]
             [pagora.clj-utils.network.http-get :as network]
             ;;           ;; [pagora.aum.test.snapshot :as snapshot]
             ])
   [pagora.clj-utils.core :as cu]
   [pagora.aum.util :as au]
   [pagora.aum.security :as security]
   [pagora.aum.om.next.impl.parser :as omp]
   [clojure.pprint :refer [pprint]]
   [taoensso.timbre :as timbre]))

;; Sente multimethod event handler, dispatches on event-id ------------
(defmulti event-msg-handler
  (fn [{:keys [id]} {:keys [config parser parser-env websocket]}]
    id))

;; Helper fns -----------
(defn get-user-by-ev-msg
  "Checks if remember token in db for uid is still valid, if so,
  returns user associated"
  [env {:as ev-msg :keys [uid ring-req ?reply-fn]}]
  (let [user (security/get-user-by-remember-token env ring-req)
        user-id (:id user)]
    (when (and (not= uid :uid-nil) (= (au/user-id->uid user-id) uid))
      user)))

(defn mutation? [query]
  (some (comp symbol? :dispatch-key) (:children (omp/query->ast query))))

(defn send-response [{:keys [uid ?reply-fn query response config]}]
  (when (get-in config [:query-log])
    (timbre/info :#w (if (mutation? query) "Mutation" "Read")" reply sent ")
    (when (au/is-development? config) (pprint response)))
  (?reply-fn response))


;; Handlers ----------
(defmethod event-msg-handler :chsk/handshake
  [{:keys [?data]} _]
  (let [[?uid ?csrf-token ?handshake-data] ?data]
    (timbre/infof "Handshake: %s" ?data)))

(defmethod event-msg-handler :chsk/ws-ping
;; Pinging from client
  [{:keys [event id ?data ring-req ?reply-fn send-fn]} _]
  (let [session (:session ring-req)
        uid     (:uid     session)]
    ;; (timbre/debugf "Ping event: %s" event)
    (when ?reply-fn
      (?reply-fn {:umatched-event-as-echoed-from-from-server event}))))

;; Process a query (read/mutation) from client
(defmethod event-msg-handler :aum/query
  [{:keys [?data ?reply-fn uid] :as ev-msg }
   {:keys [parser parser-env config websocket]}]
  (try
    (let [query      ?data
          query-type (if (mutation? query) "Mutation" "Read")]
      (when (:query-log config)
        (if (au/is-development? config)
          (do
            (timbre/info :#w query-type " received: ")
            (pprint query))
          (timbre/info :#w query-type " received: " query)))

      ;; Parser might set status to something other than :ok
      (reset! (:state parser-env) {:status :ok})
      (let [user   {:id 1 :name "admin"}  ;; (get-user-by-ev-msg parser-env ev-msg)
            response {:value (if user
                               (let [user (security/process-user parser-env user)
                                     user (select-keys user
                                                            (security/get-whitelist parser-env :read :user user))
                                     {:keys [chsk-send!]} websocket]
                                 (parser {:user user
                                          :push (fn [response]
                                                  ;; (timbre/info (str "Pushing response to " uid ":"))
                                                  (pprint response)
                                                  ;;TODO-MERGE: mock this in mock-mode.
                                                  #?(:clj
                                                     (chsk-send! uid [:app/broadcast response])))}
                                         query))
                               {:authenticated? false})}]
        (when ?reply-fn           
          (let [state    (-> parser-env :state deref)
                response (merge (select-keys state [:status :table-data :original-table-data :warnings :tempids])
                                response)]
            (if (:simulate-network-latency config)
              (do
                (timbre/info :#y "Simulating network latency of " (/ (:latency config) 1000.0) " seconds")
                #?(:clj (Thread/sleep (:latency (config))))
                (send-response {:uid uid :?reply-fn ?reply-fn :query query :response response
                                :config config}))
              (send-response {:uid uid :?reply-fn ?reply-fn :query query :response response
                              :config config}))))))
    (catch #?(:clj Exception :cljs :default) e
        (timbre/info e)
      (let [{:keys [msg context stacktrace] :as error} (cu/parse-ex-info e)]
        (?reply-fn
         {:status :error
          :value {:message msg :query ?data :context context :stacktrace [:not-returned]}})))
    ))

(defmethod event-msg-handler :app/call-external-api
  [{:keys [?data ?reply-fn]} _]
  (timbre/info "?data" ?data)
  ;;TODO validate (:id ?data) is a number!!!
  (let [{:keys [username password url]} ?data]
    #?(:cljs (timbre/info "TODO-MERGE mock api response")
       :clj (network/http-get {:url url :cb (fn [result]
                                         (timbre/info result)
                                         (?reply-fn result))
                          :username username :password password}))))

(defmethod event-msg-handler :app/list-table-cols
  [{:as ev-msg :keys [?data ?reply-fn]} {:keys [parser-env]}]
  (let [{:keys [table type]} ?data
        user (get-user-by-ev-msg parser-env ev-msg)]
    (?reply-fn (security/get-whitelist parser-env type table user))))

;; (security/get-whitelist (assoc p/parser-env :db-config database.config/db-config) :read :question {:id 1 :role "group-admi"})

;; #?(:clj
;;    (defmethod event-msg-handler :app/get-build
;;      [{:as ev-msg :keys [?data ?reply-fn]} _]
;;      (?reply-fn integrations/build-info)))

(def snapshot-file-name "/test/cljs/tests/snapshot_data.cljs" )

;; #?(:clj
;;    (defmethod event-msg-handler :tests/update-snapshot
;;      [{:as ev-msg :keys [?data ?reply-fn]} _]
;;      (let [{:keys [path actual-result]} ?data]
;;        (timbre/info ?data)
;;        (snapshot/update-snapshot-data snapshot-file-name path actual-result)
;;        (?reply-fn :ok))))

(defmethod event-msg-handler :app/test
  [{:keys [event id ?data ring-req ?reply-fn send-fn uid connected-uids]} _]
  (timbre/info "Test received!!!")
  ;; (pprint ev-msg)
  (timbre/info ?data)
  (timbre/info "meta:" (meta ?data))
  (when ?reply-fn
    (?reply-fn (:test-received ?data))))

(defmethod event-msg-handler :app/login
  [{:keys [?data ?reply-fn]} {{:keys [config]} :parser-env
                              :as parser-env}]
  (when (not (:disable-login? config))
    (timbre/info "Login:" (assoc ?data :password "***"))
    (let [user (security/login parser-env ?data)
          user (security/process-user parser-env user)
          response {:authenticated (boolean (:remember-token user))
                    :remember-token (:remember-token user)
                    :user-role (:role user)}]
      (when ?reply-fn
        ;; (timbre/info response)
        (?reply-fn response)))))

(defmethod event-msg-handler :app/logout
  [{:keys [?reply-fn] :as ev-msg} {:keys [parser-env]}]
  (let [user (get-user-by-ev-msg parser-env ev-msg)
        response {:response (security/logout parser-env user)}]
    (when ?reply-fn
      ;; (timbre/info response)
      (?reply-fn response))))

(defmethod event-msg-handler :aum/frontend-config
  [{:keys [?reply-fn id] :as ev-msg} {:keys [config parser-env]}]
  (let [user nil ;; (get-user-by-ev-msg parser-env ev-msg)
        ]
    (timbre/info :#pp {:frontend-config (:frontend-config config)})

    (when ?reply-fn
      (?reply-fn (:frontend-config config)))))

;; (defn find-handler-fn
;;   "Every msg from sente has an id, here we hackishly look for a var in
;;   this ns that has the same name"
;;   [sente-id]
;;   (timbre/info "sente-id" sente-id)
;;   (ns-resolve (:ns (meta #'event-msg-handler)) (symbol (name sente-id))))

;;TODO-CLJC
;; ev-msg map to spoof for frontend:
;; (timbre/info :#r (event-msg-handler {:id :app/login
;;                                      ;; :uuid 1
;;                                      ;; :ring-req {}
;;                                      :?data {:user "foo user" :password "abc"}
;;                                      :?reply-fn (fn [data] (timbre/info data))}))
;; (get-in req [:cookies "remember_token" :value])

(defmethod event-msg-handler :default
  [{:keys [event id ?data ring-req ?reply-fn send-fn uid connected-uids]
    :as ev-msg} {:keys [config]}]
  (let [session (:session ring-req)
        ;; uid     (:uid     session)
        ]
    (timbre/info "EVENT in event-msg-handler")
    (timbre/info :#pp
                 {:id id
                  :uid uid
                  :event event
                  :data ?data
                  :cookies (:cookies ring-req)
                  :connected-uids connected-uids
                  :session session
                  :reply-fn ?reply-fn
                  })
    ;; (timbre/info :#pp {:frontend-config (:frontend-config config)})

    ;;
    (if-let [handler-fn false ;; (find-handler-fn id)
             ;; authorized? (authorize-ev-msg ev-msg)
             ]
      ;; if handler-fn                           ;we've got a handler for it
      (handler-fn ev-msg)
      ;; (if authorized?
      ;;   (handler-fn ev-msg)
      ;;   (when ?reply-fn               ;if not authorized suggest client set current-user to nil
      ;;     (timbre/info "Unauthorized, but trying to query:" ?data)
      ;;     (?reply-fn {:current-user nil}))
      ;;   )

      (do
        (timbre/debugf "Unhandled sente id: %s" event)
        (when ?reply-fn     ;let client know he's sent something unprocessable
          (?reply-fn {:umatched-event-as-echoed-from-from-server event}))))))



;; ;;  ; Note that this'll be fast+reliable even over Ajax!:
;; (defn test-fast-server>user-pushes []
;;   (doseq [uid (:any @connected-uids)]
;;     (doseq [i (range 100)]
;;       (chsk-send! uid [:fast-push/is-fast (str "hello " i "!!")]))))

;; (comment (test-fast-server>user-pushes))

;; ;; ;;;; Example: broadcast server>user

;; ;; ;; As an example of push notifications, we'll setup a server loop to broadcast
;; ;; ;; an event to _all_ possible user-ids every 10 seconds:

;; (defn start-broadcaster! []
;;   (go-loop [i 0]
;;     (<! (async/timeout 10000))
;;     (println (format "Broadcasting server>user: %s" @connected-uids))
;;     (doseq [uid (:any @connected-uids)]
;;       (chsk-send! uid
;;                   [:some/broadcast
;;                    {:what-is-this "A broadcast pushed from server"
;;                     :how-often    "Every 10 seconds"
;;                     :to-whom uid
;;                     :i i}]))
;;     (recur (inc i))))

;; (comment (start-broadcaster!))

;; (try
;;   (json/read-str "")
;;   (catch Exception e
;;     (timbre/warn (.toString e))))


;; (reset! sente/debug-mode?_ true) ; Uncomment for extra debug info
