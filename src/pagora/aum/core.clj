(ns pagora.aum.core
  (:require
   [pagora.aum.environment :as env]
   [pagora.aum.config :refer [make-app-config]]
   [pagora.aum.integrant.state :as ig-state]
   [taoensso.timbre :as timbre]
   [integrant.core :as ig]
   [pagora.clj-utils.timbre :refer [middleware]]))

(defn make-ig-system-config [{:keys [server states] :as config}]
  (merge {
    :pagora.aum.database.connection/db-conn
    {:config config}

    :pagora.aum.parser.core/parser-env
    {:config config
     :db-conn (ig/ref :pagora.aum.database.connection/db-conn)
     }

    :pagora.aum.parser.core/parser
    {:config config
     :parser-env (ig/ref :pagora.aum.parser.core/parser-env)}

    :pagora.aum.websockets.core/websocket-listener
    {:config config
     :parser (ig/ref :pagora.aum.parser.core/parser)
     :parser-env (ig/ref :pagora.aum.parser.core/parser-env)}


    :pagora.aum.web-server.routes/routes
    {:config config
     :websocket (ig/ref :pagora.aum.websockets.core/websocket-listener)}

    :pagora.aum.web-server.handler/handler
    {:config config
     :routes (ig/ref :pagora.aum.web-server.routes/routes)}

    :pagora.aum.web-server.core/httpkit-server
    {:server-options server
     :config config
     :handler (ig/ref :pagora.aum.web-server.handler/handler)}

    } states))

(def aum-multimethod-namespaces
  ['pagora.aum.database.validate-sql-fun
   'pagora.aum.database.process-params
   'pagora.aum.database.process-result
   ;; 'database.query-hooks
   ])

(defn- try-require [sym]
  (try (do (require sym :reload) sym)
       (catch java.io.FileNotFoundException _)))

(defn load-namespaces [symbols]
  (doall (->> symbols (map try-require))))


(def aum-state {;; :app-config nil
                ;; :ig-system-config nil
                ;; :ig-system nil
                ;; :ig-system-vars nil
                })

(defn init
  [{:keys [db-config app-config-ns frontend-config-keys]}]
  (when app-config-ns
    (timbre/info "Loading app config ns:" (into [] (load-namespaces [app-config-ns]))))
  (let [{:keys [multimethod-namespaces] :as app-config} (make-app-config)
        app-config (merge {:db-config db-config
                           :frontend-config (select-keys app-config frontend-config-keys)}
                          app-config)
        _ (when-let [timbre-log-level (:timbre-log-level app-config)]
            (timbre/merge-config! {:level timbre-log-level
                                   :middleware [middleware]}))
        ig-system-config (make-ig-system-config app-config)
        aum-config {:app-config app-config
                    :ig-system-config ig-system-config}]
    (timbre/info "Loading integrant namespaces:" (into [] (ig/load-namespaces ig-system-config)))
    (timbre/info "Loading multimethod namespaces:" (into [] (load-namespaces (concat aum-multimethod-namespaces multimethod-namespaces))))
    (alter-var-root #'aum-state merge aum-config)
    aum-config))


(defn get-app-config []
 (:app-config aum-state))

(defn get-ig-vars []
  (:ig-system-vars aum-state))

(defn get-db-conn []
  (:db-conn (get-ig-vars)))

(defn get-parser []
  (:parser (get-ig-vars)))

(defn get-parser-env []
  (:parser-env (get-ig-vars)))

(defn get-routes []
  (:routes (get-ig-vars)))

(defn ig-init [ig-system-config]
  (let [system (ig/init ig-system-config)]
    (alter-var-root #'aum-state assoc
                    :ig-system system
                    :ig-system-vars (into {} (map (fn [[k _]]
                                                    [(keyword (name k)) (get system k)])
                                                  ig-system-config)))
   system))

(defn go
  "For production use. The ig-system var could be used to halt, reconfigure and
  restart the system in production"
  [{:keys [ig-system-config]}]
  (let [system (ig-init ig-system-config)]
    ;;So we can restart it:
    (when (not env/is-development?)
      (alter-var-root #'ig-state/system (constantly system)))))

(defn restart [aum-config]
  (ig/halt! ig-state/system)
  (go aum-config))
