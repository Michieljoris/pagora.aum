(ns pagora.aum.config
  (:require
   [environ.core :refer [env]]
   [pagora.clj-utils.core :as cu]
   [pagora.aum.environment :refer [environment]]
   [taoensso.timbre.appenders.3rd-party.logstash :refer [logstash-appender]]
   [taoensso.timbre :as timbre]
   ))

;; Config keys need to be assigned scalar values (so no maps or vectors) so we
;; can set them in env vars on the command line

;; Something like the following would run the jar (in the target directory)::
;; CLJ_ENV=prod DB_USER=test DB_PASSWORD=abc DB_URL="//localhost:3306/" DB_NAME=some_db QUERY_LOG=true  SQL_LOG=true HTTP_LOG=false SERVER_PORT=8080 SERVER_IP=0.0.0.0 java -jar prod.jar


(def parser-config
  {
   ;; Om-next parser
   :limit-max 100 ;with joins query time can grow exponentially, so impose top limit on number of queried records
   :derive-join-type-from-schema true ;if keys and table names are regular saves configuring db
   :om-process-roots false          ;whether to ignore keys that are not a table
   :normalize true                  ;whether to embed joins or store in a map

   :sql-log true                        ;print actual sql queries being made
   :query-log true
   :validate-log true

   :simulate-network-latency false
   :latency 2000

   ;;Event store is disabled since it's not yet in master
   :event-store-disabled false;; we might also want to disable this in testing for instance

   :print-exceptions true
   })


;;This map should have all possible keys
(def default-config
  {:locales (array-map :nl {:language "Dutch" :locale :nl}
                       :de {:language "German" :locale :de}
                       :en {:language "English" :locale :en})
   :email-regex #"(?i)^[^\s]+@([\da-z\.-]+)\.([a-z\.]{2,63})$"

   :max-image-dimensions {:logo {:width 100
                                 :height 100
                                 :max-file-size 3000
                                 }
                          :brand {:width 300
                                  :height 50
                                  :max-file-size 5000
                                  }
                          :pdf-logo {:width 3000 :height 3000
                                     :max-file-size 100000}}
   :app-path "app/"
   :app-html-file-name "app.html"
   :devcards-html-file-name "devcards.html"

   ;; Web server
   :server-ip "127.0.0.1"
   :server-port "8080"

   ;; Mysql database
   :db-url "//localhost:3306/"
   ;; :db-name "chin_minimal"
   ;; :db-name "chin_minimal_templates"
   :db-name  "foo"
   :db-use-ssl false
   :om-next-test-db-name "om_next_test"
   :db-user "root"
   :db-password ""

   :db-pool true             ; in dev mode running tests mess up db-conn if true
   :db-pool-loglevel "INFO"
   :min-pool-size 3
   :initial-pool-size 3
   :db-print-spec true

   :elasticsearch-url "http://localhost:9200"
   :elasticsearch-log-result true

   ;; Logging
   :http-log false
   :timbre-log-level :info
   :print-exceptions true
   :integrant-log true

   :logstash-host "0.0.0.0"
   :logstash-port 12345
   :logstash-level :info
   :logstash-enabled true

   ;; Following two settings need coordination with the compression and
   ;; gzip tasks in build.boot

   ;; Whether to set correct type and encoding for requested files that end in a .gz extension
   :gz-mime-types false ;; defaults to: #{"text/css" "text/javascript"}
   :disable-login? false

   ;;Elasticsearch
   :es-url "http://127.0.0.1:9200"

   ;;OBS
   ;;For local development create .env file in root dir of repo with content like this:
   ;; export OBS_BUCKET="chinchilla-development"
   ;; export OBS_ACCESS_KEY="some access key"
   ;; export OBS_SECRET_KEY="some secret key"
   :obs-access-key "foo"
   :obs-secret-key "bar"
   :obs-bucket "??"
   :obs-region "eu-de"
   :obs-service "s3"

   ;;Show keys that don't have a translation as such, meaning, the whole key is shown
   ;;in square brackets, so with the prefex app/. By default the key is used
   ;;as its own default translation, or whatever is set in app.translation-keys ns.
   :mark-untranslated-keys false

   :redis-host "127.0.0.1"
   :redis-port "6379"
   :redis-password nil

   :event-store-disabled true

   :skip-wrap-authenticate false ;; {:id 1 :email "test@axion5.net" :group_id 10}
   :enable-cors true

   :vorlon-script false       ;whether to add vorlon.js script tag to app.html
   :websocket-timeout 8000
   :default-remote :aum
   :remotes nil
   })

(defn get-env-var-or-v [k v]
  (let [env-var (env k)
        env-var (if (= env-var "false") false env-var)]
    (if (nil? env-var) v env-var)))

(defn config-from-env [config]
  (into {} (map (fn [k]
                  [k (get-env-var-or-v k (get config k))])
                (keys config))))

(defmulti config
  (fn [environment]
    environment))

(defmethod config :default [_] nil)

(defn make-app-config []
  (let [app-config (merge (config :common) (config environment))
        app-config (merge default-config parser-config app-config)
        app-config (merge app-config (config-from-env app-config) {:env environment :clj-env environment})
        mysql-database {:url (:db-url app-config)
                        :db-name (:db-name app-config)
                        :om-next-test-db-name  (:om-next-test-db-name app-config)
                        :user (:db-user app-config)
                        :password (:db-password app-config)
                        :use-ssl (:db-use-ssl app-config)
                        :pool (:db-pool app-config)
                        :pool-loglevel (:db-pool-loglevel app-config)
                        :min-pool-size (:min-pool-size app-config)
                        :initial-pool-size (:initial-pool-size app-config)
                        :print-spec (:db-print-spec app-config)}
        redis {:host (:redis-host app-config)
               :port (or (cu/parse-natural-number (:redis-port app-config)) 6379)
               :password (:redis-password app-config)
               :translation {:prefix "translations."}}
        app-config (assoc app-config
                      :mysql-database mysql-database
                      :redis redis
                      :server {:port (cu/parse-natural-number (:server-port app-config))
                               :ip (:server-ip app-config)}
                      :nrepl {:port (cu/parse-natural-number (:nrepl-port app-config))
                              :ip (:nrepl-host app-config)}
                      :logstash-port (cu/parse-natural-number (:logstash-port app-config))
                      :limit-max (cu/parse-natural-number (:limit-max app-config))
                      :obs-endpoint (str "https://" (:obs-bucket app-config) ".obs.otc.t-systems.com"))
        app-config (update app-config :gz-mime-types #(cond
                                                (true? %) #{"text/css" "text/javascript"}
                                                (and % (not-empty %)) %
                                                :else nil))]
    ;; (clojure.pprint/pprint app-config)
    app-config))
