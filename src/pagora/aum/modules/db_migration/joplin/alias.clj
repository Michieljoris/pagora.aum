(ns pagora.aum.modules.db-migration.joplin.alias
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [clojure.pprint :refer [pprint]]
            [joplin.repl :as repl]))

;; https://github.com/juxt/joplin

;;TODO-aum: validation and useful error reporting

(defn- load-config' [path]
  (-> (io/resource path)
      repl/load-config))

(def ^:dynamic *load-config* load-config')

;; config file is the filename of a joplin configuration which has to recide
;; somewhere in the resources.

(defn joplin-do
  ([action options] (joplin-do action options nil))
  ([action {:keys [config env db id num] :as options} args]
   (pprint {:action action :options options :args args})
   (let [action (keyword action)
         env (keyword env)
         db (keyword db)
         conf (*load-config* config)
         action-fn (get {:migrate #(if db
                                     (repl/migrate conf env db)
                                     (repl/migrate conf env))
                         :seed #(apply repl/seed conf env db args)
                         :rollback-n #(when (and db num)
                                        (repl/rollback conf env db (Long/parseLong num)))

                         :rollback-id #(when (and db id)
                                         (println "rolling back:" id) ;;TODO-aum:doesn't seem to be working
                                         (println (repl/rollback conf env db id)))
                         :reset #(when db
                                   (repl/reset conf env db args))
                         :pending #(when db
                                     (repl/pending conf env db))
                         :create (fn create []
                                   (pprint {:creating id :in db})
                                   (when (and db id)
                                     (repl/create conf env db id)))}
                        action)]
     (action-fn))))

(def cli-options
  [["-a" "--action ACTION" "migrate, seed, rollback, reset, pending or create"]
   ["-c" "--config CONFIG" "EDN resource with joplin configuration." :default "joplin.edn"]
   ["-e" "--env ENVIRONMENT" "dev, staging, prod or test" :default "dev"]
   ["-d" "--db DATABASE" "database to do migration on"]
   ["-n" "--num NUM" "number of migrations to rollback" :default "1"]
   ["-i" "--id ID" "name of migration to create"]
   ["-h" "--help"]])

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments summary errors]}(cli/parse-opts args cli-options)
        action (first arguments)
        args (rest arguments)]
    (cond
      (= true (:help options)) (exit 0 summary)
      :else (do (joplin-do action options args)
                (System/exit 0)))))
(comment
  (joplin-do :migrate {:config "joplin.edn" :env :dev} "foo")
  (joplin-do :pending {:config "joplin.edn" :env :dev :db :aum-minimal})
  (joplin-do :rollback-n {:config "joplin.edn" :env :dev :db :aum-dev :num "1"})
  ;; (joplin-do :rollback-id {:config "joplin.edn" :env :dev :db :sql-minimal :id "20200330150018-create-accounts"})

  (joplin-do :seed {:config "joplin.edn" :env :dev :db :aum-dev} ["seed1"])
  (joplin-do :reset {:config "joplin.edn" :env :dev :db :aum-minimal})
  )
