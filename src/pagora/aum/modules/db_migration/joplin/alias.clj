(ns pagora.aum.modules.db-migration.joplin.alias
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [jdbc.core :as jdbc]
            [pagora.aum.config :as config]
            [stch.sql.ddl :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [joplin.core :as joplin]
            [joplin.repl :as repl]
            [cuerdas.core :as str]))

;; https://github.com/juxt/joplin

;;TODO-aum: validation and useful error reporting

(defn- load-config' [path]
  (-> (io/resource path)
      repl/load-config))

(def ^:dynamic *load-config* load-config')

;; config file is the filename of a joplin configuration which has to recide
;; somewhere in the resources.

(defn do-create-migration
  "Create a scaffold migrator file"
  [prefix target id ns]
  (when (:migrator target)
    (let [migration-id (joplin/get-full-migrator-id id)
          ns-name (string/replace migration-id "_" "-")
          target-parts (-> (:migrator target) (string/split #"/") rest)
          path (str prefix "/" (string/join "/" target-parts) "/"
                    (string/replace migration-id "-" "_")
                    ".clj")]
      (println "creating" path)
      (try
        (spit path (format "(ns %s
(:require
   [pagora.aum.modules.db-migration.joplin.core :refer [exec fetch]]
   [stch.sql.ddl :refer :all]))

;; https://stch-library.github.io/sql/

(defn up [db]
  (exec db :todo-sql-here))

(defn down [db]
  (exec db :tod-sql-here))

" (apply str (interpose "." (concat target-parts [ns-name])))))
        (catch Exception e
          (println "Error creating file" path))))))


(defn  make-url [{:keys [db-name db-user db-password db-options db-url]}]
  (str "mysql:" db-url db-name "?user=" db-user "&password=" db-password db-options))

(defn create-migrations-table [conn migrations-table]
  (let [sql (create
             (-> (table migrations-table)
                 (varchar :id [255])
                 (varchar :created_at [32]))
             (engine :InnoDB)
             (collate :utf8-general-ci))]
    (jdbc/execute conn [sql])))

(defn get-db-name [conf db]
 (let [{:keys [db-spec]} (-> conf :databases db)
       {:keys [db-name]} db-spec]
   db-name))

(defn recreate-db [conf db]
  (let [
        {:keys [db-spec migrations-table]} (-> conf :databases db)
        {:keys [db-name]} db-spec
        mysql-url (make-url (assoc db-spec :db-name "mysql"))]
    (println "Recreating" db-name)
    (with-open [conn (jdbc/connection mysql-url)]
      (jdbc/execute conn [(str "DROP DATABASE IF EXISTS " db-name)])
      (jdbc/execute conn [(str "CREATE DATABASE IF NOT EXISTS " db-name)]))
    (with-open [conn (jdbc/connection (make-url db-spec))]
      (create-migrations-table conn migrations-table))))

(defn set-url [databases]
  (into {} (->> databases
                (map (fn [[k {:keys [type db-spec] :as v}]]
                       (if (contains? #{:sql :jdbc} type)
                         (let [db-spec (config/config-from-env db-spec)]
                           {k (assoc v :url (str "jdbc:" (make-url db-spec))
                                     :db-spec db-spec)})
                         [k v]))))))

(defn print-safe [s]
  (let [lines (string/split s #"\n")]
    (doseq [line lines]
      (when-not (str/includes? line "ragtime.jdbc.SqlDatabase")
        (println line)))))

(defn joplin-do
  ([action options] (joplin-do action options nil))
  ([action {:keys [config env db id num] :as options} args]
   (let [action (keyword action)
         env (keyword env)
         db (keyword db)
         conf (*load-config* config)
         conf (update conf :databases set-url)
         db-name (get-db-name conf db)
         _ (when db-name (println "Database:" db-name))
         action-fn (get {:migrate #(if db
                                     (-> (repl/migrate conf env db)
                                         with-out-str print-safe )
                                     (-> (repl/migrate conf env)
                                         with-out-str print-safe))
                         :seed #(apply repl/seed conf env db args)
                         :rollback-n #(when (and db num)
                                        (-> (repl/rollback conf env db (Long/parseLong num))
                                            with-out-str print-safe))
                         :rollback-id #(when (and db id)
                                         (println "rolling back:" id) ;;TODO-aum:doesn't seem to be working
                                         (println (repl/rollback conf env db id)))
                         :reset #(when db
                                   (-> (repl/reset conf env db args)
                                       with-out-str print-safe))
                         :pending #(when db
                                     (repl/pending conf env db))
                         :create #(when (and db id)
                                    (with-redefs [joplin/do-create-migration (partial do-create-migration (:path conf))]
                                      (repl/create conf env db id)))
                         :rebuild (fn []
                                    (when db
                                      (recreate-db conf db)
                                      (-> (repl/migrate conf env db)
                                          with-out-str print-safe)
                                      (-> (apply repl/seed conf env db args)
                                          with-out-str print-safe)))}
                        action)]
     (action-fn))))

(def cli-options
  [["-a" "--action ACTION" "migrate, seed, rollback, reset, pending, rebuild or create"]
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
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args cli-options)
        action (first arguments)
        args (rest arguments)]
    (cond
      (= true (:help options)) (exit 0 summary)
      :else (do (joplin-do action options args)
                (System/exit 0)))))
(comment
  (joplin-do :migrate {:config "joplin.edn" :env :dev :db :aum-minimal})
  (joplin-do :pending {:config "joplin.edn" :env :dev :db :aum-minimal})
  (joplin-do :rollback-n {:config "joplin.edn" :env :dev :db :aum-minimal :num "1"})
  ;; (joplin-do :rollback-id {:config "joplin.edn" :env :dev :db :sql-minimal :id "20200330150018-create-accounts"})

  (joplin-do :seed {:config "joplin.edn" :env :dev :db :aum-dev} ["seed1"])
  (joplin-do :reset {:config "joplin.edn" :env :dev :db :aum-minimal})
  (joplin-do :create {:config "joplin.edn" :env :dev :db :aum-minimal :id "my-new-migration"})
  (joplin-do :rebuild {:config "joplin.edn" :env :dev :db :aum-minimal} ["seed1"])

  )
