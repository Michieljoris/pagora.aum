(ns pagora.aum.modules.db-migration.joplin.core
  (:require
   [jdbc.core :as jdbc]
   [cuerdas.core :as str]))

(defn exec [db sql]
  (let [uri (str/strip (:connection-uri db) "jdbc:")]
    (with-open [conn (jdbc/connection uri)]
      (jdbc/execute uri [sql]))))

(defn fetch [db sql]
  (let [uri (str/strip (:connection-uri db) "jdbc:")]
    (with-open [conn (jdbc/connection uri)]
      (jdbc/execute uri [sql]))))
