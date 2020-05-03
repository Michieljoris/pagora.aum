(ns pagora.aum.database.adapter.clojure-java-jdbc
  (:gen-class)
  (:require [hugsql.adapter :as adapter]
            [cuerdas.core :as str]
            [taoensso.timbre :as timbre]
            [clojure.java.jdbc :as jdbc]))

(deftype HugsqlAdapterClojureJavaJdbc []

  adapter/HugsqlAdapter
  (execute [this db sqlvec options]
    (if (some #(= % (:command options)) [:insert :i!])
      (jdbc/db-do-prepared-return-keys db sqlvec)
      (apply jdbc/execute! db sqlvec (:command-options options))))

  (query [this db sqlvec {:keys [fn-name max-execution-time] :as options}]
    (let [add-max-exec-time-hint? (and (contains? #{'get-cols-from-table 'get-joined-rows} fn-name)
                                       (string? (first sqlvec)))

          sqlvec (cond-> sqlvec
                   add-max-exec-time-hint?
                   (update 0 #(str/replace %  #"^\s*select"
                                           (str "select /*+ MAX_EXECUTION_TIME(" (or max-execution-time 5000) ") */"))))]

      ;; (when add-max-exec-time-hint?
      ;;   (timbre/info "MAX_EXECUTION_TIME set for query:" max-execution-time "ms"))

      (apply jdbc/query db sqlvec (:command-options options))))

  (result-one [this result options]
    (first result))

  (result-many [this result options]
    result)

  (result-affected [this result options]
    (first result))

  (result-raw [this result options]
    result)

  (on-exception [this exception]
    (throw exception)))

(defn aum-hugsql-adapter-clojure-java-jdbc []
  (->HugsqlAdapterClojureJavaJdbc))

;; (str/replace "select foo" #"^\s*select" "select /*+ MAX_EXECUTION_TIME(1000) */")
