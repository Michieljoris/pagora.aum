(ns pagora.aum.database.jdbc-joda-time
  "Copy of: https://github.com/clj-time/clj-time/blob/master/src/clj_time/jdbc.clj
  in terms of functionality (see
  https://github.com/clj-time/clj-time#clj-timejdbc) however

  clojure.java.jdbc protocol extensions supporting DateTime coercion.
  To use in your project, just require the namespace:
    => (require 'database.jdbc-clj-time)
    nil
  Doing so will extend the protocols defined by clojure.java.jdbc, which will
  cause java.sql.Timestamp objects in JDBC result sets to be coerced to
  org.joda.time.DateTime objects, and vice versa where java.sql.Timestamp
  objects would be required by JDBC."
  (:require [clj-time.coerce :as tc]
            [taoensso.timbre :as timbre :refer [info]]
            [clojure.java.jdbc :as jdbc]))

(defn ^:dynamic result-set-read-column-timestamp [v]
  ;; (timbre/info :#pp {:timestamp v})
  (tc/from-sql-time v))

(defn ^:dynamic result-set-read-column-date [v]
  ;; (timbre/info :#pp {:date v})
  (tc/from-sql-date v))

(defn ^:dynamic result-set-read-column-time [v]
  ;; (timbre/info :#pp {:time v})
  (org.joda.time.DateTime. v))

; http://clojure.github.io/java.jdbc/#clojure.java.jdbc/IResultSetReadColumn
(extend-protocol jdbc/IResultSetReadColumn
  java.sql.Timestamp
  (result-set-read-column [v _2 _3]
    (result-set-read-column-timestamp v))
  java.sql.Date
  (result-set-read-column [v _2 _3]
    (result-set-read-column-date v))
  java.sql.Time
  (result-set-read-column [v _2 _3]
    (result-set-read-column-time v)))

; http://clojure.github.io/java.jdbc/#clojure.java.jdbc/ISQLValue
(extend-protocol jdbc/ISQLValue
  org.joda.time.DateTime
  (sql-value [v]
    (tc/to-sql-time v)))
