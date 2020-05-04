(ns pagora.aum.modules.auth.subscriptions.time
  (:require
   [clj-time.core :as t]
   [clj-time.local :as l]
   [clj-time.format :as f]
   [clj-time.coerce :as c]
   [clj-time.coerce :as tc]
   ))

(defn ^{:dynamic true} now []
  (t/plus (t/now) (t/days 0)))

(defn equal-or-after? [t1 t2]
  (or (t/equal? t1 t2) (t/after? t1 t2)))

(defn equal-or-before? [t1 t2]
  (or (t/equal? t1 t2) (t/before? t1 t2)))

(defn to-local-date-in-tz
  "Converts a date-time to a local-date in time-zone. Returns nil if
  unsuccessful."
  [date-time time-zone]
  (try
    (let [tz (cond
               (number? time-zone) (t/time-zone-for-offset time-zone)
               (string? time-zone) (t/time-zone-for-id time-zone)
               :else time-zone)

          ;;Adjusts the time zone and adjusts the date (and time) if needed.
          ;;This is still the same instant in time.
          date-time-with-tz (t/to-time-zone date-time tz)]

      ;;Drop all time zone info
      (c/to-local-date date-time-with-tz))
    (catch Exception e
      nil)))


(defn parse-date-string [s]
  (try
    (c/to-local-date (f/parse (f/formatter "yyyy-MM-dd") s))
    (catch Exception e
      :invalid-date)))


(defn sql-date-to-local-date
  "Converts a java.sql.Date to a joda local date if it isn't already of that type,
  returns nil is passed in nil, or :invalid-date is not a valid date"
  [sql-date]
  (cond
    (nil? sql-date) nil
    (instance? org.joda.time.LocalDate sql-date) sql-date
    (instance? java.sql.Date sql-date ) (c/to-local-date (c/from-sql-date sql-date))
    (string? sql-date) (parse-date-string sql-date)
    :else :invalid-date))

;; (sql-date-to-local-date nil)
;; (sql-date-to-local-date "2000-10-10")
;; (def d (sql-date-to-local-date  #inst "2019-10-10T00:00:00.000-00:00"))
;; => #object[org.joda.time.LocalDate 0x64d2cbd5 "2019-10-10"]
;; (sql-date-to-local-date  d)
;; => #object[org.joda.time.LocalDate 0x7c7d929d "2019-10-10"]

(defn today-in-tz [time-zone-id]
  (to-local-date-in-tz (now) time-zone-id))

;; (today-in-tz 10)
;; => #object[org.joda.time.LocalDate 0x68ff8286 "2019-12-11"]
;; (c/to-local-date (t/now))
;; => #object[org.joda.time.LocalDate 0x5e382726 "2019-12-12"]
;; (t/now)
;; => #object[org.joda.time.DateTime 0x1823933e "2019-12-12T07:50:35.265Z"]
;; (t/to-time-zone (t/now) (t/time-zone-for-offset 10))
;; => #object[org.joda.time.DateTime 0x5a5a9ffa "2019-12-12T17:39:16.849+10:00"]
;; (t/available-ids)
