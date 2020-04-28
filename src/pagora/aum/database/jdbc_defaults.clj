(ns pagora.aum.database.jdbc-defaults)


;;We want to make sure we're using the default implementation for this.
;;Particularly we don't want any dates to be transformed, for instance into joda
;;time dates.
;;https://github.com/clojure/java.jdbc/blob/0ea6110529b04337dc4bc8e6c23bc9f566027e45/src/main/clojure/clojure/java/jdbc.clj#L376
(defprotocol IResultSetReadColumnDefault
  "Protocol for reading objects from the java.sql.ResultSet. Default
   implementations (for Object and nil) return the argument, and the
   Boolean implementation ensures a canonicalized true/false value,
   but it can be extended to provide custom behavior for special types."
  (result-set-read-column [val rsmeta idx]
"Function for transforming values after reading them from the database"))

(extend-protocol IResultSetReadColumnDefault
  Object
  (result-set-read-column [x _2 _3] x)

  Boolean
  (result-set-read-column [x _2 _3] (if (= true x) true false))

  nil
  (result-set-read-column [_1 _2 _3] nil))

;; (extend-protocol IResultSetReadColumnDefault
;;   Object
;;   (result-set-read-column [x _2 _3] x)

;;   Boolean
;;   (result-set-read-column [x _2 _3] (if (= true x) true false))

;;   nil
;; (result-set-read-column [_1 _2 _3] nil))

;;https://github.com/clojure/java.jdbc/blob/0ea6110529b04337dc4bc8e6c23bc9f566027e45/src/main/clojure/clojure/java/jdbc.clj#L336
(defprotocol ISQLValue
  "Protocol for creating SQL values from Clojure values. Default
   implementations (for Object and nil) just return the argument,
   but it can be extended to provide custom behavior to support
   exotic types supported by different databases."
  (sql-value [val] "Convert a Clojure value into a SQL value."))

(extend-protocol ISQLValue
  Object
  (sql-value [v] v)

  nil
(sql-value [_] nil))
