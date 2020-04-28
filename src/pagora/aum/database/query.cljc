(ns pagora.aum.database.query
  (:require
   #?@(:clj
       [[clojure.java.jdbc :as jdbc]])
   [pagora.aum.database.build-sql :as build-sql]
   [pagora.clj-utils.core :as cu]
   [cuerdas.core :as str]
   [taoensso.timbre :as timbre :refer [info]]))

(defn cols->cols-hyphened [cols]
  (mapv (comp cu/hyphen->underscore name) cols))

(defn dispatch-on-fun [env fun params & args]
  fun)

(defmulti validate-sql-fun dispatch-on-fun)

;; Extendable. Receives params after being processed by aum-process-params
;; first
(defmulti process-params
  "Receives env, fun and params. Should return a params map again. By
  default returns params as is"
  dispatch-on-fun)

;;By default params is not processed at all.
(defmethod process-params :default
  [_ _ params] params)

;;Override and/or extend to change how aum processes params
(defmulti aum-process-params
  "Receives env, fun and params. Should return a params map again. By
  default returns params as is"
  dispatch-on-fun)

;;By default params is not processed at all.
(defmethod aum-process-params :default
  [_ _ params] params)

(defmulti process-result
  "Receives env, fun, params and result.Should return some kind of
  result again. By default returns result as is."
  dispatch-on-fun)

(defmethod process-result :default
  [env _ params result]
  result)

(defmulti aum-process-result
  "Receives env, fun, params and result.Should return some kind of
  result again. By default returns result as is."
  dispatch-on-fun)

(defmethod aum-process-result :default
  [env _ params result]
  result)

;; (ns-unmap *ns* 'table-hook)
(defmulti table-hook
  "Receives env, fun, params and processed result. Dispatches on fun
  and table if set in params. Return is ignored. Meant for side effects "
  (fn [env fun params & args]
    ;; (timbre/info "dispatching" [fun (:table-keyword params)])
    [fun (:table-keyword params)]))

(defmethod table-hook :default [& args])

#?(:clj
   (defn log-sql [{:keys [fun params aum-hugsql-ns hugsql-ns print-only parser-config] :as args}]
     (when (or (:sql-log parser-config) print-only)
       (let [fun-sqlvec (str (name fun) "-sqlvec")
             fun-sqlvec-ref (build-sql/resolve-sql-fun fun-sqlvec hugsql-ns aum-hugsql-ns)
             sqlvec (fun-sqlvec-ref params)
             sql-str (str/replace (first sqlvec) "\n" " ")
             sql-str (str/replace sql-str "  " " ")
             sql-seq (conj (rest sqlvec) sql-str)]
         (info :#g (str/join " " sql-seq))
         )))
   :cljs
   (defn log-sql [{:keys [fun params aum-hugsql-ns hugsql-ns print-only parser-config] :as args}]
     (timbre/info :#g (build-sql/build-sql fun params))))

;;Specifically for make-query-params fn
;;TODO-MERGE: this fn takes into account :pluralize-table-names? and returns a string table as is.
;; also it looks in the schema for the table-name, instead of just in db-config
;; (defn table-name
;;   "Table names are singular hyphenated and keywords. This fn returns
;;   the actual table name by looking it up in db-config.schema in env or
;;   otherwise just adds an 's'. If table is already a string it is
;;   returned as is.Returns a string or nil."
;;   [{:keys [db-config] :as env} table]
;;   (cond
;;     (keyword? table) (-> (or (get-in db-config [:schema table :table-name])
;;                              (if (:pluralize-table-names? db-config)
;;                                (str (name table) "s")
;;                                table))
;;                          name
;;                          du/hyphen->underscore)
;;     (string? table) table))

;;TODO-MERGE: this fn is not used anywhere?
;; (defn make-query-params
;;   "Takes all the elements need to build and query and returns a data
;;   map ready to pass to a hugsql query"
;;   [env {:keys [table cols where limit order-by updates]}]
;;   (let [table-name (table-name env table)]
;;     {:table table
;;      :cols cols
;;      :updates updates
;;      :where-clause (when where (db-clauses/conds->sqlvec {:table table
;;                                                           :table-name table-name
;;                                                           :alias-prefix ""
;;                                                           :props nil
;;                                                           :cols (db-clauses/conds->sqlvec {:table table
;;                                                                                            :table-name table-name
;;                                                                                            :alias-prefix ""
;;                                                                                            :props nil
;;                                                                                            :cols nil
;;                                                                                            :conds where})
;;                                                           :conds where}))
;;      :limit-clause (when limit (db-clauses/make-limit-clause limit nil))
;;      :order-by-clause (when order-by (db-clauses/order-by->sqlvec {:table table
;;                                                                    :table-name table-name
;;                                                                    :alias-prefix ""
;;                                                                    :cols nil
;;                                                                    :order-by order-by}))}))

(defn sql
  "Executes fun with db connection from env as second argument Add a
  map under the sql key in env to replace aum processing of params,
  results of sql fns and aum-hugsql-ns. For extra or to override
  individual sql fns set hugsql-ns. Add methods for process-params,
  process-result to do extra processing for aum sql fns (they're
  called after aum-process-params/results) or your own sql fns. It's
  also possible to add/override methods for aum-process-params and
  aum-process-results. Before the actual sql call is made the
  multimethod validate-sql-fun is called. For the standard aum-sql
  fns an appropriate validating fn is found using the :db-config as
  set in the env and by calling security/get-validation-fun. This will
  also throw an exception if no validation fn is found. For any extra
  hugsql fns a validate-sql-fun method will need to be defined, since
  it will throw an exception by default.

  It is possible to use this fn to just call (and possibly log) sql
  fns with complete custom processing of params, results and custom
  validations by passing in fns for all keys starting with aum-.
  However in practice it'll be probably enough to extend the various
  multimethods (process-params, process-result and validate-sql-fun).

  Minimal env param:
{:db-conn some-db-conn
 :schema (schema/get-schema db-conn)
 :user {:id 1 :group-id 10}
 :db-config some-db-config
 :parser-config {:sql-log true}
 ;;optional
 :sql {:aum-process-params your-process-params
       :aum-process-result your-process-result
       :aum-hugsql-ns \"your-hugsql-fns-ns\"
       :aum-validate-sql-fun your-validate-sql-fun
       :hugsql-ns \"more-hugsql-fns-ns\"}
 }

 Fun param can be a keyword.
"
  ([env fun] (sql env fun {} false))
  ([env fun params] (sql env fun params false))
  ([{:keys [parser-config db-conn]
     {:keys [aum-process-params
             aum-process-result
             aum-validate-sql-fun
             aum-hugsql-ns hugsql-ns
             jdbc-result-set-read-column jdbc-sql-value]
      :or {aum-process-params aum-process-params
           aum-process-result aum-process-result
           aum-validate-sql-fun validate-sql-fun
           aum-hugsql-ns "pagora.aum.database.queries"
           #?@(:clj
              [jdbc-result-set-read-column jdbc/result-set-read-column
               jdbc-sql-value jdbc/sql-value])}
      } :sql
     :as env} fun params print-only]
   (let [fun (or (:fun params) fun) ;if we decided to use a alternative fun, use that
         params (->> params
                     (aum-process-params env fun)
                     (process-params env fun))
         fun-ref (build-sql/resolve-sql-fun fun hugsql-ns aum-hugsql-ns)]
     ;; (timbre/info fun params)
     (aum-validate-sql-fun env fun params)
     (log-sql {:fun fun :params params :aum-hugsql-ns aum-hugsql-ns :hugsql-ns hugsql-ns
               :print-only print-only :parser-config parser-config})
     (if-not print-only
       (let [result
             #?(:clj (with-redefs [jdbc/result-set-read-column jdbc-result-set-read-column
                                   jdbc/sql-value jdbc-sql-value]
                       ;; Actual call to sql fn.
                       (fun-ref db-conn params)))
             #?(:cljs
                (do
                  ;; (timbre/info :#pp params)
                  (fun-ref db-conn params)
                  )
                )]
         ;; (timbre/info :#pp result)
         (let [result (->> result
                           (aum-process-result env fun params)
                           (process-result env fun params))]
           (table-hook env fun params result)
           result))))))
