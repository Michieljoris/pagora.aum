(ns pagora.aum.database.query
  (:require
   #?@(:clj
       [[clojure.java.jdbc :as jdbc]
        [pagora.aum.database.jdbc-joda-time :as jdbc-joda-time]])
   [pagora.aum.database.build-sql :as build-sql]
   [pagora.clj-utils.core :as cu]
   [cuerdas.core :as str]
   [clojure.pprint :refer [pprint]]
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
         sqlvec
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

(defn adjust-for-tz
  "When db server is not set to utc we correct for the timezone"
  [v]
  (- (.getTime v)
     (* 60000 (.getTimezoneOffset v))))

(defn wrap-with-bindings [datetime-return-type f]
  (case datetime-return-type
    :joda (f)
    (f)
    ;; (with-bindings
    ;;   {#'pagora.aum.database.jdbc-joda-time/result-set-read-column-timestamp
    ;;    ;;mysql types: TimeStamp and DateTime
    ;;    #(new java.sql.Timestamp (adjust-for-tz %))
    ;;    #'pagora.aum.database.jdbc-joda-time/result-set-read-column-date
    ;;    ;;mysql type: Date
    ;;    #(new java.sql.Date (adjust-for-tz %))
    ;;    #'pagora.aum.database.jdbc-joda-time/result-set-read-column-time
    ;;    ;;mysql type: Time
    ;;    #(new java.sql.Time(adjust-for-tz %))}
    ;;   (f))
    ))

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
  ([{:keys [parser-config]
     {:keys [aum-process-params
             aum-process-result
             aum-validate-sql-fun
             aum-hugsql-ns hugsql-ns
             datetime-return-type]
      :or {aum-process-params aum-process-params
           aum-process-result aum-process-result
           aum-validate-sql-fun validate-sql-fun
           aum-hugsql-ns "pagora.aum.database.queries"
           dateitme-return-type :joda}
      } :sql
     :as env} fun params print-only]
   (let [fun (or (:fun params) fun) ;if we decided to use a alternative fun, use that
         {:keys [noop] :as params} (->> params
                                        (aum-process-params env fun)
                                        (process-params env fun))]

     (if noop
       noop
       (let [_ (aum-validate-sql-fun env fun params)
             sql-vec (log-sql {:fun fun :params params :aum-hugsql-ns aum-hugsql-ns :hugsql-ns hugsql-ns
                               :print-only print-only :parser-config parser-config})
             process-result-fn #(->> %2
                                     (aum-process-result %1 fun params)
                                     (process-result %1 fun params))]
         (if print-only
           sql-vec
           (let [fun-ref (build-sql/resolve-sql-fun fun hugsql-ns aum-hugsql-ns)
                 result
                 #?(:clj (jdbc/with-db-transaction [tx (:db-conn env)]
                           (when (:simulate? params)
                             (timbre/info :#b "Simulating..")
                             (jdbc/db-set-rollback-only! tx))
                           (try
                             (let [{:keys [db-conn] :as env} (assoc env :db-conn tx)
                                   result (process-result-fn env
                                                             (wrap-with-bindings datetime-return-type
                                                                                 #(fun-ref db-conn params)))]
                               (table-hook env fun params result)
                               result)
                             (catch Exception e
                               (timbre/info :#r "An exception was thrown while trying to execute an sql query (either by query itself or any hooks)."
                                            "No change has been made to the database.")
                               (throw e)))))
                 #?(:cljs
                    (do
                      (let [result (process-result-fn env (fun-ref db-conn params))]
                        (table-hook env fun params result)
                        result)))]
             result)))))))


;; Debug/test:
;; (comment
;;   (require '[pagora.clj-utils.database.connection :as db-connection])
;;   (require '[pagora.aum.database.schema :as schema])
;;   (def db-conn (db-connection/make-db-connection {:url "//localhost:3306/"
;;                                                   :db-name "aum_minimal"
;;                                                   ;; :db-name "aum_development"
;;                                                   :print-spec true
;;                                                   :use-ssl false
;;                                                   :user "root"
;;                                                   :password ""}))
;;   (def env {:db-conn db-conn
;;             :schema (schema/get-schema db-conn)
;;             :user {:id 1 :group-id 10 :subgroup-ids [2 3] :role "master-admin"}
;;             :db-config {:event-store {:table-name :event-store
;;                                       :delete {:scope [:name := 100]}
;;                                       :update {:scope [:or [[:group-id := :u/group-id]
;;                                                             [:group-id :in :u/subgroup-ids]]]
;;                                                :whitelist [:id :name]}
;;                                       }}
;;             :sql {:hugsql-ns "database.queries"}
;;             :parser-config {:sql-log true
;;                             :event-store-disabled true}})
;;   ;; (sql env :insert-record  {:table :group
;;   ;;                           :skip-validate? true
;;   ;;                           :mods {:name "bar"}
;;   ;;                           })
;;   ;; (sql env :delete-record  {:table :event-store
;;   ;;                           :skip-validate? true
;;   ;;                           :id 3
;;   ;;                           :scope "foo"
;;   ;;                           })
;;   (pprint (sql env :search-translations {:group-id -1
;;                                          ;; :order-by-clause (db-clauses/make-order-by-clause {:order-by [[:t1.id :asc]]})
;;                                          :where-clause (let [clause
;;                                                              (db-clauses/make-where-clause {:table-name "t1" :cols [:t1.id :t1.key] :where [:t1.key := "a1"]})]
;;                                                          (into [(subs (first clause) 6)] (rest clause))
;;                                                          )

;;                                          :limit 5})))

;; (let [clause
;;       (db-clauses/make-where-clause {:table-name "t1" :cols [:t1.group_id :t2.translation_id:a.b] :where [:and  [[:t1.group_id := 2][:t1.group_id := 1]]]})]
;;   (into [(subs (first clause) 6)] (rest clause))
;;   )
;; (db-clauses/make-where-clause {:table-name "t1a" :cols [:t1.group_id :t2.translation_id:a.b] :where [:and  [[:t1.group_id := 2][:a.b := 1]]]})
;; ;; => ["where (`t1a`.`a` = ? AND `t1a`.`a` = ?)" 2 1]

;;  where t1.group_id is null and ((t2.translation_id = t1.id and t2.group_id = :group-id) or (t2.translation_id is null and t2.id = t1.id))



;; Obsolete

;; (defn _process-params
;;   "Basically lets you use keywords for tables and cols. Table keywords
;;   are expected to be singular and hyphenated. Also makes sure only the
;;   specified sql fns can be executed"
;;   [env fun {:keys [table mods cols] :as params}]
;;   (let [cols (cols->cols-hyphened cols)]
;;     (condp = fun
;;       :get-cols-from-table
;;       (assoc params
;;              :table (db-inspect/table-name env (:table params))
;;              :cols cols)
;;       :get-joined-rows
;;       ;; When t1==t2 aliases kick in, see hug.sql
;;       (let [t1 (:t1 params)
;;             t2 (:t2 params)
;;             t1-name (db-inspect/table-name env t1)
;;             t2-name (db-inspect/table-name env t2)
;;             t1=t2? (= t1 t2)
;;             [t1-alias t2-alias] (if t1=t2?
;;                                   [(str "t1_" t1-name) (str "t2_" t2-name)]
;;                                   [t1-name t2-name])
;;             t1-foreign-key (du/keyword->underscored-string (:t1-foreign-key params))
;;             t2-foreign-key (du/keyword->underscored-string (:t2-foreign-key params))
;;             cols (map #(str (if t1=t2? t2-alias t2-name) "." %) cols)
;;             cols (conj cols (str (:join-table params) "." t1-foreign-key))]
;;         (assoc params
;;                :t1-name t1-name :t1-alias t1-alias
;;                :t2-name t2-name :t2-alias t2-alias
;;                :t1=t2? t1=t2?
;;                :cols cols
;;                :t1-foreign-key t1-foreign-key
;;                :t2-foreign-key t2-foreign-key))
;;       :count-belongs-to {:table (db-inspect/table-name env (:table params))
;;                          :belongs-to-column (du/keyword->underscored-string (:belongs-to-column params))
;;                          :id (:id params)
;;                          :cond (:cond params)}
;;       :get-now params

;;       ;;Mutations
;;       :insert-record (let [{:keys [cols vals]} (du/map->keys-and-vals mods)
;;                            cols (cols->cols-hyphened cols)]
;;                        {:table (db-inspect/table-name env table) :cols cols :vals vals
;;                         :no-timestamp? (db-inspect/no-timestamp? env table)})
;;       :update-record {:table (db-inspect/table-name env (:table params)) :id (:id params)
;;                       :cols cols :vals (:vals params) :no-timestamp? (db-inspect/no-timestamp? env (:table params))}
;;       :delete-record {:table (db-inspect/table-name env (:table params)) :id (:id params)}
;;       :bulk-update (let [{:keys [table where where-cols updates no-timestamp?]} params
;;                          table-name (db-inspect/table-name env table)]
;;                      {:table table-name
;;                       :no-timestamp? (boolean no-timestamp?)
;;                       :cols (-> updates keys cols->cols-hyphened)
;;                       :vals (-> updates vals vec)
;;                       :where-clause (clauses/make-where-clause {:table-name table-name
;;                                                                 :cols where-cols
;;                                                                 :where where})})
;;       :insert-event {:table (db-inspect/table-name env (:table params)) :cols cols :vals (:vals params)}
;;       (throw (ex-info"Unknown sql function" {:fun fun})))))


;; (defn aum-process-result
;;   "Transforms cols of rows back to hyphenated keywords. Also for some
;;   other queries that return something other than rows, make sure the
;;   result is meaningful "
;;   [env fun params result]
;;   (condp = fun
;;     ;; http://www.hugsql.org/#using-insert
;;     :get-now (:now result)
;;     :update-record result
;;     :insert-record (:generated_key result) ;NOTE: generated_key  only works for mysql
;;     :insert-event (:generated_key result) ;NOTE: generated_key  only works for mysql
;;     :delete-record (when (= 1 result) result) ;;return nil if nothing is deleted
;;     :bulk-update (first result)
;;     (du/transform-keys (comp keyword du/underscore->hyphen name) result)))

;; (defn process-params [{{:keys [process-params-plus]
;;                         :or {process-params-plus (constantly nil)}
;;                         } :sql
;;                        :as env} fun params]
;;   (or (process-params-plus env fun params)
;;       (_process-params env fun params)))


;; (defn process-result [{{:keys [process-result-plus]
;;                         :or {process-result-plus (constantly nil)}
;;                         } :sql
;;                        :as env} fun params result]
;;   (or (process-result-plus env fun params result)
;;       (_process-result env fun params result)))



