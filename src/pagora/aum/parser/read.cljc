(ns pagora.aum.parser.read
  (:refer-clojure :exclude [read])
  (:require
   [pagora.aum.database.clauses :as clauses]
   [pagora.aum.parser.calc-meta-data :refer [calc-meta-data]]
   [pagora.aum.database.inspect :as db-inspect]
   [pagora.aum.database.query :refer [sql]]
   [pagora.aum.security :as security]
   [pagora.clj-utils.core :as cu]

   [pagora.aum.om.util :as om-util]

   [clojure.set :refer [difference]]
   [cuerdas.core :as str]
   [taoensso.timbre :as timbre :refer [spy error info warn]]
))

;;TODO:
;;- get-scope isn't used
;;- :derive-table-and-column-names-from-schema true not implemented

;; Similar lib:
;; https://github.com/walkable-server/walkable

;For repl-ing
;; #?(:clj
;;    (ns-unmap *ns* 'read))
;; #?(:clj
;;    (ns-unmap *ns* 'get-rows))
;; #?(:clj
;;    (ns-unmap *ns* 'insert-join))

(defmulti read (fn [_ k _] k))

(declare get-join)
(declare row->ident)

(defn add-meta [rows meta]
  (let [with-meta? (seq (dissoc meta :query-cols))]
    (if with-meta?
      {:rows (if (:query-cols meta) rows nil)
       :meta (dissoc meta :query-cols)}
      rows)))

(defn process-ast [{ :keys [parser-config ast state user] :as env} key params]
  (if-not (sequential? (:query ast))
    (throw (ex-info "Query is not sequential" {:query (:query ast)})))
  (let [{:keys [t2-rows new-table-data]} (get-join env
                                                   {:join-ast ast :table-data {}
                                                    :user user})
        rows-meta-data (meta t2-rows)
        ;; _ (timbre/info key (into [] t2-rows) ast)
        value (if (om-util/ident? (:key ast))
                nil ;; (first t2-rows)
                (mapv (partial row->ident env key) t2-rows))
        value (add-meta value rows-meta-data)]
    (when (:normalize parser-config)
      (swap! state update :table-data #(cu/deep-merge-concat-vectors % new-table-data)))
    {:value value}))


(defn table-key? [{:keys [ast parser-config] :as env} key]
  ;; (not (and (keyword? key)
  ;;           (= (apply str (take 2 (str key))) ":#")))
  (or (db-inspect/table? env key)
      (om-util/ident? (:key ast))
      (:om-process-roots parser-config))) ;;this treats every key as a reference to a table

;;The aum read multimethod handles errors itself, and;places them in the
;;result value; at the key that caused the error
(defmethod read :default
  [{:keys [parser-config db-config state ast user parser query] :as env} key {:keys [custom-read] :as params}]
  ;; key is really dispatch-key, real key is gotten from ast
  (try
    (if (and (table-key? env key)
             ;;if any key, even if it is a table key has the custom-read param set we use it to resolve the key
             (not custom-read))
      ;;Process key as a query over a table
      (process-ast env key params)
      {:value (let [env (select-keys env (into [:state :user] (:aum-keys env)))
                    env (update env :subquery-path #(conj (or % []) key))]
                ;; (timbre/info "------ " (:subquery-path env))
                ;; (timbre/info "------ "  custom-read query params)
                (if (some? custom-read)
                  (get (parser env `[({~custom-read ~query}
                                      ~(dissoc params :custom-read))])
                       custom-read)
                  (parser env query)))})

    ;;Process subquery instead
    (catch
        #?@(:clj [Exception e])
        #?@(:cljs [:default e])
        (let [{:keys [msg context stacktrace] :as error} (cu/parse-ex-info e)]
          (when (:print-exceptions parser-config) (info e))
          (swap! state update :status (constantly :error))
          {:value {:message msg :context context :stacktrace [:not-returned]}}))))


(defmulti get-rows
  "Gets rows from t2 with only cols props. If join-type is not nil the
  specified join is fetched from t1 to t2, using t1-rows as
  originating rows. t1, t2 and cols are expected to be keywords"
  (fn [join-type env {:keys [t1 t1-rows t2 cols join-table props scope]}]
    {:pre [(every? keyword? cols)
           (or (keyword? t1) (nil? t1))
           (or (keyword? t2) (nil? t2))]}
    join-type))

(defn exec-sql-query [{:keys [parser-config] :as env} {:keys [join-type sql-fn sql-fn-args return-empty-vector? join-type] :as data}]
  ;; (timbre/info (:cols sql-fn-args))
  ;; (timbre/info (meta (:cols sql-fn-args)))
  (let [{:keys [cols]}  sql-fn-args
        query-cols (:cols (meta cols))
        _ (when (and
                 (not= query-cols :all-cols)
                 (seq query-cols)
                 (zero? (count cols))
                 (:query-log parser-config))
            (warn :#r "No columns requested for " (:t2 sql-fn-args)
                  ", so no rows fetched.\nPerhaps no blacklist or whitelist is configured for this table?."))
        rows     (if (or (nil? query-cols)
                         (and (seq? query-cols) (empty? query-cols))
                         return-empty-vector?)
                   []
                   (sql env sql-fn sql-fn-args))
        calculations (:with-meta (:params sql-fn-args))
        calculations  (when (some? calculations)
                        (if (sequential? calculations)
                          calculations
                          [calculations]))
        meta-data (reduce (fn [m calculation]
                            (let [calculation-params (if (keyword? calculation)
                                                       {:type calculation}
                                                       calculation)
                                  calculation-type (:type calculation-params)]
                              (if-let [calc-method (get-method calc-meta-data calculation-type)]
                                (assoc m calculation-type (calc-method env rows (assoc data :calculation-params
                                                                                       calculation-params)))
                                m)))
                          nil calculations)]
    (with-meta (into [] rows) (assoc meta-data :query-cols query-cols))))

(defmethod get-rows nil
  [_
   {:keys [db-config parser-config] :as env}
   {:keys [t2 where-clause cols limit-clause] :as args}]
  ;; select columns from table
  ;; (get-rows-nil args)
  (let [table-config (get db-config t2)
        {:keys [count offset]} limit-clause
        limit-max (:limit-max (if (contains? table-config :limit-max)
                                table-config parser-config))]
    (if (and (number? count) (number? limit-max)
             (> count limit-max))
      (throw (ex-info (str "Count of limit (" count ") clause is larger than max for root query of " limit-max)
                      {:table t2})))
    (if-not (or (:root table-config)
                (:allow-root parser-config))
      (throw (ex-info (str t2 " can not be used for root query") {:table t2})))

    (exec-sql-query env {:sql-fn :get-cols-from-table
                         :join-type nil
                         :sql-fn-args (assoc args
                                      :table t2
                                      :cond nil
                                      :limit-clause (clauses/make-limit-clause limit-clause limit-max)
                                      :where-clause (where-clause nil))})))

(defmethod get-rows :belongs-to
  [_ env
   {:keys [t1-rows t2 where-clause  t2-foreign-key] :as args}]
  ;; select columns from t2 where id in owner-ids
  (let [foreign-key-values (set (map t2-foreign-key t1-rows))
        cond [:id :in foreign-key-values]]

     (exec-sql-query env {:sql-fn :get-cols-from-table
                          :return-empty-vector? (zero? (count foreign-key-values))
                          :join-type :belongs-to
                          :sql-fn-args (assoc args
                                              :table t2
                                              :cond cond
                                              :where-clause (where-clause cond))})))

(defn limit [{:keys [count offset]} rows]
  (let [rows (if offset (drop offset rows) rows)]
    (if count (take count rows) rows)))

(defmethod get-rows :has-many
  [_ env
   {:keys [t1-rows t2 where-clause cols t1-foreign-key limit-clause]:as args}]
  ;; select columns from t2 where t1_id in t1-ids
  ;; (get-rows-has-many args)
  (let [cols (if (cu/includes? cols t1-foreign-key) cols (conj cols t1-foreign-key))
        t1-ids (mapv :id t1-rows)
        cond [t1-foreign-key :in t1-ids]]
    (exec-sql-query env {:sql-fn :get-cols-from-table
                         :return-empty-vector? (zero? (count t1-ids))
                         :join-type :has-many
                         :sql-fn-args (assoc args
                                             :table t2
                                             :cols cols
                                             :limit-clause nil
                                             :cond cond
                                             :t1-ids t1-ids
                                             :where-clause (where-clause cond))})))

(defmethod get-rows :many-to-many
  [_ env
   {:keys [t1 t1-rows t2 cols join-table t1-foreign-key t2-foreign-key
           where-clause cols order-by-clause limit-clause join-table-cols] :as args}]
  ;; (pprint  args)

  ;; [_ t1 t1-rows t2 cols join-table]
  ;; select columns from t1
  ;; join t1_t2 on t1.id=t1_t2.t1_id
  ;; join t2 on t2.id=t1_t2.t2_id
  ;; where t1_id in t1-ids
  (let [t1-table-id (keyword (str (db-inspect/table-name env t1) ".id"))
        t1-ids (mapv :id t1-rows)
        t1=t2? (= t1 t2)
        alias-prefix (when t1=t2? "t1_")
        t1-table-id (if t1=t2?
                      (keyword (str alias-prefix (name t1-table-id)))
                      t1-table-id)
        cond [t1-table-id :in t1-ids]]
    (exec-sql-query env {:sql-fn :get-joined-rows
                         :join-type :many-to-many
                         :return-empty-vector? (zero? (count t1-ids))
                         :sql-fn-args (assoc args
                                             :cols cols
                                             :join-table-cols join-table-cols
                                             :limit-clause nil
                                             :cond (with-meta cond
                                                     {:alias-prefix alias-prefix})
                                             :where-clause (where-clause (with-meta cond
                                                                           {:alias-prefix alias-prefix})
                                                                         t1-table-id))})))
(defn get-single-row
  "Using an ident, eg [[dossier/by-id 1]]"
  [{:keys [env table id cols scope params props whitelist]}]
  (let [where (:where params)
        cond [:id := id]]
    (sql env :get-cols-from-table {:table table :cols cols
                                   :where where
                                   :cond cond
                                   :scope scope
                                   :where-clause (clauses/make-where-clause {:table table
                                                                             :table-name (db-inspect/table-name env table)
                                                                             :alias-prefix ""
                                                                             :scope scope
                                                                             :cond cond
                                                                             :where (:where params)
                                                                             :props props
                                                                             :cols whitelist})})))

(defn maybe-ident->table [maybe-ident]
  (let [is-ident? (om-util/ident? maybe-ident)
        [t-by-id id] (if is-ident? maybe-ident [])
        t-by-id (if (and t-by-id (= (name t-by-id) "by-id")) (keyword (namespace t-by-id)) t-by-id)]
    [t-by-id id]))


(defn calc-permissions [{:keys [parser-config] :as env} table cols user]
  (let [{:keys [whitelist scope] :or {whitelist []}} (security/get-permissions env :read table user)
        cols (if (= cols :all-cols) whitelist cols)
        whitelisted-cols (filterv #(cu/includes? whitelist %) cols)
        removed-cols (difference (set cols) (set whitelisted-cols))]
    (when (and (seq removed-cols) (:query-log parser-config))
      (warn :#r "Removed blacklisted or non existing columns from query:" removed-cols))
    [whitelist whitelisted-cols scope]))

(defn extract-join-table-cols [cols join-table-kw]
  (if (= cols :all-cols)
    [cols nil]
    (let [{:keys [cols join-table-cols]}
          (group-by (fn [col] (if (and
                                   join-table-kw
                                   (= (keyword (namespace col)) join-table-kw))
                                :join-table-cols :cols))
                    cols)
          join-table-cols (mapv #(-> % name keyword) join-table-cols)]
      [cols join-table-cols])))

(defn get-unjoined-rows
  "Uses a get-rows multimethod to do the actual sql query for a bunch
  of rows.The user argument should be a map with for user props that
  can be used to whitelist columns, and set scope on rows to query
  for."
  [{:keys [parser-config] :as env}
   {:keys [join-type join-table join-table-kw t1 t1-rows t2 t2-from-ident
           id cols user params t1-foreign-key t2-foreign-key]}]
  (let [ ;; [t2-from-ident id] (maybe-ident->table t2)
        [cols join-table-cols] (extract-join-table-cols cols join-table-kw)
        [_ join-table-cols _] (calc-permissions env join-table-kw join-table-cols user)
        [whitelist whitelisted-cols scope] (calc-permissions env (or t2-from-ident t2) cols user)
        join-table-cols (remove #(or (= % :id) (cu/includes? whitelisted-cols %)) join-table-cols)
        t2-alias-prefix (if (and (= t1 t2) (= join-type :many-to-many))
                          "t2_" "")     ;aliasing for many-to-many to itself
        whitelisted-cols (with-meta whitelisted-cols {:cols cols})
        ]
    (do
      (if (and (nil? join-type) t2-from-ident)
        (get-single-row {:env env
                         :table t2-from-ident
                         :id id
                         :cols whitelisted-cols
                         :scope scope
                         :params params
                         :props user
                         :whitelist whitelist}) ;ident
        (get-rows join-type env
                  {:t1 t1
                   :t1-rows t1-rows
                   :t2 t2
                   :cols whitelisted-cols
                   :join-table-cols join-table-cols
                   :t1-foreign-key t1-foreign-key
                   :t2-foreign-key t2-foreign-key
                   :join-table join-table
                   :params params
                   :scope scope
                   :where (:where params)
                   :alias-prefix t2-alias-prefix
                   :where-clause (fn
                                   ([cond]
                                    (clauses/make-where-clause {:table t2
                                                                :table-name (db-inspect/table-name env t2)
                                                                :alias-prefix t2-alias-prefix
                                                                :scope scope
                                                                :cond cond
                                                                :where (:where params)
                                                                :props user
                                                                ;;This will exception out if in where clause a col is
                                                                ;; used that's not in whitelist, which is correct!!!
                                                                :cols whitelist}))
                                   ([cond col]
                                    (clauses/make-where-clause {:table t2
                                                                :table-name (db-inspect/table-name env t2)
                                                                :alias-prefix t2-alias-prefix
                                                                :scope scope
                                                                :cond cond
                                                                :where (:where params)
                                                                :props user
                                                                :cols (conj whitelist col)})))
                   :order-by-clause (clauses/make-order-by-clause {:table t2
                                                                   :table-name (db-inspect/table-name env t2)
                                                                   :alias-prefix t2-alias-prefix
                                                                   :order-by (:order-by params)
                                                                   :cols whitelisted-cols})
                   :limit-clause (:limit params)})))))

(defn categorize-props
  "Takes an om ast of a join and returns a map of props and joins."
  [env ast]
  {:pre [(= (:type ast) :join)]}
  (let [children (:children ast)
        {:keys [prop join]} (group-by :type children)
        props prop
        joins join
        star-key?  (= (get-in children [0 :key]) '*)
        join-info  (fn [join]
                     ;; {:post [(cu/includes? [:belongs-to :has-many :many-to-many] (:type %))]}
                     (let [[table-from-ident _] (maybe-ident->table (:key ast))
                           table (or table-from-ident (:key ast))
                           ;; table (spy :info (:key ast))
                           join-info (db-inspect/_get-join-info env table (:key join))]
                       (when (nil? join-info)
                         (throw (ex-info (str "Missing join type from " (:key ast) " to " (:key join)) join)))
                       join-info))
        joins  (->> joins
                    (map (fn [join] (let [join+ (merge join (join-info join))
                                          query (:query join+)]
                                      (if (or (number? query) (= query '...)) ;recursive query
                                        (let [recursive-join (assoc join :query (if (number? query) (dec query) query))
                                              children (map #(if (= % join) recursive-join %) children)]
                                          (assoc join+ :children children))
                                        join+))))
                    (filter #(not= (:query %) 0))) ;don't follow join if recursion limit is reached.
        props (map :key prop)
        props (when (seq children) (vec (set (conj props :id))))]
    {:props props :joins joins :star-key? star-key?}))

(defn table->ident-name [table-name]
  (let [ident? (om-util/ident? table-name)
        table-name (if ident? (first table-name) table-name)
        ident-name? (= (name table-name) "by-id")]
    (if ident-name? table-name (keyword (str (name table-name) "/by-id")))))

(defn row->ident [{:keys [parser-config]} table-name row]
  (if (:normalize parser-config)
    [(table->ident-name table-name) (:id row)]
    row))

(defn get-belongs-to-foreign-keys [joins]
  (let [belongs-to-tables (filter #(= (:join-type %) :belongs-to) joins)] ;get the something-id prop
    (map :t2-foreign-key belongs-to-tables)))

(defmulti insert-join
  "Associates in every t1 row the t2 prop with t2 rows that are joined
  to it according to the join-type."
  (fn [join-type join-info]
    join-type))

;; For importing into test. I think the ns-unmap creates:
 ;; insert-join already refers to: #'parser.read/insert-join in namespace:
 ;;   parser.read-normalize-test
(defn insert-join* [join-type join-info])

(defmethod insert-join :belongs-to
  [_ {:keys [env t1-rows t2-key t2 t2-rows t2-foreign-key t1-cols t1-props]}]
  ;; (timbre/info "inserting belongs-to join")
  (let [foreign-key t2-foreign-key
        t2-rows-meta (meta t2-rows)
        t2-rows-by-foreign-key-value
        (reduce (fn [m row]
                  (assoc m (:id row) (row->ident env t2 row))) {} t2-rows)]
    (mapv (fn [row]
            (let [foreign-key-value (get row foreign-key)
                  row (cond-> row
                        (and (not= t1-cols :all-keys)
                             (not (cu/includes? t1-props foreign-key))) (dissoc foreign-key))]
              (assoc row t2-key (add-meta (get t2-rows-by-foreign-key-value foreign-key-value)
                                  t2-rows-meta))))
          t1-rows)))

;;TODO: bit cheeky putting this here, coupling it with a calc-meta method, and should be a multimethod as well.
;;However we might need a few more "post-process-meta" uses to see and extract the pattern here.
(defn process-many-meta [{:keys [id]} {:keys [count-by-join] :as meta-data}]
  (if count-by-join
    (-> meta-data (dissoc :count-by-join) (assoc :count (or (get count-by-join id) 0)))
    meta-data))

(defn insert-many-join [{:keys [env t1 t1-rows t2-key t2 t2-rows foreign-key ;; limit-params
                                t2-cols t2-props]}]
  (let [t2-rows-meta (meta t2-rows)
        t2-rows-by-foreign-key-value (group-by foreign-key t2-rows)]
    (mapv (fn [row]
            (let [rows (get t2-rows-by-foreign-key-value (:id row))
                  ;;If actual foreign key is queried, remove it, leave inserted one
                  ;;However this has only effect when result is returned denormalized
                  ;;For normalized results this has to happen to the table-data. See get-join

                  rows (mapv (fn [row]
                               (cond-> row
                                 (and (not= t2-cols :all-keys)
                                      (not (cu/includes? t2-props foreign-key))) (dissoc foreign-key)))
                             rows)
                  ;; rows (limit limit-params rows)
                  ;; row (with-meta row {:join-ids (map :id rows)})
                  ;;TODO decouple from calc-method!!
                  t2-rows-meta (process-many-meta row t2-rows-meta)]
              (assoc row t2-key (add-meta (mapv (partial row->ident env t2) rows) t2-rows-meta))))
         t1-rows)))

(defmethod insert-join :has-many
  [_ {:keys [t1-foreign-key] :as join-info}]
  (insert-many-join (assoc join-info :foreign-key t1-foreign-key)))

(defmethod insert-join :many-to-many
  [_ {:keys [t1-foreign-key] :as join-info}]
  (insert-many-join (assoc join-info :foreign-key t1-foreign-key)))

(defn normalize-rows
  "Take a seq of rows and returns a lookup by id table"
  [{:keys [parser-config]} table-name rows]
  (when (:normalize parser-config)
    (let [ident-name (table->ident-name table-name)
          result (reduce (fn [m row] (assoc m (:id row) row)) {} rows)]
      {ident-name result})))

(defn merge-table-data [{:keys [parser-config] :as env} td1 td2]
  ;; (info "Merging td1: " td1)
  ;; (info "Merging td2: " td2)
  (when (:normalize parser-config)
    (cu/deep-merge-concat-vectors td1 td2)))

(defn get-join
  "Returns list of rows for join"
  [env {:keys [t1 t1-rows join-ast table-data user]}]
  (let [{:keys [props joins star-key?]} (categorize-props env join-ast)
        ;; Not much sense in getting rows for a join if there's no t1-rows to
        ;; embed them in. Also cuts short unbounded recursive queries.
        follow-joins? (pos? (count t1-rows))
        joins (if (and (:join-type join-ast) (not follow-joins?)) [] joins)
        ;; We need to fetch these belongs-to keys on top of the desired keys
        ;; so we know what to get when we go fetch the joined row
        belongs-to-foreign-keys (get-belongs-to-foreign-keys joins)
        t2-cols (or (and star-key? :all-cols)
                    (reduce (fn [props foreign-key]
                              (cond-> props
                                (not (cu/includes? props foreign-key)) (conj foreign-key)))
                            props belongs-to-foreign-keys))
        t2 (or (:table join-ast) (:key join-ast)) ;if join key is alias for a table
        ;; _ (info "join-ast--------------")
        ;; _ (pprint join-ast)
        ;; _ (info "end join-ast--------------")
        [t2-from-ident id] (maybe-ident->table t2)
        {:keys [join-type join-table join-table-kw t1-foreign-key t2-foreign-key params]} join-ast
        ;; _ (timbre/info "Getting unjoined-rows" t1 t2 join-type params)
        t2-rows (get-unjoined-rows env {:join-type join-type
                                        :join-table join-table
                                        :join-table-kw join-table-kw
                                        :t1 t1 :t1-rows t1-rows
                                        :t2 t2 :t2-from-ident t2-from-ident
                                        :t1-foreign-key t1-foreign-key
                                        :t2-foreign-key t2-foreign-key
                                        :id id :cols t2-cols
                                        :user user :params params})
        limit-params (:limit params)
        t2-rows (if (and (= join-type :has-many)
                         limit-params)
                  (into [] (mapcat #(limit limit-params %) (vals (group-by t1-foreign-key t2-rows))))
                  t2-rows)

        ]
    ;;Let's go look for joins from t2, so let's call it t1 from now, before we
    ;;return it as t2 again.
    (let [t1 (or t2-from-ident t2)
          t1-cols t2-cols
          ;; t1-foreign-key t2-foreign-key
          t1-props props
          t1-rows-meta (meta t2-rows)]
      (loop [joins-to-process joins
             t1-rows t2-rows
             table-data table-data]
        (if (seq joins-to-process)
          (let [join (first joins-to-process)
                ;; _ (timbre/info "get-join" (:key join) )
                {:keys [new-table-data t2-rows t2-cols t2-props]} (get-join env {:t1 t1
                                                                                 :t1-rows t1-rows
                                                                                 :join-ast join
                                                                                 :table-data table-data
                                                                                 :user user})
                t2 (or (:table join) (:key join))
                t1-rows (insert-join (:join-type join) {:env env
                                                        :t1 t1 :t1-rows t1-rows
                                                        :t1-cols t1-cols
                                                        :t1-props t1-props
                                                        :t2-key (:key join)
                                                        :t2 t2
                                                        :t2-rows t2-rows
                                                        :t2-cols t2-cols :t2-props t2-props
                                                        :t1-foreign-key (:t1-foreign-key join)
                                                        :t2-foreign-key (:t2-foreign-key join)
                                                        ;; :limit-params (get-in join [:params :limit])
                                                        })

                ;; _ (timbre/info "t1, t2" t1 t2)
                ;; _  (timbre/info "t1-rows")
                ;; _ (timbre/info :#pp t1-rows)
                ;; _  (timbre/info "t2-rows")
                ;; _ (timbre/info :#pp t2-rows)
                ;; _ (timbre/info "new-table data for " (:key join))
                ;; _ (timbre/info :#pp new-table-data)

                ;;Remove table data that is not linked to in a join. This
                ;;happens when there's a limit on the join. We can't do this via
                ;;sql, so we apply limit manually. But that means we might have
                ;;surplus table data. We remove that here. This works as long as we
                ;;don't have a query that's using the same has-many join deeper nested again.
                ;; t2-by-id (bu/table->table-by-id t2)
                ;; new-table-data (if (= (:join-type join) :has-many)
                ;;                  (let [join-ids (reduce (fn [join-ids row]
                ;;                                           (into join-ids (:join-ids (meta row))))
                ;;                                         [] t1-rows)]
                ;;                    (update new-table-data t2-by-id #(select-keys % join-ids)))
                ;;                  new-table-data)
                ]

            ;; (timbre/info "purged new-table data for " (:key join))
            ;; _ (timbre/info :#pp new-table-data)

            (recur (rest joins-to-process) t1-rows (merge-table-data env table-data new-table-data)))

          (let [keys-to-dissoc  (cond-> []
                                  (and (= join-type :many-to-many)
                                       (some? t1-foreign-key)) (conj t1-foreign-key)
                                  (and  (= join-type :has-many)
                                        (not= t1-cols :all-cols)
                                        (not (cu/includes? t1-props t2-foreign-key))) (conj t2-foreign-key))
                rows-for-table-data (cond->> t1-rows
                                      (seq keys-to-dissoc)
                                      (mapv #(apply dissoc % keys-to-dissoc)))]
            {:new-table-data (merge-table-data env (normalize-rows env t2 rows-for-table-data) table-data)
             :t2-rows (with-meta t1-rows t1-rows-meta)
             :t2-cols t1-cols
             :t2-props t1-props}))))))
