(ns pagora.aum.parser.core
  (:require
   #?(:clj [pagora.aum.database.jdbc-defaults :as jdbc-defaults])
   [integrant.core :as ig]
   #?@(:clj
       [[pagora.aum.om.next.server :as om]]
       :cljs
       [[pagora.aum.om.next :as om]])
   [pagora.clj-utils.database.connection :refer [make-db-connection]]
   [pagora.aum.om.util :as om-util]
   [pagora.aum.database.schema :as schema]
   [pagora.aum.parser.mutate :as mutate]
   [pagora.aum.parser.read :as read]
   [pagora.aum.security :as security]
   [clojure.pprint :refer [pprint]]
   [taoensso.timbre :as timbre]
   ))

(def parser-config-defaults
  {;;Log queries received, and returned edn map
   :query-log false
   ;;Log actual sql queries being made
   :sql-log false
   ;;With joins query time can grow exponentially, so impose top limit on number of queried records
   :limit-max 100
   ;;If keys and table names are regular saves configuring db
   :derive-join-type-from-schema true
   :derive-table-and-column-names-from-schema true
   ;;Whether to ignore :root boolean key for a table in db-config
   :allow-root false
   ;;Whether to ignore keys that are not a table
   :om-process-roots false
   ;;Whether to embed joins or store in a map in the state atom
   :normalize true})

(defn get-schema [db-conn]
   (try
     (let [schema (schema/get-schema db-conn)]
       {:raw-schema schema
        :schema (schema/make-condensed-schema schema)})
     (catch
         #?(:clj Exception :cljs :default) e
       ;; (pprint e)
         (timbre/error :#r"Could not get schema. Probably database doesn't exist. Or some other setting is not valid in db-conn:")

       (pprint db-conn))))


(defn parser-env [{:keys [db-conn db-conn-params db-config] :as parser-env}]
  (let [db-conn (if (nil? db-conn)
                  (make-db-connection db-conn-params)
                  db-conn)
        parser-env (update parser-env :parser-config #(merge parser-config-defaults %))
        {:keys [schema raw-schema]} (get-schema db-conn)
        schema (security/secure-schema schema db-config)
        aum-keys [:db-config :parser-config :sql :db-conn :schema :cb :subquery-path :aum-keys]
        _ (security/validate-db-config db-config schema)
        parser-env (assoc parser-env
                          :db-conn db-conn :state (atom nil)
                          :schema schema :raw-schema raw-schema
                          ;;TODO: Really should put all aum info under a aum key,
                          ;;but this would mean refactoring all the fns where env gets
                          ;;destructured into the aum info keys. This aum-keys key
                          ;;allows me to pass on this info into parsers without knowing
                          ;;what keys are exactly aum keys
                          :aum-keys aum-keys)]
    (assoc parser-env :aum (select-keys parser-env aum-keys))))


(defn make-parser-env [{:keys [parser-config db-config db-conn sql]}]
  (parser-env {:parser-config parser-config
               :db-config db-config
               :db-conn db-conn
               :sql sql}))

(defmethod ig/init-key ::parser-env [_ {:keys [config db-conn]}]
  (when (:integrant-log config) (timbre/info :#g "[INTEGRANT] creating parser-env"))
  (make-parser-env {:parser-config config
                    :db-config (:db-config config) ;;description of tables, their names, joins, crud permissions and validations
                    :db-conn #?(:clj db-conn :cljs @db-conn)
                    :sql {:hugsql-ns "database.queries"
                          #?@(:clj [:jdbc-result-set-read-column jdbc-defaults/result-set-read-column])
                          #?@(:clj [:jdbc-sql-value jdbc-defaults/sql-value])}}))

(defn secured-read [{:keys [user] :as env} key params]
  (if user
    (read/read env key params)
    {:value :not-authorized }))

(defn secured-mutate [{:keys [state user] :as env} key params]
  (timbre/info :#r "secured-mutate")
  (if user
    (mutate/mutate-handle-errors env key params)
    {:value :not-authorized }))

 (defn take-nth-mutations [mutation-map n]
   (->> mutation-map
        (reduce (fn [v [k mutations]]
                  (conj v (get mutations n)))
                [])
        (filterv some?)))

 (defn merge-parser-mutation-results [result1 result2]
   (reduce (fn [result [k v]]
             (update result k (fn [existing-result]
                                (conj (if (vector? existing-result)
                                        existing-result
                                        [existing-result])
                                      v))))
           result1 result2))

 (defn group-mutation-results [parser env query]
   (let [mutation-map (reduce (fn [m k]
                                (if (om-util/mutation? k)
                                  (update m (om-util/mutation-key k) (fn [v] (conj (or v []) k)))
                                  m))
                              {} query)
         query (filterv #(not (om-util/mutation? %)) query)
         mutations (take-nth-mutations mutation-map 0)
         query (into query mutations)
         base-result (parser env query)]
     ;; (timbre/info "base-result" base-result)
     ;; (pprint mutation-map)
     (loop [result base-result n 1]
       (let [next-mutations (take-nth-mutations mutation-map n)]
         (if (seq next-mutations)
           (let [next-result (parser env next-mutations)]
             ;; (timbre/info "next-result" next-result)
             (recur (merge-parser-mutation-results result next-result) (inc n)))
           result)))))


(defn parser [{:keys [read mutate parser-env]
               :or {read secured-read
                    mutate secured-mutate}}]
  (let [aum-parser (om/parser {:read read
                                 :mutate mutate})]
    (fn [env q]
      (reset! (:state parser-env) {:status :ok})
      (group-mutation-results aum-parser (merge parser-env env) q))))

(defn make-parser [parser-env]
  (parser parser-env))

(defmethod ig/init-key ::parser [k {:keys [config parser-env]}]
  (when (:integrant-log config) (timbre/info :#g "[INTEGRANT] creating" (name k)))
  (make-parser {:parser-env #?(:clj parser-env :cljs @parser-env)}))
