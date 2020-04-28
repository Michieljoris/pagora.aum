(ns pagora.aum.database.queries
  "This is the namespace that provides the fns that do the actual calling to a
  mysql engine. In backend this is done by hugsql (currently) and in frontend by
  alasql. Currently the fn namespace are defined as vars in this namespace. In
  future though it might be better to use maps, and a fn that retrieves the
  correct fn for some sql call. Either as an executable fn, or as a sqlvec.
  Frontend builds the sql fns itself. Backend on hugsql. Though :TODO that
  could/should be done in backend by build-sql as well. And we can call jdbc
  ourselves. This way back and frontend will be running exactly the same code,
  up to the actual mysql engine (jdbc and alasql respectively)"
  #?(:clj
     (:refer-clojure :exclude [count]))
  #?(:clj (:require [hugsql.core :as hugsql]
                    [hugsql.parameters :as hugsql-parameters]
                    [clojure.string :as string])
     :cljs (:require [pagora.aum.database.build-sql :as build-sql])))

;; Wrapping cols in ANY_VALUE(..) works but result columns are still wrapped in it. They would have to be removed in _process_result in query.clj
;; Leaving this here as an example of how to write these things
;; (defn identifier-param-list [param data options]
;;   [(string/join
;;     ",  "
;;     (map
;;      #(if (vector? %)
;;         (str (hugsql-parameters/identifier-param-quote (first %) options)
;;              " as "
;;              (hugsql-parameters/identifier-param-quote (second %)
;;                                      (merge options {:no-dot-split true})))
;;         (str "ANY_VALUE(" (hugsql-parameters/identifier-param-quote % options) ")"))
;;      (into [] (get-in data (hugsql-parameters/deep-get-vec (:name param))))))])

;; (defmethod hugsql-parameters/apply-hugsql-param :i-any-value* [param data options]
;;   (identifier-param-list param data options))


;; Load all sql fns into this namespace
#?(:clj
   (hugsql/def-db-fns "pagora/aum/database/hug.sql" {:quoting :mysql}))

#?(:clj
   (hugsql/def-sqlvec-fns "pagora/aum/database/hug.sql" {:quoting :mysql}
     ))


#?(:cljs
   (def create-table-sqlvec (partial build-sql/build-sql :create-table)))


#?(:cljs
   (def insert-rows-sqlvec (partial build-sql/build-sql :insert-rows)))

#?(:cljs
   (def insert-rows (build-sql/resolve-sql-fun :insert-rows)))

#?(:cljs
   (def create-table (build-sql/resolve-sql-fun :create-table)))

#?(:cljs
   (def get-cols-from-table
     (build-sql/resolve-sql-fun :get-cols-from-table)))

#?(:cljs
   (def drop-db (build-sql/resolve-sql-fun :drop-db)))

#?(:cljs
   (def create-db (build-sql/resolve-sql-fun :create-db)))

#?(:cljs
   (def attach-db (build-sql/resolve-sql-fun :attach-db)))

#?(:cljs
   (def show-tables (build-sql/resolve-sql-fun :create-db)))

#?(:cljs
   (def select-user-by-remember_token
     (let [sql-fun (build-sql/resolve-sql-fun :select-user-by-remember_token nil nil)]
       (fn [db-conn params]
         ;; (js/console.log "select-user-by-remember-token" params)
         (first (sql-fun db-conn params))))))


#?(:cljs
   (def select-user-by-email
     (let [sql-fun (build-sql/resolve-sql-fun :select-user-by-email nil nil)]
       #(first (sql-fun %1 %2)))))

#?(:cljs
   (def set-user-remember_token!
     (let [sql-fun (build-sql/resolve-sql-fun :set-user-remember_token! nil nil)]
       #(vector (sql-fun %1 %2)))))
