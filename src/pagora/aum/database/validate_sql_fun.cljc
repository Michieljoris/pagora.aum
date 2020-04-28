(ns pagora.aum.database.validate-sql-fun
  (:require [pagora.aum.database.query :as q]
            [pagora.aum.security :as security]
            [taoensso.timbre :as timbre :refer [info]]))

(defmethod q/validate-sql-fun :get-cols-from-table [_ _ _])
(defmethod q/validate-sql-fun :get-joined-rows [_ _ _])
(defmethod q/validate-sql-fun :count-belongs-to [_ _ _])
(defmethod q/validate-sql-fun :get-now [_ _ _])
(defmethod q/validate-sql-fun :count [_ _ _])
(defmethod q/validate-sql-fun :count-by-join [_ _ _])

(defmethod q/validate-sql-fun :insert-record
  [env fun {:keys [table-keyword mods skip-validate?]}]
  (when (not skip-validate?)
    (let [validation-fun (security/get-validation-fun env table-keyword :create)]
      (validation-fun env nil mods))))

(defmethod q/validate-sql-fun :update-record
  [env fun {:keys [table-keyword id mods skip-validate? current-record]}]
  (when (not skip-validate?)
    (when-not current-record
      (throw (ex-info "Can't update a non existing record" {:id id :table table-keyword})))
    (let [validation-fun (security/get-validation-fun env table-keyword :update)]
      (validation-fun env current-record mods))))

(defmethod q/validate-sql-fun :delete-record
  [env fun {:keys [table-keyword id skip-validate? current-record mods]}]
  (when (not skip-validate?)
    (let [validation-fun (security/get-validation-fun env table-keyword :delete)]
      (validation-fun env current-record mods))))

(defmethod q/validate-sql-fun :bulk-update
  [env fun {:keys [table-keyword skip-validate? mods]}]
  (when (not skip-validate?)
    (let [validation-fun (security/get-validation-fun env table-keyword :bulk-update)]
      (validation-fun env nil mods))))

(defmethod q/validate-sql-fun :insert-rows
  [env fun {:keys [table-keyword mods skip-validate?]}]
  (when (not skip-validate?)
    (let [validation-fun (security/get-validation-fun env table-keyword :bulk-create)]
      (validation-fun env nil mods))))


(defmethod q/validate-sql-fun :search-translations [_ _ _])

(defmethod q/validate-sql-fun :insert-event [_ _ _])

(defmethod q/validate-sql-fun :ids [_ _ _])
