(ns pagora.aum.database.connection
  (:require
   [taoensso.timbre :as timbre]
   [integrant.core :as ig]

   #?(:clj [clj-time.jdbc])
   [pagora.clj-utils.database.connection :refer [make-db-connection]]
   ))

(defmethod ig/init-key ::db-conn [k {{:keys [mysql-database] :as config} :config}]
  (when (:integrant-log config) (timbre/info :#g "[INTEGRANT] creating" (name k)))
  (make-db-connection mysql-database))
