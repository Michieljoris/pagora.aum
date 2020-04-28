(ns pagora.aum.frontend.config
  (:require
   [pagora.aum.environment :refer [environment]]
   [taoensso.timbre :as timbre]))


(def default-config
  {:env environment
   :clj-env environment
   :app :app})

(defmulti config
  (fn [environment]
    environment))

(defmethod config :default [_] nil)

(defn make-app-config []
  (let [app-config (merge (config :common) (config environment))
        app-config (merge default-config app-config {:env environment :clj-env environment})]
    app-config))
