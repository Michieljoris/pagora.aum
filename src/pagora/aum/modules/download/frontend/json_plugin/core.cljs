(ns pagora.aum.modules.download.frontend.json-plugin.core
  (:require
   [pagora.aum.modules.download.frontend.data :refer [make-data]]
   [cuerdas.core :as str]))

(defn clj->json
  "Returns a string of stringified js map"
  [x]
  (.stringify js/JSON (clj->js x)))

(defn rows->json [rows]
  (str "["
       (apply str
              (->> rows
                   (map #(str (clj->json %) "\n"))
                   (str/join ",")))
       "]"))

(defmethod make-data :json [_ {:keys [rows columns on-data]}]
  (let [rows (->> rows (mapv #(select-keys % columns)))]
    (on-data (rows->json rows))))
