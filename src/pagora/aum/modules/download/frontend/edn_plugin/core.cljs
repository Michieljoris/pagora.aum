(ns pagora.aum.modules.download.frontend.edn-plugin.core
  (:require
   [pagora.aum.modules.download.frontend.data :refer [make-data]]
   [fipp.edn :refer (pprint) :rename {pprint fipp}]))

(defmethod make-data :edn [_ {:keys [rows on-data]}]
  (on-data (with-out-str (fipp rows))))
