(ns pagora.aum.modules.download.frontend.csv-plugin.core
  (:require
   [pagora.aum.modules.download.data :refer [make-data]]
   [common.csv :as csv]))

(defmethod make-data :csv[_ {:keys [rows columns on-data]}]
  (on-data (csv/maps->csv-str rows {:columns columns
                                    :options {:include-column-names true
                                              ;; :separator "my-newline"
                                              ;; :quote? (constantly true)
                                              }})))
