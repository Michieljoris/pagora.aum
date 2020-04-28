(ns pagora.aum.frontend.parser.calc-mods
  (:require
   [pagora.aum.util :as au]
   [pagora.aum.om.next :as om]
   [clojure.data :refer [diff]]
   [taoensso.timbre :as timbre]
   ))

;; (ns-unmap *ns* 'calc-mods)

(defn default-calc-mods
  "Default method of calc-mods. It removes all client only keys, and
  then compares the record with the the value for the :record key in
  the meta map of record. A map with differences is returned. If a
  record is 'new', that is if it has a tempid for id the whole record
  is returned (minus :id key). If you pass in record2 it is used
  instead for comparison"
  [{:keys [record record2] :as params}]
  (let [meta-record (:record (meta record))
        record (into {} (remove
                         (fn [[k _]]
                           (or (= k :om.next/computed)
                               (au/client-only-key? k)))
                         record))
        record2 (if (contains? params :record2)
                  record2
                  (if (om/tempid? (:id record))
                    {:id (:id record)}
                    (or meta-record record)))
        [mods _ _] (diff record record2)]
    mods))


;; (ns-unmap *ns* 'calc-mods)

(defmulti calc-mods (fn [table record] table))

(defmethod calc-mods :default [table record]
  (default-calc-mods {:record record}))
