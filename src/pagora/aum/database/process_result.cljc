(ns pagora.aum.database.process-result
  (:require [pagora.clj-utils.core :as cu]
            #?(:cljs [goog.object])
            [clojure.set :as set]
            [pagora.aum.database.query :as q]
            [taoensso.timbre :as timbre :refer [info]]))

;; (ns-unmap *ns* 'q/aum-process-result)

(defn last-insert-id [{:keys [db-conn]} {:keys [table]} result]
   #?(:clj
      (:generated_key result)          ;NOTE: generated_key  only works for mysql
      :cljs
      (let [{:keys [alasql db-name]} @db-conn
            next-id (goog.object/getValueByKeys alasql "databases" db-name "tables" table
                                            "identities" "id" "value")]
        (if next-id (dec next-id)))))

(defmethod q/aum-process-result :get-cols-from-table
  [env _ params result]
  (cu/transform-keys (comp keyword cu/underscore->hyphen name) result))

(defmethod q/aum-process-result :get-joined-rows
  [env _ {:keys [cols join-table join-table-cols t1 t2] :as params} result]
  (if (-> params :count?)
    (get (first result) (keyword "count(*)"))
    (let [records (cu/transform-keys (comp keyword cu/underscore->hyphen name) result)
          join-table-prefix (str (name t1) "-" (name t2) "/")
          rename-map (reduce (fn [acc c]
                               (assoc acc c (keyword (str join-table-prefix (name c)))))
                             {} join-table-cols)
          ;; rename-map (reduce (fn [acc c]
          ;;                      (let [[n k] (str/split c ".")]
          ;;                        (if (= n join-table)
          ;;                          (assoc acc (keyword k) (keyword (str join-table-prefix k)))
          ;;                          acc)))
          ;;                    {} cols)
          ]
      (mapv (fn [r]
              (set/rename-keys r rename-map))
            records))))

(defmethod q/aum-process-result :get-now
  [env _ params result]
  (:now result))

(defmethod q/aum-process-result :insert-record
  [env _ params result]
  (last-insert-id env params result))

(defmethod q/aum-process-result :insert-event
  [env _ params result]
    (last-insert-id env params result))

(defmethod q/aum-process-result :delete-record
  [env _ params result]
  (when (= 1 result) result)) ;;return nil if nothing is deleted

(defmethod q/aum-process-result :bulk-update
  [env _ params result]
  (first result))

(defmethod q/aum-process-result :search-translations
  [env _ params result]
  (cu/transform-keys (comp cu/underscore->hyphen name) result))

(defmethod q/aum-process-result :count
  [env _ params result]
  ;; (first (vals result))
  (:count result))

(defmethod q/aum-process-result :ids
  [env _ params {:keys [last_id row_count] :as result}]
  (mapv #(+ last_id %) (range row_count)))


(defmethod q/aum-process-result :count-by-join
  [env _ params result]
  (let [fk (keyword (:t1-foreign-key params))]
    (reduce (fn [acc el]
              (assoc acc (get el fk) (:count el)))
            {} result)))
