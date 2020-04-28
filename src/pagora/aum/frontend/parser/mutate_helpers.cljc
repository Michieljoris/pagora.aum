(ns pagora.aum.frontend.parser.mutate-helpers
  (:require
   [clojure.data :refer [diff]]
   [pagora.aum.util :as au]
   [pagora.aum.om.next :as om]
   [taoensso.timbre :as timbre]
   ;; #?@(:clj  [[transcriptor :as check :refer [returns!]]])
   ))

(defmulti calc-is-dirty
  "Compares record with its meta. If found different, dependent on its
  type (table) returns true otherwise false"
  (fn [table record] table))


(defn normalize-booleans
  "In the database tinyint(1) (booleans) columns might be null. This fn
  sets them to false in the meta-record for proper comparison if app
  has set the corresponding value in the record to false"
  [meta-record record]
  (let [keys-set-to-false (->> record
                               (filter (fn [[k v]] (false? v)))
                               keys
                               set)]
    (into {} (map (fn [[k v]]
                    (if (and (nil? v)
                             (contains? keys-set-to-false k))
                      [k false]
                      [k v]))
                  meta-record))))

(defmethod calc-is-dirty :default
  [_ {:keys [id] :as record}]
  ;; (info "record" record)
  ;; (info "meta of record" (:record (meta record)))
  (if-let [meta-record (:record (meta record))]
    (not= record (normalize-booleans meta-record record))))

(defn is-dirty? [table record]
  (let [record (vary-meta record update :record dissoc :is-dirty?)]
    (calc-is-dirty table (dissoc record :is-dirty?))))

(defn sort-idents
  "Sort idents by their id, but leave tempid idents alone."
  [idents]
  (sort (fn [[_ id1] [_ id2]]
          (if (or (om/tempid? id1) (om/tempid? id2))
            0                           ;leave them in original order
            (compare id1 id2))) idents))

(defn update-unsaved-records
  "Updates :client/unsaved-records in state by maintaining a map of
  table to a set of ids of unsaved records. Update can be vector of, or a
  single map such as {:table :some-table :id 123 :add-or-remote :add}"
  [state updates]
  (let [updates (if (map? updates) [updates] updates)]
    (swap! state update :client/unsaved-records
           (fn [unsaved-records]
             (reduce (fn [unsaved-records {:keys [table id add-or-remove]}]
                       (update unsaved-records table
                               (fn [s] (condp = add-or-remove
                                         :add (conj (or s #{}) id)
                                         :remove (disj s (get s id))
                                         (timbre/error "Illegal key, you silly developer!!")))))
                     unsaved-records updates))))
  (au/set-onbeforeunload state))

(defn modify-record
  "Pass in an id and a table keyword, and a map of the modifications
  you would like. If record does not exist in the table it gets
  created. State should be an atom. If an existing record has no meta,
  original record is stored in it. Pass in is-dirty-fn to set dirty
  flag properly for the record in its metadata. When updating record
  prev-uuid is set to point at last state where record wasn't
  modified, and next-uuid and next-uuids are set to nil. If replace?
  is true mods are not merged but become the record.
  Returns updated record. Record has extra prop called :is-dirty?
  indicating whether record is same or not compared to what's in
  the :record key of its metadata"
  ([reconciler state mod] (modify-record reconciler state mod nil))
  ([reconciler state {:keys [id table mods meta-mods
                             replace? no-history-entry? ignore-tempid]} is-dirty-fn]
   (let [;;Fetch record to modify from state
         table-by-id (au/table->table-by-id table)
         record (get-in @state [table-by-id id])
         ;; Retrieve value of :record key of meta of record
         meta-record (-> record meta :record)
         ;;Set the :record key of the meta of our record to the metarecord
         record (vary-meta record  assoc
                           :record  (if (some? meta-record)
                                      (merge meta-record meta-mods)
                                      (when (and is-dirty-fn (some? mods)) record)))
         ;; Merge mods with the record
         record (cond-> (merge record mods)
                  ;; If replace? param is set the mods map *is* the record
                  replace? (select-keys (keys mods))
                  ;; Set the various meta props to keep track of mutations to the record.
                  (and (some? mods) (not no-history-entry?))
                  (vary-meta assoc
                             :prev-uuid (last (au/mutation-history reconciler))
                             :uuid nil
                             :next-uuids nil))
         ;; If a is-dirty-fn is passed in, set the is-dirty? prop on record.
         ;; _ (timbre/info "IS_IRTY_FN" is-dirty-fn)
         record (if is-dirty-fn
                  (let [is-dirty? (is-dirty-fn table record)]
                    ;; Update the :client/dirty-record map in app state
                    (update-unsaved-records state {:table table
                                                   :id id
                                                   :add-or-remove (if (or (and (not ignore-tempid) (om/tempid? id))
                                                                          is-dirty?)
                                                                    :add :remove)})
                    ;; Set is-dirty? on record, and remove meta record if not is-dirty?
                    (cond-> (assoc record :is-dirty? is-dirty?)
                      (not is-dirty?) (vary-meta dissoc :record)))
                  record)]

     ;; (info "modified record")
     ;; (pprint record)
     ;; (pprint (meta record))
     ;; Swap the record in app state for the modified record
     (swap! state assoc-in [table-by-id id] record)
     ;; Return the record itself.
     record)))
