(ns pagora.aum.frontend.reconciler.migrate
  (:require [pagora.aum.util :as au]
            [pagora.clj-utils.core :refer [transform-values]]
            [taoensso.timbre :as timbre]
            [pagora.aum.om.tempid :refer [tempid?]]
            [pagora.aum.om.util :as om-util]))

(defn string-tempids-mapping->tempids-mapping [tempids]
  (into {}
        (map (fn [[ k v]]
               [[(first k) (au/create-tempid (second k))] v])
             tempids)))

(defn transform-server-response-uuid->tempid
  "Can't send om tempids over the wire so we've sent strings for om
  tempids. So we get strings back again in the tempids mapping put
  together by the server. We need to make them tempids again.This
  grabs all the mutations and replaces the uuid string with a om
  tempid if the mutation comes with a tempids mapping. Specter would
  easier here, but this works also"
  [server-response]
  (into {} (map (fn [[k v]]
                  (if (symbol? k) ;only for mutations
                    {k (update v :tempids string-tempids-mapping->tempids-mapping)}
                    ;; (let [tempids (:tempids v)
                    ;;       tempids (string-tempids-mapping->tempids-mapping tempids)
                    ;;       ]
                    ;;   {k (merge v {:tempids tempids})})
                    [k v]))
                server-response)))


(defn tempid-idents-map->tempids-map [tempid-idents-map]
  (into {} (map (fn [[[_ k] [_ v]]] [k v]) tempid-idents-map)))

(defn migrate-idents
  "Takes a map and substitutes any value that is a tempid ident with
  the db ident using the tempids mapping. If value is a vector but not
  an ident, any elements of the vector that are a tempid ident get
  substituted. In all other cases if value is an simple tempid it gets
  substituted directly. This is mainly for making sure tempids are
  replaced in keys such as :client/selected-item"
  [m tempid-idents-map]
  (let [tempids-map (tempid-idents-map->tempids-map tempid-idents-map)]
    ;; (timbre/info :#pp tempids-map)
    (letfn [(tempid-ident? [e] (and (om-util/ident? e) (tempid? (second e))))
            (subsitute-vector [v] (mapv (fn [e]
                                          (if (tempid-ident? e)
                                            (or (get tempid-idents-map e) e) ;replace with its possible mapping
                                            e))
                                        v))
            (substitute-tempid  [value]
              (if (vector? value)
                (if (tempid-ident? value)
                  (or (get tempid-idents-map value) value) ;replace with its possible mapping
                  (subsitute-vector value))
                (or (get tempids-map value) value)))]
      (transform-values substitute-tempid m))))

;; Partially copied from om-next source. Replacing tempids in table data is done
;; the same. The default implementation then does a db->tree, replacing idents
;; as it builds the tree, then a tree->db to get the normalized state back
;; again. However it then limits the new state to whatever the query is at the
;; time. But I want to keep existing state, even if not queried. So I walk the
;; state map myself and replace tempid idents as I go.
(defn migrate
  "Given app-state-pure (the application state as an immutable value),
   tempids (a hash map from tempid to stable id), and an optional id-key
   keyword, return a new application state value with the tempids replaced by
   the stable ids. This also removes :is-dirty? key, since it's saved now"
  ([app-state-pure _ tempids]
    (migrate app-state-pure nil tempids nil))
  ([app-state-pure _ tempids id-key]
   (letfn [(dissoc-in [pure [table id]] ;dissoc the map for id from table
             (assoc pure table (dissoc (get pure table) id)))
           (step [pure [old [_ id :as new]]]
             (let [result (-> pure
                              (dissoc-in old)
                              (assoc-in new
                                        (cond-> (merge (get-in pure old) (get-in pure new))
                                          (not (nil? id-key)) (assoc id-key id)
                                          :always (dissoc :is-dirty?))))]
               result))]
     (if-not (empty? tempids)
       ;; First substitute tempids in table data and entities' id prop
       (let [pure' (reduce step app-state-pure tempids)]
         ;; Replace idents in tree
         (migrate-idents pure' tempids))
       app-state-pure))))
