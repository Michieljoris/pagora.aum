(ns pagora.aum.util
  #?(:cljs
     (:require-macros
      [pagora.clj-utils.macros :refer [assert-x]]))

  (:require
   #?(:clj
      [pagora.clj-utils.macros :refer [assert-x]])
   [clojure.set :as set :refer [rename-keys]]
   [clojure.pprint :refer [pprint]]
   [pagora.aum.om.util :as om-util]
   [pagora.clj-utils.core :as cu]
   [cuerdas.core :as str]
   [pagora.aum.om.next :as om]
   [taoensso.timbre :as timbre]))

;; om-next ++++++++++++++++++++++++++++++++++++++++++++++++++

(defn component->app-state [component]
  (-> component om/get-reconciler om/app-state deref))

#?(:clj
   (defn create-tempid
     "Takes a string and recreates om/tempid from it"
     [s]
     (om/tempid s)))

#?(:cljs
   (defn create-tempid
     "Takes a string and recreates om/tempid from it"
     [s]
     (om/tempid (uuid. s))))

(defn tempid->string
   "Returns a string of id. Unpacks it if it's a tempid"
   [id]
   (str (if (om/tempid? id)
          (. id -id)
          id)))

;; deprecated
(defn record-id->el-id [id]
  (tempid->string id))

#?(:cljs
   (defn string->tempid [s]
     "Takes a string and creates a tempid if it seems one (not a plain number)
     otherwise returns the number "
     (let [record-id-is-uuid? (some (comp js/isNaN js/parseInt) s)] ;or use regex, but this works
       (if record-id-is-uuid? (create-tempid s) (js/parseInt s)))))


#?(:clj
   (defn string->tempid [s]
     "Takes a string and creates a tempid if it seems one (not a plain number)
     otherwise returns the number "
     (create-tempid s)))

;; deprecated
(defn el-id->record-id [s]
  (string->tempid s))

(defn table->table-by-id
  "Transforms keyword such as :some-table to :some-table/by-id
  keyword"
  [table]
  (keyword (str (cond-> table
                  table name) "/by-id")))

(defn get-ident
  "Gets the first dossier-type ident in seq of keywords"
  [ks table-by-id]
  (let [ident (first (filter om-util/ident? ks))]
    (if (= (first ident) table-by-id) ident)))

(defn rename-ident-key
  "Given two maps, if there's an [table-by-id n1] ident as key in
  first map, replaces this key with the first [table-by-id n2]
  from the second map, returns first map."
  [x y table-by-id]
  (let [xks (keys x)
        yks (keys y)
        x-ident (get-ident xks table-by-id)
        y-ident (get-ident yks table-by-id)]
    (rename-keys x { x-ident y-ident})))

(defn om-deep-merge
  "Default merge is shallow. This one isn't. Mutation keys (symbols)
  are not merged though"
  [a b]
  (letfn [(dm [a b]
            (merge-with (fn [x y]
                          (cond (and (map? x) (map? y))
                                (dm (rename-ident-key x y :dossier-type/by-id) ;TODO: probably obsolete
                                    y)
                                :else y))
                        a b))]
    (let [b (into {} (remove #(symbol? (first %)) b))] ;let's not merge mutations
      (dm a b))))

#?(:cljs
   (def transact-debounced
     (cu/debounce (fn [this query]
                    (om/transact! this query))
                  300)))

(defn select-keys-in-page-state
  [state table-or-path keys]
  (let [path (if (keyword? table-or-path) [:table table-or-path] table-or-path)
        app-state (if (cu/atom? state) @state state)
        {:keys [app/page]} app-state
        page-state-path (concat [:client/page-state page] path)]
     (select-keys (get-in app-state page-state-path) keys)))

(defn get-key-in-page-state
  "Retrieves page state for current route and returns key for table"
  [state table-or-path key]
   (get (select-keys-in-page-state state table-or-path [key]) key))

(defn update-page-state
  [state table-or-path update-fn]
  (let [path (if (keyword? table-or-path) [:table table-or-path] table-or-path)
        {:keys [app/page]} @state
        page-state-path (concat [:client/page-state page] path)]
     (swap! state update-in page-state-path update-fn)))

(defn set-key-in-page-state
  [state table-or-path key value]
   (update-page-state state table-or-path #(assoc % key value)))

;; (let [state (atom {:app/page :some-route})]
;;   (set-key-in-page-state state [:foo :bar] :selected-id "hello")
;;   @state
;;   (get-key-in-page-state state [:foo :bar] :selected-id))

(defn calc-invalidation [validation record col]
  (let [validations (cond-> validation
                      (map? validation) (vector))
        value (get record col)]
    (some (fn [{:keys [invalidated? validated? message]
                :or {validated? (constantly true)
                     message "Validation failed"}
                :as validation}]
            (let [invalidated? (or invalidated? (complement validated?))
                  invalidated? (invalidated? record col value)]
              (when invalidated? (assoc validation :invalidated? invalidated?))))
          validations)))

(defn calc-invalidations [record validate]
  (into {} (->> validate
                (map (fn [[col validation]]
                       (let [validation-result (calc-invalidation validation record col)]
                         ;; (timbre/info "validation-result" validation-result)
                         [col validation-result])))
                (filter (fn [[_ {:keys [invalidated?]}]]
                          invalidated?)))))

;; (def validate-example {:validated? (fn [record col value]
;;                                      (= value "123"))
;;                        :message "Please fill in 123"})
;; (calc-invalidations {:name "123"} {:name validate-example :foo validate-example})
;; => {:foo {:validated? #object[scratchpad$fn__78372 0x1ed489f7 "scratchpad$fn__78372@1ed489f7"], :message "Please fill in 123", :invalidated? true}}


(defn filter-map->where-clauses
  "Every k-v pair in filter-map is a path into filters, so k is name
  of filter, v is true or false. Returns seq of where clauses found at
  the paths"
  [filters filter-map]
  (into []
        (->> filter-map
             (map (fn [filter]
                    (get-in filters filter)))
             (remove nil?))))

(defn make-where-clause
  "Takes search and filters and settings for them and returns a
  compound where clause, if where clause is empty returns nil"
  [{:keys [search-term search-fields filter-map filters or-filters extra-clause]}]
  (let [where-filters (filter-map->where-clauses filters filter-map)
        where-filters (if (seq where-filters) [(if or-filters :or :and) where-filters])
        where-search (if (and (seq search-fields) (not (empty? search-term)))
                       [:or (mapv #(vector % :like (cu/escape-sql-like-term search-term))
                                  search-fields)])
        where (into [] (remove nil? [where-filters where-search extra-clause]))]
    (when (seq where) (conj [:and] where))))

(defn get-selected-id
  "Takes app state and a table keyword and returns the selected id as
  set in page state for the current route (as set in app state) and
  table"
  [state table]
  (let [app-state (if (cu/atom? state) @state state)
        {:keys [app/page]} app-state]
    (get-in app-state [:client/page-state page :table table :selected-id])))

(defn get-selected-item
  "Takes app state and a table keyword and returns the selected item as
  set in page state for the current route (as set in app state) and
  table"
  [state table]
  (get-in @state [(table->table-by-id table) (get-selected-id state table)]))

(defn page-dirty?
  "Retrieves edits-tables from the page state for the current
  route, :client/unsaved-records maps tables to a list of unsaved
  record ids for the table. If any of the edits-tables are not empty
  this fn returns true"
  [app-state current-route app-config]
  (timbre/info :#r "Fix this fn!!!! We have to pass config in!!!!")
  (let [{:keys [edits-tables]} (app-config {:page (keyword (name current-route))})
        unsaved-records-by-table (select-keys (:client/unsaved-records app-state) edits-tables)]
    (some not-empty (vals unsaved-records-by-table))))

(defn get-dirty-routes
  ([state app-config] (get-dirty-routes identity state app-config))
  ([t state app-config]
   (timbre/info :#r "Fix this fn!!!! We have to pass config in!!!!")
   (let [{:keys [client/unsaved-records]} @state
         dirty-routes (reduce (fn [routes [route page-state]]
                                (let [{:keys [edits-tables]} (app-config {:page (keyword (name route))})]
                                  (if (some seq (vals (select-keys unsaved-records edits-tables)))
                                    (conj routes route)
                                    routes)))
                              []
                              (:client/page-state @state))
         dirty-route-names (apply str (->> dirty-routes
                                           (map (comp t name))
                                           (interpose ", ")))]
     {:dirty-routes dirty-routes
      :dirty-route-names dirty-route-names})))

#?(:cljs
   (defn set-onbeforeunload
     ;;TODO-MIGRATE: fix and feed in app-config!!!!
     ([state] (set-onbeforeunload identity state {}))
     ([t state app-config]
      (let [{:keys [dirty-routes dirty-route-names]} (get-dirty-routes state app-config)]
        (if (seq dirty-routes)
          ;; Dialog box shows standard msg in most browsers, not what's returned. Still trying.
          (aset js/window "onbeforeunload" (fn [e] (let [ret (str (t "You have unsaved changes in") (str " " dirty-route-names "\n")
                                                                  (t "Data will be lost if you leave the page, are you sure?"))]
                                                     (aset e "returnValue" ret)
                                                     (timbre/info ret)
                                                     ret)
                                             ))
          (aset js/window "onbeforeunload" nil))))))

#?(:clj
   (defn set-onbeforeunload
     ([state] (set-onbeforeunload identity state))
     ([t state]
      (timbre/info "setting onbeforeunload"))))

;; Adapted from
;; https://gist.github.com/Peeja/eccd5e8169709c12ec0420c86beb49b9
;; Other solution is to update key in app-state on init, see comments in blogpost link below.
#?(:cljs
   (defn rerender-react-tree [reconciler]
     (timbre/info :#y "Rerendering whole react tree")
     (let [root-component (om/app-root reconciler)
           ;; (om/component? x root-component (om/class->any reconciler x))
           ]
       (letfn [(js-vals [o]
                 (map #(aget o %) (js-keys o)))
               ;; Finds the children of a React internal instance of a component.
               ;; That could be a single _renderedComponent or several
               ;; _renderedChildren.
               (children [ic]
                 (if ic
                   (or (some-> (.-_renderedComponent ic) vector)
                       (js-vals (.-_renderedChildren ic)))
                   ))
               (descendant-components [c]
                 ;; Walk the tree finding all of the descendent internal instances...
                 (->> (tree-seq #(seq (children %)) children (.-_reactInternalInstance c))
                      ;; ...map to the public component instances...
                      (map #(.-_instance %))
                      ;; ...and remove the nils, which are from DOM nodes.
                      (remove nil?)))]
         (doseq [c (descendant-components root-component)]
           (try
             (if (.-forceUpdate c) (.forceUpdate c))
             (catch :default e
               (js/console.log e)
               ;; (js/console.log c)
               ))
           )

         ;; (try
         ;;   (doseq [c (descendant-components root-component)]
         ;;     (try
         ;;       (.forceUpdate c)
         ;;       (catch :default e ;; js/Error e  ;; (info e)
         ;;         ))
         ;;     )
         ;;   (catch :default e
         ;;     (pprint e)
         ;;     (error "error, probably because you're using devcards")))
         ))))

(defn remote->post-remote-key [remote-key]
  (keyword (str "post-" (name remote-key))))

;; Debug functions **************************************************
;; Should be removed as dead code in advanced compilation

#?(:cljs
   (defn print-record [uuid record key]
     (let [meta-record (-> record meta :record)]
       (pprint (str (or uuid "[uuid]") " " (get record key)  " " (cu/from-now identity (:updated-at record)) " :is-dirty? " (:is-dirty? record)))
       (pprint (str "Meta: " (get  meta-record key) " " (cu/from-now identity (:updated-at  meta-record)) " :is-dirty? " (:is-dirty?  meta-record))))))

#?(:clj
   (defn print-record [uuid record key]
     (let [meta-record (-> record meta :record)]
       (println (str  "Record:" record  " " ;; (cu/from-now identity (:updated-at record))
                    ))
       (println (str "Meta: " meta-record " " ;; (cu/from-now identity (:updated-at  meta-record))
                    )))))

#?(:cljs
   (defn mutation-history [reconciler]
     (js->clj (.-arr (-> reconciler :config :history)))))

#?(:clj
   (defn mutation-history [reconciler]
     (.list (-> reconciler :config :history))))

#?(:cljs
   (defn print-history [this record table-by-id]
     (let [reconciler (om/get-reconciler this)
           h (mutation-history reconciler)]
       (timbre/info :#b ">>> History uuids")
       (doseq [entry (js->clj h)]
         (let [state (om/from-history reconciler entry)
               h-record (get-in state [table-by-id (:id record)])
               {:keys [prev-uuid uuid next-uuid uuid-trail] :as meta-record} (meta h-record)]
           (pprint entry)
           (print-record uuid h-record :key)
           (pprint (str (or prev-uuid "[prev-uuid]") " <<< " (or uuid "[current-uuid?]") " >>> " (or next-uuid "[next-uuid]")))
           (pprint (reverse uuid-trail))))
       ;; (pprint (last h))
       ;; (pprint (uuid? (last h)))
       )
     (timbre/info :#y "current:")
     (let [{:keys [prev-uuid uuid next-uuid uuid-trail] :as meta-record} (meta record)]
       (print-record uuid record :key)
       (pprint (str (or prev-uuid "[prev-uuid]") " <<< " (or uuid "[current-uuid?]") " >>> " (or next-uuid "[next-uuid]")))
       (pprint (reverse uuid-trail)))

     (timbre/info :#b "<< History uuids")))

#?(:clj
   (defn print-history [reconciler table record-id]
     (let [h (mutation-history reconciler)
           table-by-id (table->table-by-id table)
           record (get-in @(om/app-state reconciler) [table-by-id record-id])
           counter (atom 0)]
       (doseq [entry h]
         (let [state (om/from-history reconciler entry)
               h-record (get-in state [table-by-id (:id record)])
               {:keys [prev-uuid uuid next-uuid uuid-trail] :as meta-record} (meta h-record)]
           (println "----------------------------------------")
           (println "App state at" @counter ":" entry)
           (print-record uuid h-record :key)
           (pprint (str (or prev-uuid "[prev-uuid]") " <<< " (or uuid "[current-uuid?]") " >>> " (or next-uuid "[next-uuid]")))
           (println "uuid-trail:" (reverse uuid-trail)))

         (swap! counter inc)
         )
       ;; (pprint (last h))
       ;; (pprint (uuid? (last h)))

       (let [{:keys [prev-uuid uuid next-uuid uuid-trail] :as meta-record} (meta record)]
           (println "----------------------------------------")
           (timbre/info :#y "current:")
           ;; (println "App state at" @counter ":" entry)
           (print-record uuid record :key)
           (pprint (str (or prev-uuid "[prev-uuid]") " <<< " (or uuid "[current-uuid?]") " >>> " (or next-uuid "[next-uuid]")))
           (println "uuid-trail:" (reverse uuid-trail))

           ))

     (timbre/info :#b "<< History uuids")))



(defn client-key?
  "These are ignored for remotes"
  [k]
  (and (keyword? k)
       (str/starts-with? (namespace k) "client")))

(defn client-only-key? [k]
  (or (= k :is-dirty?)
      (and (keyword? k)
           (str/starts-with? (namespace k) "client"))))

(defn root-client-key?
  "These are ignored for remotes and looked up on the root of the
  app-state map"
  [k]
  (and (keyword? k)
       (= (namespace k) "client")))

(defn list-of-idents? [v]
  (and (vector? v) (om-util/ident? (first v))))

(defn reset-item-batch
  "Resets both latest item batch and item cache in app state to nil at
  respectively client/<table>-list and <path-to-component>.item-batch.<table>.
  If page > 1 item-batch is added to cache and then reset. Page and any other
  keys passed in are added to table state "
  [state table {page-nr :page :as updated-table-state}]
  (let [{:keys [app/page]} @state
        list-table (get-in @state [:client/page-state page :list-table])
        item-batch-path [page :list :item-batch (or list-table table) :rows]
        item-batch (get-in @state item-batch-path)
        ;; item-batch (cond-> item-batch
        ;;              (map? item-batch) (get :rows))
        ]
      (swap! state update-in item-batch-path (constantly nil))
      (update-page-state state table (fn [{:keys [new-records cache] :as table-state}]
                                       (let [cache (if (= page-nr 1) nil (into [] (concat cache item-batch)))
                                             new-records (into [] (filter #(om/tempid? (second %))
                                                                          new-records))]
                                         (merge table-state updated-table-state
                                                {:cache cache
                                                 :new-records new-records}))))))

#?(:cljs
   (defn make-db-connection [{:keys [db-name print-spec alasql-persistence]}]
     (when print-spec
       (timbre/info :#w "Database details:")
       (timbre/info :#w (str "\n" (with-out-str (pprint {:db-name db-name})))))
     ;; (case alasql-persistence
     ;;   "LOCALSTORAGE" (do
     ;;                    (.exec (goog.object/getValueByKeys js/alasql "databases" "alasql")
     ;;                           (str "CREATE " alasql-persistence " DATABASE IF NOT EXISTS " db-name) )
     ;;                    (.exec (goog.object/getValueByKeys js/alasql "databases" "alasql")
     ;;                           (str "ATTACH " alasql-persistence " DATABASE " db-name)))
     ;;   nil)
     {:alasql js/alasql :db-name db-name :alasql-persistence alasql-persistence}))


(defn perform-query
  "Performs query and returns the table data produced for that query"
  ([env query] (:table-data (perform-query env query false)))
  ([{:keys [parser] :as env} query denormalize?]
   (let [state-for-parse (atom nil)
         env (merge (select-keys env (into [:user] (:aum-keys env)))
                    {:state state-for-parse})
         env (assoc-in env [:parser-config :normalize] (not denormalize?))
         result (parser env query)]
     (assert-x (not= (:status @state-for-parse) :error)
               (str "Not able to perform query " query) result)
     {:table-data (:table-data @state-for-parse)
      :result result})))

(defn read-record
  "Performs query on table for record where id is record-id. Merges
  any table-data of result with table-data key in state, returns
  record itself"
  [{:keys [state] :as env} {:keys [id table query denormalize?]}]
  (when (and (some? id) (seq query))
    (let [query [(list {table query} {:where [:id := id]})]
          {:keys [table-data result]} (perform-query env query denormalize?)]
      (if denormalize?
        {:record result}
        {:table-data table-data :record (get-in table-data [(table->table-by-id table) id])}))))

(defn user-id->uid [user-id]
  (keyword (str "uid-" (or user-id "nil"))))

(defn is-development? [{:keys [clj-env]}]
  (= clj-env :dev))
