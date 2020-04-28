(ns pagora.aum.security
  (:require
   [pagora.aum.database.inspect :as db-inspect]
   [pagora.aum.database.queries :as q]

   [pagora.aum.database.validate.core :as av]
   [pagora.clj-utils.core :as cu]

   [clojure.pprint :refer [pprint]]
   #?@(:clj
       [[crypto.password.bcrypt :as bcrypt]])
   [clojure.set :as set]
   [taoensso.timbre :as timbre :refer [info]]))

(def ^{:dynamic true} *schema-warnings* true)

;; (ns-unmap *ns* 'process-user)

(defmulti process-user
  "foo"
  (fn [env user]

      (timbre/info :#pp {:again :there!!!})
    (:id user)))

(defmethod process-user :default
  [env user]
  (assoc user :role "no-role"))

#?(:clj
   (defn make-uuid
     "UUID generator for user tokens. (password reminder / cookies )"
     [] (str (java.util.UUID/randomUUID))))

#?(:cljs
   (defn make-uuid
     "UUID generator for user tokens. (password reminder / cookies )"
     [] (str "some-uuid" (js/Math.random))))

#?(:clj
   (defn password-encrypt
     "Encrypt the password using bcrypt for storage."
     [password]
     (bcrypt/encrypt password)))

#?(:clj
   (defn make-random-password []
     (bcrypt/encrypt (make-uuid))))

#?(:cljs
   (defn password-encrypt
     "Encrypt the password using bcrypt for storage."
     [password]
     password))

#?(:cljs
   (defn make-random-password []
     "some-random-password"))

#?(:clj
  (defn password-check [original encrypted]
    (bcrypt/check original encrypted)))

#?(:cljs
  (defn password-check [original encrypted]
    (= original encrypted)))

(defn user-underscore->hyphen [user]
  (cu/transform-keys (comp keyword cu/underscore->hyphen name) user))

(defn override-whitelist [all-columns override]
  (let [whitelist-override (:whitelist override)
        blacklist-override (:blacklist override)
        pred [(some? whitelist-override) (some? blacklist-override)]]
    (condp = pred
      ;;if nothing is defined, nothing is accessible
      [false false] []
      ;;if only whitelist is defined, remove non existing columns
      [true false] (vec (set/intersection (set all-columns) (set whitelist-override)))
      ;;if only blacklist is defined remove them from all columns
      [false true] (vec (set/difference (set all-columns) (set blacklist-override)))
      ;;if both are defined calc whitelist first, then remove non existing columns
      (vec (set/intersection (set all-columns) (set/difference (set whitelist-override) (set blacklist-override)))))))

(defn get-override [node {:keys [user-id group-id role] :as user}]
  (let [user-id-node (get-in node [:id user-id])
        role-node (get-in node [:role role])
        group-id-node (get-in node [:group group-id])
        subnode (or user-id-node role-node group-id-node)]
    (if subnode
      (get-override subnode user)
      node)))

(defn get-relevant-props-from-user [user]
  {:user-id (:id user)
   :group-id (:group-id user)
   :role (:role user)})

(defn get-permissions
  "Gets whitelist, blacklist and scope as set in db-config in env.
  Derives blacklist from whitelist if schema is defined. Throws
  exception if schema is nil and neither whitelist or blacklist is
  nil for the table and type (type should be :read, :update
  or :create)"
  [{:keys [db-config schema] :as env} type table user]
  (let [override (get-override (get-in db-config [table type])
                               (get-relevant-props-from-user user))]
    (merge {:scope (:scope override)} ;scope are conditions that should get added to where clauses
           (let [all-columns (db-inspect/list-columns env table)
                 whitelist (override-whitelist all-columns override)]
             {:whitelist whitelist
              :blacklist (vec (set/difference (set all-columns) (set whitelist)))}))))

(defn get-whitelist
  ([env type table] (get-whitelist env type table nil))
  ([env type table user] (:whitelist (get-permissions env type table user))))

(defn get-blacklist
  ([env type table] (get-blacklist env type table nil))
  ([env type table user] (:blacklist (get-permissions env type table user))))

(do
  ;; (require
  ;;  ;; '[digicheck.database.connection :refer [make-db-connection]]
  ;;  '[parser.core :refer [parser-env]])


  (defn secure-schema [schema db-config]
    (let [get-table-name (partial db-inspect/table-name {:db-config db-config})
          table-name->columns (reduce (fn [m [table {:keys [table-name columns schema] :as config}]]
                                        (assoc m (get-table-name table) (or columns (keys schema)) ))
                                      {} db-config)]
      (into {} (mapv (fn [[table-name {:keys [columns] :as config}]]
                       (let [defined-columns (set (get table-name->columns table-name))
                             derived-columns (set columns)
                             columns (into [] (set/intersection defined-columns derived-columns))
                             non-existing-defined-columns (set/difference defined-columns derived-columns)
                             non-defined-colums (set/difference derived-columns defined-columns)]
                         (when (and (seq non-existing-defined-columns) *schema-warnings*)
                           (timbre/warn :#cyan "[db-config] There are columns defined for " table-name " that don't exist in the database: "
                                        (vec non-existing-defined-columns) ".")


                           )
                         (when (and (contains? table-name->columns table-name)
                                    (seq non-defined-colums)
                                    (seq columns)
                                    *schema-warnings*)
                           (timbre/warn :#magenta "[db-config] Not all columns for table" table-name "are defined. Missing are:" (vec non-defined-colums))
                           )
                         [table-name (assoc config :columns columns)]))
                     schema))))

  ;; (secure-schema (:schema parser-env) database.config/db-config)
  )

(do
  ;;TODO: validate cols used in scope against whitelists??
  (defn validate-db-config [db-config schema]
    (doseq [[table {:keys [columns]}] db-config]
      (when (empty? columns)
        (timbre/warn :#r (str "There is config for table " (db-inspect/table-name {:db-config db-config} table)
                              ", however no columns are defined. Om queries for records in this table will not return any results.") ))))
  ;; (validate-db-config database.config/db-config nil)
  )

(defn get-scope
  ([env table] (get-scope env table nil))
  ([env table user]
   (let [override (get-override (get-in env [:db-config table :read])
                                ;; (:read (get-env table))
                                (get-relevant-props-from-user user))]
     (:scope override))))

(defn get-user-by-remember-token
  "Gets remember token from req, finds associated user and returns it
  ,or nil if not found. If optional third cols param is not empty then
  only those cols are returned for user"
  ([env req] (get-user-by-remember-token env req []) )
  ([{:keys [db-conn] :as env} req cols]
   (let [remember_token (get-in req [:cookies "remember_token" :value])
         user (q/select-user-by-remember_token db-conn {:remember_token remember_token
                                                        :cols (mapv (comp cu/hyphen->underscore name) cols)})]
     (when (not (:deleted user))
       (user-underscore->hyphen user)))))

;; (get-user-by-remember-token {:cookies {"remember_token" {:value "f830c933-9ee9-4e40-8796-59428f448e32"}} } [:id])

(defn get-validation-fun
  "Pass in a table keyword, and optional user and method (:update
  or :create or :delete) and the appropriate entry in database config
  is returned. If no method is given the entry at :validate is used.
  Otherwise at at the [:validate method] path. Defaults don't fall
  through, so once you define something for eg a group or role you
  need to define a default. If validation function is not a function
  error is thrown. If there's no :validate key defined for a table it
  a fn is returned that takes 3 args: env record and mods. This fn
  again calls a extendable validate multimethod"
  ([env table] (get-validation-fun env table nil))
  ([env table method]
   (let [validate-config (get-in env [:db-config table :validate])]
     (if (nil? validate-config)
       ;; Use av/validate multimethod
       (fn validate [env record mods] (av/do-validate method table env record mods))
       ;; Legacy. Using multimethod is better. However this way you can specify validation by id
       ;; and per group as well, not just by role.
       (let [relevant-props-from-user (get-relevant-props-from-user (:user env))
             override (when method (get-override (method validate-config)
                                                 relevant-props-from-user))
             override (or override (get-override validate-config
                                                 relevant-props-from-user))
             validation-fun (or (:default override) override)]
         (if-not (fn? validation-fun)
           av/throw-not-allowed
           validation-fun
           ;; (fn foo [env record mods]
           ;;   (validation-fun env record mods))
           ))))))

(defn login
  "Checks email/password against database and if authenticated returns
  user (hyphenated props)"
  [{:keys [db-conn] :as env} {:keys [email password]}]
  (let [user (user-underscore->hyphen (q/select-user-by-email db-conn {:email email}))]
    (when (and user (not (:deleted user))
               (password-check password (:encrypted-password user)))
      user)))

(defn logout
  "Resets remember token for user."
  [{:keys [db-conn] :as env} user]
  (when-let [{:keys [id]} user]
    (info "Logging out user with id " id)
    (if (= 1 (first (q/set-user-remember_token! db-conn {:id id :remember_token (make-uuid)})))
      :success
      :fail)))

