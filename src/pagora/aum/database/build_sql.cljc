(ns pagora.aum.database.build-sql
  (:require
   #?(:clj [clojure.test :refer [deftest is are with-test test-ns]])
   [clojure.test :refer [is]]
   ;; [pagora.aum.database.queries :as q]
   [pagora.clj-utils.core :as cu]
   [cuerdas.core :as str]
   [taoensso.timbre :as timbre :refer [info]])
  )

(def ^:dynamic *quoting* :mysql)

(def s "foo.bar")

(defn quote-table-column [tc]
  (str/join "."
            (mapv (fn [p] (str "`" p "`"))
                  (str/split tc "."))))

(defn q [s]
  (if *quoting*
    (cond
      (string? s) (quote-table-column s)
      (sequential? s) (mapv #(q %) s)
      (keyword? s) (q (name s)) ;;for testing
      :else (throw (ex-info "Can't quote something that's not a string or sequence of strings" {:s s})))
    s))

(defn assert-col
  "Throws if cols is not nil and c is not in cols"
  [c cols]
  (if-not (or (nil? cols) (cu/includes? cols c))
    (throw (ex-info
            (str c " is not an allowed column for this table") {:col c :cols cols}))))

(defn cond-snip [{:keys [prefix p-types cond]}]
  (let [[e1 e2 e3] cond
        add-prefix (fn [prefix v]
                     (cond->> v
                       (some? prefix) (into [prefix])))]
      (condp = p-types
        :iv [(str/join " " (add-prefix prefix [e1 e2 "?"])) e3]
        :iv* (into [(str/join " "
                              (add-prefix prefix
                                          [e1 e2
                                           (apply str ["(" (str/join "," (repeat (count e3) "?")) ")"])
                                           ]))]
                   e3)
        :vi [(str/join " " (add-prefix prefix ["?" e2 e3])) e1]
        :ii [(str/join " " (add-prefix prefix [e1 e2 e3]))]
        :vv [(str/join " " (add-prefix prefix ["?" e2 "?"])) e1 e3]
        :in [(str/join " " (add-prefix prefix [e1 e2 "NULL"]))]
        "")))

;; (q/cond-snip {:prefix "prefix" :p-types :iv :cond ["a" ":=" 1]})

(do
  (defn add-snips
    ([v1 v2] (add-snips v1 v2 ""))
    ([[s1 & a1] [s2 & a2] join]
     (into [(str s1 join s2)] (concat a1 a2)))
    )
  ;; (add-snips ["abc" 3] ["def" 3 4] " ")
  )

(do
  (defn clause-snip [{:keys [prefix cond]}]
    (let [sqlvec  (reduce (fn [m e]
                            (add-snips m e " "))
                          cond)
          prefix (if prefix (str prefix " "))]
      (into [(str prefix "(" (first sqlvec)
                  ")")] (rest sqlvec))))

  ;; (clause-snip {:prefix "prefix" :cond '(["t.a = ?" 1] ["AND t.b = ?" 2])})
  )
;; => ["prefix (t.a = ? AND t.b = ?)" 1 2]
;; => ["prefix\n( t.a = ? AND t.b = ?)" 1 2]

(defn where-snip [{:keys [clause]}]
  (into [(str "where " (first clause))] (rest clause)))

(defn limit-snip [{:keys [count offset] :as params}]
  (cond-> [(str "LIMIT " count)]
    offset (add-snips [(str " OFFSET " offset)])))

;; (limit-snip {:count 1 :offset 3})
;; => ["limit ? offset ?" 1 3]

(defn order-by-snip
  "Takes a list of valid columns and returns a order-by sqlvec to be
  passed to a hugsql query as the order-by param"
  [{:keys [table table-name alias-prefix order-by cols]}]
  [(str "order by "
        (str/join ", " (map (fn [[col dir]]
                              (when (nil? col) (throw (ex-info "Can't order by nil column :-)" {:table table
                                                                                              :cols cols
                                                                                              :order-by order-by})))
                              (assert-col col cols)
                              (if-not (cu/includes? [nil :desc :asc :DESC :ASC] dir)
                                (throw (ex-info (str dir " is not a valid order direction") {:table table
                                                                                             :cols cols
                                                                                             :order-by order-by})))
                              (str alias-prefix table-name  "."
                                   (q (cu/hyphen->underscore (name col)))
                                   (when dir (str " " (name dir)))))
                            order-by)))])


(defmulti build-sql
  (fn [fun-kw params]
    fun-kw)
  )
(defmethod build-sql :default
  [_ _])


(defn remove-whitespace [sqlvec]
  (update-in sqlvec [0] #(str/replace % #"[\n ]+" "")))

(defn cleanup-whitespace [sqlvec]
  (update-in sqlvec [0] #(str/replace % #"[\n ]+" " "))
  )

;; (remove-newline ["a    b\n c" 12 3])

;;*************** Getting/updating of user(s) for authorization purposes
(defmethod build-sql :select-user-by-remember_token [_ {:keys [cols remember_token]}]
  [(apply str (concat ["SELECT "]
                      (if (seq cols) (str/join "," (q cols))  ["*"])
                      [" FROM users WHERE remember_token = ?"]) ) remember_token])

;; (build-sql :select-user-by-remember_token {:remember_token "abc" :cols ["id" "name"]})
;; => ["SELECT `id`,`name` FROM users WHERE remember_token = ?" "abc"]
;; (cleanup-whitespace (q/select-user-by-remember_token-sqlvec {:remember_token "abc" :cols ["id" "name"]}))
;; => ["SELECT `id`, `name` FROM users WHERE remember_token=?" "abc"]

;; #?(:clj
;;    (deftest select-user-by-remember_token
;;      (is (=
;;           (remove-whitespace (build-sql :select-user-by-remember_token {:cols ["id"] :remember_token "abc"}))
;;           ;; => ["SELECT id FROM users WHERE remember_token = ?" "abc"]
;;           (remove-whitespace (q/select-user-by-remember_token-sqlvec {:cols ["id"] :remember_token "abc"}))))))
;; => ["SELECT \nid\nFROM users WHERE remember_token=?" "abc"]

(defmethod build-sql :select-user-by-email [_ {:keys [email]}]
  ["SELECT * FROM users WHERE email = ?" email])
;; (build-sql :select-user-by-email {:email "a@bcom"})
;; => ["SELECT * FROM users WHERE email = ?" "a@bcom"]
;; (q/select-user-by-email-sqlvec {:email "a@bcom"})
;; => ["SELECT * FROM users WHERE email=?" "a@bcom"]

(defmethod build-sql :select-user-by-id [_ {:keys [id]}]
  ["SELECT * FROM users WHERE id = ?" id])
;; (build-sql :select-user-by-id {:id "a@bcom"})
;; => ["SELECT * FROM users WHERE id = ?" "a@bcom"]
;; (q/select-user-by-id-sqlvec {:id "a@bcom"})
;; => ["SELECT * FROM users WHERE id=?" "a@bcom"]


(defmethod build-sql :set-user-remember_token! [_ {:keys [id remember_token]}]
  ["UPDATE users SET remember_token = ? WHERE id = ?" remember_token id])
;; (build-sql :set-user-remember_token! {:id "a@bcom"})
;; => ["UPDATE users SET remember_token = ? WHERE users.id = ?" nil "a@bcom"]
;; (q/set-user-remember_token!-sqlvec {:id "a@bcom" :remember_token "abc"})
;; => ["UPDATE users SET remember_token=? WHERE users.id=?" "abc" "a@bcom"]


;; *************** Interpretation of om/next queries
(do
  (defmethod build-sql :get-cols-from-table [_ {:keys [cols table where-clause :order-by-clause :limit-clause]}]
    (let [sqlvec [(apply str `["SELECT " ~@(if (seq cols) (str/join ", " (q cols))  ["*"]) " FROM " ~table])]]
      (cond-> sqlvec
        where-clause (add-snips where-clause " ")
        order-by-clause (add-snips order-by-clause " ")
        limit-clause (add-snips limit-clause " "))))
  ;; (let [params {:cols ["id" "foo"] :table "my_table"
  ;;               :where-clause ["where (t1a.a = ? AND t1a.a = ?)" 1 2]
  ;;               :order-by-clause (order-by-snip {:table-name "foo" :order-by [[:a] [:b :asc]]})
  ;;               :limit-clause (limit-snip {:count 3 :offset 4})}]
  ;;   (cleanup-whitespace (q/get-cols-from-table-sqlvec params))
  ;;   (build-sql :get-cols-from-table params))
  )
;; => ["select id foo from my_table where (t1a.a = ? AND t1a.a = ?) order by foo.a, foo.b asc limit ? offset ?" 1 2 3 4]

(do
  (defmethod build-sql :get-joined-rows [_ {:keys [cols t1-name t1=t2? t1-alias join-table count?
                                                   t1-foreign-key t2-name t2-alias t2-foreign-key
                                                   where-clause order-by-clause limit-clause]}]
    (let [sqlvec [(str (apply str `["select DISTINCT " ~@(if count?
                                                  "count(*)"
                                                  (if (seq cols)
                                                    (str/join ", " (q cols))
                                                    ["*"]))])
                       " FROM " (q t1-name)
                       (when t1=t2? (str " " (q t1-alias)))
                       " INNER JOIN " (q join-table) " on " (q t1-alias) ".id=" (q join-table) "." (q t1-foreign-key) " inner join "
                       (q t2-name) (when t1=t2? (str " " (q t2-alias)))
                       " on "
                       (q t2-alias)".id=" (q join-table) "." (q t2-foreign-key))]]
      (cond-> sqlvec
        where-clause (add-snips where-clause " ")
        order-by-clause (add-snips order-by-clause " ")
        limit-clause (add-snips limit-clause " "))))
  (let [params {:cols ["id" "foo"] :t1-name "t1" :t2-name "t2" :t1-alias "t1-alias" :t2-alias "t2-alias"
                :t1=t2? true
                :count? true
                :join-table "join-table" :t1-foreign-key "t1-foreign-key" :t2-foreign-key "t2-foreign-key"
                :where-clause ["where (t1a.a = ? AND t1a.a = ?)" 1 2]
                :order-by-clause (order-by-snip {:table-name "foo" :order-by [[:a] [:b :asc]]})
                :limit-clause (limit-snip {:count 3 :offset 4})}]
    ;; (is (= (str/lower (cleanup-whitespace (q/get-joined-rows-sqlvec params)))
    ;;        (str/lower (build-sql :get-joined-rows params))))
    ))
;; => ["select DISTINCT count(*) from `t1` `t1-alias` inner join `join-table` on t1-alias.id=join-table.t1-foreign-key inner join `t2` `t2-alias` on t2-alias.id=join-table.t2-foreign-key where (t1a.a = ? AND t1a.a = ?) order by foo.`a`, foo.`b` asc LIMIT 3 OFFSET 4" 1 2]

;; *************** Mutations

(defn placeholders [vals]
  (str/join "," (repeat (count vals) "?")))

(do
  (defmethod build-sql :insert-record [_ {:keys [no-timestamp? table cols vals]}]
    (into [(apply str `["INSERT INTO " ~(q table) " (" ~@(str/join ", " (q cols))
                        ~(when (not no-timestamp?) ", updated_at, created_at")
                        ") VALUES ("
                        ~@(str (placeholders vals) (when (not no-timestamp?) ", NOW(), NOW()"))
                        ")"])] vals))
  ;; (let [params {:no-timestamp? true :cols ["id" "foo"]
  ;;               :table "some-table" :vals [1 2]
  ;;               }]
  ;;   (cleanup-whitespace (q/insert-record-sqlvec params))
  ;;   (build-sql :insert-record params))
  )
;; => ["insert into some-table (id, foo, updated_at, created_at) VALUES (?,?, NOW(), NOW())" 1 2]


;; -- :name insert-record :i!
;; /*~
;; (if (:no-timestamp? params)
;; "insert into :i:table (:i*:cols) VALUES (:v*:vals)"
;; "insert into :i:table (:i*:cols, updated_at, created_at) VALUES (:v*:vals, NOW(), NOW())")
;; ~*/

(do
  (defmethod build-sql :update-record [_ {:keys [table cols vals no-timestamp? where-clause]}]
    (let [sqlvec (into [(str "UPDATE " (q table) " SET "
                             (str (str/join "," (map #(str % " = ?") (q cols)))
                                  (when (not no-timestamp?) ", updated_at = NOW()"))
                             " ")] vals)]
      (add-snips sqlvec where-clause)))
  ;; (let [params {:no-timestamp? false :cols ["id" "foo"]
  ;;               :table "some-table" :vals [1 2]
  ;;               :where-clause ["where (t1a.a = ? AND t1a.a = ?)" 3 4]
  ;;               }]
  ;;   (cleanup-whitespace (q/update-record-sqlvec params))
  ;;   (build-sql :update-record params))
  )
;; => ["update some-table set id = ?,foo = ? , updated_at = NOW() where (t1a.a = ? AND t1a.a = ?)" 1 2 3 4]
;; => ["update some-table set id = ?,foo = ? where (t1a.a = ? AND t1a.a = ?)" 1 2 3 4]

;;TODO exactly the same as update-record.
(do
  (defmethod build-sql :bulk-update [_ {:keys [table cols vals no-timestamp? where-clause]}]
    (let [sqlvec (into [(str "UPDATE " (q table) " SET "
                             (str (str/join "," (map #(str % " = ?") (q cols)))
                                  (when (not no-timestamp?) ", updated_at = NOW()"))
                             " ")] vals)]
      (add-snips sqlvec where-clause)))
  ;; (let [params {:no-timestamp? false :cols ["id" "foo"]
  ;;               :table "some-table" :vals [1 2]
  ;;               :where-clause ["where (t1a.a = ? AND t1a.a = ?)" 3 4]
  ;;               }]
  ;;   (cleanup-whitespace (q/bulk-update-sqlvec params))
  ;;   (build-sql :bulk-update params))
  )
;; => ["update some-table set id = ?,foo = ? , updated_at = NOW() where (t1a.a = ? AND t1a.a = ?)" 1 2 3 4]

(do
  (defmethod build-sql :delete-record [_ {:keys [table where-clause]}]
    (let [sqlvec [(str "DELETE FROM " (q table) " ")]]
      (add-snips sqlvec where-clause)))

  ;; (let [params {:table "some-table"
  ;;               :where-clause ["where (t1a.a = ? AND t1a.a = ?)" 3 4]
  ;;               }]
  ;;   (cleanup-whitespace (q/delete-record-sqlvec params))
  ;;   (build-sql :delete-record params))
  )
;; => ["delete from some-table where (t1a.a = ? AND t1a.a = ?)" 3 4]

(do
  (defmethod build-sql :insert-event [_ {:keys [table cols vals]}]
    (into [(apply str `["INSERT INTO " ~(q table) " (" ~@(str/join ", " (q cols))
                        ", created_at"
                        ") VALUES ("
                        ~@(str (placeholders vals) ",NOW()")
                        ")"])] vals))
  ;; (let [params {:cols ["id" "foo"]
  ;;              :table "some-table" :vals [1 2]}]
  ;;  (cleanup-whitespace (q/insert-event-sqlvec params))
  ;;  (build-sql :insert-event params))
  )
;; => ["insert into some-table (id, foo, created_at) VALUES (?,?, NOW());" 1 2]

;;*************** misc queries
(do
  (defmethod build-sql :count-belongs-to [_ {:keys [table belongs-to-column id cond]}]
    [(str "SELECT COUNT(*) FROM " (q table) " WHERE " (q belongs-to-column) " = ? " cond) id])

  ;; (let [params {:table "some-table" :belongs-to-column "some-column" :id 1
  ;;               :cond "some extra sql"}]
  ;;   (cleanup-whitespace (q/count-belongs-to-sqlvec params))
  ;;   ;; (build-sql :count-belongs-to params)
  ;;   )
  )
;; => ["select count(*) from some-table where some-column = ? some extra sql" 1]

(defmethod build-sql :get-now [_ params]
  ["SELECT NOW() AS now"])

  ;; (let [params {:table "some-table" :belongs-to-column "some-column" :id 1
  ;;               :cond "some extra sql"}]
  ;;   (cleanup-whitespace (q/get-now-sqlvec params))
  ;;   (build-sql :get-now params))

(do
  (defmethod build-sql :count [_ {:keys [table where-clause]}]
    (let [sqlvec [(str "SELECT COUNT(*) FROM " (q table))]]
      (cond-> sqlvec
        where-clause (add-snips where-clause " "))))

  ;; (let [params {:table "some-table"
  ;;               :where-clause ["where (t1a.a = ? AND t1a.a = ?)" 3 4]
  ;;               }]
  ;;   (cleanup-whitespace (q/count-sqlvec params))
  ;;   ;; (build-sql :count params)
  ;;   )
  )
;; ;;*************** db introspection
(defmethod build-sql :show-tables [_ params]
  ["SHOW TABLES"])

(defmethod build-sql :describe-table [_ {:keys [table]}]
  [(str "DESCRIBE " (q table))])

;; (q/describe-table-sqlvec {:table "foo"})
;; (build-sql :describe-table {:table "foo"})

;; *************** Test db
(defmethod build-sql :drop-db [_ {:keys [db-name alasql-persistence]}]
  [(str "DROP " alasql-persistence " DATABASE IF EXISTS " (q db-name))])
;; (q/drop-db-sqlvec {:db-name "foo"})
;; (build-sql :drop-db {:db-name "foo"})

(defmethod build-sql :create-db [_ {:keys [db-name alasql-persistence]}]
  [(str "CREATE " alasql-persistence " DATABASE IF NOT EXISTS " (q db-name))])
;; (q/create-db-sqlvec {:db-name "foo"})
;; (build-sql :create-db {:db-name "foo" :alasql-persistence "LOCALSTORAGE"})

#?(:cljs
   (defmethod build-sql :attach-db [_ {:keys [db-name alasql-persistence]}]
     [(str "ATTACH " alasql-persistence " DATABASE " (q db-name))]))

;; (q/attach-db-sqlvec {:db-name "foo"})
;; (build-sql :attach-db {:db-name "foo" :alasql-persistence "LOCALSTORAGE"})



(defmethod build-sql :create-table [_ {:keys [table-name columns]}]
  [(apply str `["CREATE TABLE " ~table-name ~@(str " (" (str/join "," columns) ")")])]
  )

;; (q/create-table-sqlvec {:table-name "foo" :columns ["one" "two"]})
;; (build-sql :create-table {:table-name "foo" :columns ["one" "two"]})


(do
  (defmethod build-sql :insert-rows [_ {:keys [table-name cols rows]}]
    (into [(apply str `["INSERT INTO " ~(q table-name)
                        " (" ~@(str/join ", " (q cols)) ") VALUES "
                        ~(str/join "," (mapv #(str "(" (placeholders %) ")") rows))
                        ]

                  )]
          (flatten rows)))

  ;; (let [params {:table-name "some-table" :cols ["id" "name"]
  ;;               :rows [[1 "foo"] [2 "bar"]]
  ;;               }]
  ;;   (cleanup-whitespace (q/insert-rows-sqlvec params))
  ;;   (build-sql :insert-rows params))
  )
;; => ["INSERT INTO `some-table` (`id`, `name`) VALUES (?,?),(?,?)" 1 "foo" 2 "bar"]


#?(:clj
   (defn resolve-fun-in-ns [fun ns-str]
     (let [fun-str (str ns-str "/" (name fun))]
       (resolve (symbol fun-str))))

   :cljs
   (defn resolve-fun-in-ns [fun-kw _]
     (fn [{:keys [alasql db-name]} params]
       (let [hugsql-sqlvec (build-sql fun-kw params)
             alasql-db (aget alasql "databases" db-name)
             alasql-exec (fn [alasql-db hugsql-sqlvec]
                           ;; (js/console.log (first hugsql-sqlvec) (clj->js (rest hugsql-sqlvec)))
                           ;; (js/console.log alasql-db)
                           (.exec alasql-db (first hugsql-sqlvec) (clj->js (rest hugsql-sqlvec))))
             result (js->clj (alasql-exec alasql-db hugsql-sqlvec))]
         ;; (js/console.log result)
         result))))


(defn resolve-sql-fun
  ([fn-kw] (resolve-sql-fun fn-kw nil nil)) ;;to resolve in cljs, and in future also in clj (by calling jdbc directly instead of via hugsql)
  ([fun hugsql-ns-str1 hugsql-ns-str2]
   (or (resolve-fun-in-ns fun hugsql-ns-str1) (resolve-fun-in-ns fun hugsql-ns-str2))))
