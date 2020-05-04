(ns pagora.aum.modules.auth.read)

(defmethod bilby/read :user-scoped-by-subscription
  [{:keys [user state parser query parser-config] :as env} _ params]
  (let [base-table :user
        virtual-table :user+subscription
        prefix-keyword (fn [prefix kw]
                         (keyword (str prefix (name kw))))
        prefix-key (fn [k]
                     (cond->> k
                       (and (keyword? k)
                            (not (namespace k))
                            (not= :null k)
                            (not= :NULL k)
                            (not (str/includes? (name k) ".")))
                       (prefix-keyword  (str (name base-table) "."))))
        prefix-vector (fn prefix-vector [v]
                        (case (count v)
                          2 [(first v) (mapv prefix-vector (second v))]
                          3 [(prefix-key (first v)) (second v) (prefix-key (last v))]
                          v))

        query (mapv prefix-key query)
        {:keys [where order-by limit]} params
        where (prefix-vector where)
        order-by (->> order-by (mapv #(vector (prefix-key (first %)))))
        query [(list {virtual-table query}
                     (merge params {:where where
                                    :order-by order-by
                                    :limit limit}))]
        _ (timbre/info :#pp {:scoped-by :subscription
                             :query query
                             :params params})
        result (get (parser env query) virtual-table)

        with-meta? (map? result)
        rows (cond-> result
               with-meta? :rows)
        base-table-by-id (bu/table->table-by-id base-table)
        virtual-table-by-id (bu/table->table-by-id virtual-table)
        data-is-normalized? (:normalize parser-config)
        strip-base-table-prefix #(into {}
                                       (->> %1
                                            ;; (remove (fn [[k v]]
                                            ;;           (or (str/starts-with? (name k) "template-user.")
                                            ;;               (str/starts-with? (name k) "user."))))
                                            (map (fn [[k v]]
                                                   [(keyword (str/strip-prefix
                                                              (name k)
                                                              (str (name base-table) "."))) v]
                                                   ))))
        rows (cond->> rows
               data-is-normalized? (mapv #(vector base-table-by-id (second %)))
               (not data-is-normalized?) (mapv #(strip-base-table-prefix %)))]

    (when data-is-normalized?
      (swap! state update :table-data
             (fn [table-data]
               (assoc (dissoc table-data virtual-table-by-id)
                      base-table-by-id
                      (into {} (->> (get table-data virtual-table-by-id)
                                    (map (fn [[id record]]
                                           [id (strip-base-table-prefix record)]))))))))
    ;; (timbre/info :#pp result)
    {:value (if with-meta?
              {:rows rows
               :meta (:meta result)}
              rows)})

)
