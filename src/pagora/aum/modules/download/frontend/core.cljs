(ns pagora.aum.modules.download.frontend.core
  (:require
   [pagora.aum.om.next :as om]
   ;;TODO-aum: get the chsk-send! fn properly
   [pagora.aum.frontend.websockets.core :as websocket :refer [chsk-send!]]
   [cuerdas.core :as str]
   [pagora.aum.om.util :as om-util]
   [pagora.aum.frontend.parser.read :as aum-read]
   [pagora.aum.util :as au :refer [table->table-by-id]]
   [pagora.aum.om.next.impl.parser :as om-parser]
   [taoensso.timbre :as timbre]
   [fipp.edn :refer (pprint) :rename {pprint fipp}]
   [clojure.pprint :refer [pprint]]

   [pagora.aum.modules.download.frontend.data :refer [make-data make-url]]
   [pagora.aum.modules.download.csv-plugin.core]
   ))

(defn prompt-download [data {:keys [type filename]}]
  (let [element (.createElement js/document "a")
        url (make-url type {:data data})
        filename (str filename "." (case type
                                     :text "txt"
                                     (name type)))]

    (. element setAttribute "href" url)
    (. element setAttribute "download" filename) ;
    (set! (.. element -style -display) "none")
    (js/document.body.appendChild element)
    (. element click)
    (js/document.body.removeChild element)
    (case type
      [:pdf :blob] (js/URL.revokeObjectURL url)
      nil)))

(defn fetch-page-data [this {:keys [on-load on-progress columns]}]
  (on-progress {:status :fetching-data})
  (let [cmp-query (om/get-query this)
        table-query (->> cmp-query
                         (some #(when (= (om-util/join-key %) :item-batch) %))
                         :item-batch
                         first)
        ast (om-parser/expr->ast table-query)
        {:keys [_ params]
         table :key} ast
        app-data (au/component->app-state this)
        {:keys [app/route]} app-data
        table-info (get-in app-data [:client/page-state route :table table])
        ;;DEBUG!!!
        ;; table-info (assoc table-info :search-term "Amsterdam")

        batch-params (aum-read/make-batch-params table-info)
        batch-params (if-let [page-batch-params (get-in app-data [:client/page-state
                                                                  route
                                                                  :batch-params
                                                                  (:batch-params params)])]
                       (if (fn? page-batch-params)
                         (page-batch-params app-data batch-params)
                         page-batch-params)
                       (aum-read/standard-batch-params batch-params))

        ;; query (filterv (fn [e]
        ;;                  (and (keyword? e)
        ;;                       (not= e :is-dirty?)
        ;;                       (not (str/starts-with? (namespace e) "client"))
        ;;                       (contains? (set columns) e)))
        ;;                query)
        query (into [] columns)
        chsk-send! (websocket/get-chsk-send!-fn)
        batch-params (assoc batch-params :with-meta :count)]

    (letfn [(fetch-chunk [{:keys [all-rows call-count chunk-size]}]
              (let [batch-params (assoc batch-params
                                        ;;TODO-NOW: only when filtering on subs!!
                                        :custom-read :user-scoped-by-subscription
                                        :limit {:count chunk-size
                                                :offset (count all-rows)})
                    bulk-query [(list {table query} batch-params)]]
                ;; (timbre/info :#pp {:bulk-query bulk-query})
                (chsk-send! [:admin/query {:query bulk-query
                                           :options {:normalize? false}}] 8000
                            (fn [{:keys [status value] :as response}]
                              (if (= status :ok)
                                (do
                                  (let [{:keys [rows meta]} (get value table)
                                        total (:count meta)
                                        all-rows (into all-rows rows)
                                        chunk-size (or chunk-size (count rows))]
                                    ;; (timbre/info :#pp {:all-rows all-rows
                                    ;;                    :new-rows rows
                                    ;;                    :call-count call-count
                                    ;;                    :total total
                                    ;;                    :chunk-size chunk-size})

                                    (on-progress {:status :fetched-chunk
                                                  :total total
                                                  :fetched (count all-rows)})
                                    (if (and
                                         ;;We do not want to loop here!!
                                         (number? total) (pos? total)
                                         (pos? chunk-size)
                                         (< (* call-count chunk-size) total)
                                         ;;The real test:
                                         (< (count all-rows) total))
                                      (fetch-chunk {:all-rows all-rows
                                                    :chunk-size chunk-size
                                                    :call-count (inc call-count)})
                                      (on-load all-rows))))
                                (on-progress {:status :error
                                              :error-msg "received error from backend"
                                              :response response}))))))]
      (fetch-chunk {:all-rows []
                    :chunk-size nil
                    :call-count 0}))))

(defn download-file [filename {:keys [type rows columns on-progress]}]
  (try
    ;; (on-progress {:status :creating-file})
    (let [download-cb (fn [data]
                        (on-progress {:status :prompting-download})
                        (prompt-download data {:type type :filename filename}))
          rows (->> rows (mapv #(select-keys % columns)))]
      (make-data type {:rows rows :columns columns :on-data download-cb}))
    (catch :default e
      (on-progress {:status :error
                    :error-msg (.toString e)}))))

(defn download [this {:keys [filename type on-progress columns]
                      :or {on-progress (fn [info]
                                         (timbre/info :#pp info))}}]
  (fetch-page-data this
                   {:on-progress on-progress
                    :columns columns
                    :on-load (fn [rows]
                               (download-file filename {:type type :rows rows :columns columns
                                                        :on-progress on-progress}))}))
