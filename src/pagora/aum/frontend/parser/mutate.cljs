(ns pagora.aum.frontend.parser.mutate
  (:require-macros
   [pagora.clj-utils.macros :refer [if-let* when-let* assert-x]])
  (:require [pagora.aum.util :as au :refer [page-dirty?]]
            [pagora.clj-utils.frontend.storage.core :as s]
            [pagora.aum.om.next :as om]
            [taoensso.timbre :as timbre]))

(defmulti mutate om/dispatch)

(defmulti post-remote (fn [mutate-symbol value]
                        mutate-symbol))

(defn mutate* [{:keys [ast default-remote default-remote]
                :or {default-remote :aum}
                :as env} key params]
  (let [{:keys [post-remote] :as mutation-result} (mutate env key params) ;; a map with any of :action, :value and zero or more remote and post-remote keys
        mutation-result (dissoc mutation-result :post-remote)
        ;; {:keys [result-without-post-remotes post-remotes]} (group-by (fn [[k _]]
        ;;                                                                (if (str/starts-with? (name k) "post-")
        ;;                                                                  :post-remotes :result-without-post-remotes))
        ;;                                                              mutation-result)
        ;; post-remotes (into {} post-remotes)
        ]
    (into {} (mapv (fn [[k v]]
                     [k (if (and v (not (contains?  #{:action :value} k))) ;;if key is a remote key
                          (let [remote k
                                ;; post-remote-k (au/remote->post-remote-key remote) ;;make post-remote keyword
                                remote-ast (if (true? v) ast v) ;;make sure remote is an ast
                                ;; post-remote-hook (get post-remotes post-remote-k)
                                ] ;;find post-remote-hook
                            ;;and add it to the params so it finds its way to
                            ;;the send fn.
                            (cond-> remote-ast
                              (some? post-remote) (assoc-in [:params :_post-remote] post-remote)
                              ;; (fn? post-remote-hook)
                              ;; (assoc-in [:params :post-remote-hooks remote] post-remote-hook)
                              ))
                          v)])
                   mutation-result))))

(defn make-mutate-fn
  ([] (make-mutate-fn {:default-remote :aum}))
  ([extra-env]
   (fn [env key params]
     (mutate* (merge env extra-env) key params))))
;; =======
;; (defn mutate* [{:keys [ast] :as env} key params]
;;   ;; (bpp "MUTATING")
;;   ;; (bpp {:state (deref (:state env)) :key key :params params})
;;   ;; (info :#p "Mutate:" key)
;;   (let [{:keys [result post-remotes]}
;;         (group-by (fn [[k _]]
;;                     (if (str/starts-with? (name k) "post-")
;;                       :post-remotes :result))
;;                   (mutate env key params))
;;         post-remotes (into {} post-remotes)]
;;     (into {} (mapv (fn [[k v]]
;;                      [k (if (and v (not= k :action))
;;                           (let [post-remote-k (keyword (str "post-" (name k)))
;;                                 remote (if (true? v) ast v)]
;;                             (assoc-in remote [:params :post-remote]
;;                                       (or (get post-remotes post-remote-k)
;;                                           ;; dummy-post-remote
;;                                           )))
;;                           v)])
;;                    result))))
;; >>>>>>> release_master

(defmethod mutate :default
  [{:keys [state]} key _]
  {:action (fn [] (timbre/info :#r (str "Unknown mutation:" key))
             {:unknown-mutation key})})

(defmethod mutate 'admin/clear-key
  [{:keys [state]} _ {:keys [key]}]
  {:action (fn []
             (swap! state dissoc key))})

(defmethod mutate 'admin/set-key
  [{:keys [state] :as env} _ {:keys [key value]}]
  {:action (fn []
             (swap! state assoc key value))})


(defmethod mutate 'admin/merge-map
  [{:keys [state]} _ {:keys [map-to-merge]}]
  {:action (fn []
             (swap! state merge map-to-merge))})

(defmethod mutate 'admin/remove-query-key
  [{:keys [state] :as env} _ {:keys [key-or-path]}]
  (let [om-path (-> env :component om/props meta :om-path)
        path (cond-> key-or-path
               (keyword key-or-path) (vector))
        path (into om-path path)]
    (swap! state update-in (butlast path) dissoc (last path))))

(defmethod mutate 'debug/bla
  [{:keys [state ast reconciler] :as env} key params]
  {:value {:foo :bar}
   :remote false
   :action (fn []
             (timbre/info "you called debug/bla mutation")
             (timbre/info "Params: " params)
             )})
