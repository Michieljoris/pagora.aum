(ns pagora.aum.om.Reconciler
  ;; (:refer-clojure :exclude #?(:clj  [deftype replace var? force]
  ;;                             :cljs [var? key replace force]))
  ;; #?(:cljs (:require-macros [om.next :refer [defui invariant]]))
  ;; (:require #?@(:clj  [clojure.main
  ;;                      [cljs.core :refer [deftype specify! this-as js-arguments]]
  ;;                      [clojure.reflect :as reflect]
  ;;                      [cljs.util]]
  ;;               :cljs [[goog.string :as gstring]
  ;;                      [goog.object :as gobj]
  ;;                      [goog.log :as glog]
  ;;                      [om.next.cache :as c]])
  ;;           [om.next.impl.parser :as parser]

  ;;           [om.db-to-tree :refer [db->tree]]
  ;;           ;; [om.tree-to-db :refer [tree->db]]
  ;;           [om.focus :refer [focus-query focus->path]]
  ;;           [om.misc :refer [query-template replace]]

  ;;           [om.tempid :as tempid]
  ;;           [om.transit :as transit]
  ;;           [om.util :as util]
  ;;           [clojure.zip :as zip]
  ;;           [om.next.protocols :as p]
  ;;           [clojure.pprint :refer [pprint]]
  ;;           [cljs.analyzer :as ana]
  ;;           [cljs.analyzer.api :as ana-api]
  ;;           [taoensso.timbre :as timbre]
  ;;           [clojure.string :as str])
  #?(:clj  (:import [java.io Writer])
     :cljs (:import [goog.debug Console])))


;;WIP

;; (defn gather-sends
;;   "Given an environment, a query and a set of remotes return a hash map of remotes
;;    mapped to the query specific to that remote."
;;   [{:keys [parser] :as env} q remotes]
;;   (timbre/info ":#g remotes:" remotes)
;;   (into {}
;;     (comp ;;transducer
;;       (map #(vector % (parser env q %))) ;;gets called first, sets remote key to returned query for earch remote
;;       (filter (fn [[_ v]] (pos? (count v))))) ;;remove remote key if empty query returned for that remote
;;     remotes))

;; (defrecord Reconciler [config state]
;;   #?(:clj  clojure.lang.IDeref
;;      :cljs IDeref)
;;   #?(:clj  (deref [this] @(:state config))
;;      :cljs (-deref [_] @(:state config)))

;;   p/IReconciler
;;   ;;Returns t of reconciler. Incremented each time app state is changed
;;   (basis-t [_] (:t @state))


;;   ;; p/IReconciler
;;   (add-root! [this root-class target options]
;;     (let [ret   (atom nil)
;;           rctor (factory root-class) ;;root constructor
;;           guid  #?(:clj  (java.util.UUID/randomUUID)
;;                    :cljs (random-uuid))]
;;      ;; Populate index
;;       (when (iquery? root-class)
;;         (p/index-root (:indexer config) root-class))
;;       ;; Normalize app-state if needed
;;       (when (and (:normalize config)
;;                  (not (:normalized @state)))
;;         (let [new-state (tree->db root-class @(:state config))
;;               refs      (meta new-state)]
;;           (reset! (:state config) (merge new-state refs))
;;           (swap! state assoc :normalized true)))
;;       ;; Actual react rendering by passing hydrated data (react props) to root
;;       ;; factory, and then calling reactDom.render on it and target
;;       (let [renderf (fn [data]
;;                       (binding [*reconciler* this
;;                                 *shared*     (merge
;;                                                (:shared config)
;;                                                (when (:shared-fn config)
;;                                                  ((:shared-fn config) data)))
;;                                 *instrument* (:instrument config)]
;;                         (let [c (cond
;;                                   #?@(:cljs [(not (nil? target))
;;                                              ((:root-render config) (rctor data) target)])
;;                                   (nil? @ret) (rctor data)
;;                                   :else (when-let [c' @ret]
;;                                           #?(:clj (do
;;                                                     (reset! ret nil)
;;                                                     (rctor data))
;;                                              :cljs (when (mounted? c')
;;                                                      (.forceUpdate c' data)))))]
;;                           (when (and (nil? @ret) (not (nil? c)))
;;                             (swap! state assoc :root c)
;;                             (reset! ret c)))))
;;             ;;Gets query for root class, calls parser on it, this returns
;;             ;;hydrated data tree which gets passed to renderf
;;             parsef  (fn []
;;                       (let [sel (get-query (or @ret root-class))]
;;                         (assert (or (nil? sel) (vector? sel))
;;                           "Application root query must be a vector")
;;                         (if-not (nil? sel)
;;                           (let [env (to-env config)
;;                                 v   ((:parser config) env sel)]
;;                             (when-not (empty? v)
;;                               (renderf v)))
;;                           (renderf @(:state config)))))]
;;         ;;Update reconciler state
;;         (swap! state merge
;;           {:target target :render parsef :root root-class
;;            :remove (fn []
;;                      (remove-watch (:state config) (or target guid))
;;                      (swap! state
;;                        #(-> %
;;                          (dissoc :target) (dissoc :render) (dissoc :root)
;;                          (dissoc :remove)))
;;                      (when-not (nil? target)
;;                        ((:root-unmount config) target)))})
;;        ;;Whenever app-state changes, bump basis-t, and call schedule-render!,
;;        ;;which calls queue-render!, which calls reconcile on reconciler ater
;;        ;;16ms
;;         (add-watch (:state config) (or target guid)
;;           (fn [_ _ _ _]
;;             (swap! state update-in [:t] inc)
;;             #?(:cljs
;;                (if-not (iquery? root-class)
;;                  (queue-render! parsef)
;;                  ;;Also calls queue-render! if :queued flag of reconciler isn't set
;;                  (schedule-render! this)))))
;;         ;;Do initial render
;;         (parsef)
;;         ;;Do any pending remote calls
;;         (when-let [sel (get-query (or (and target @ret) root-class))]
;;           (let [env  (to-env config)
;;                 snds (gather-sends env sel (:remotes config))]
;;             (when-not (empty? snds)
;;               (when-let [send (:send config)]
;;                 (send snds ;;map of remote expressions keyed by remote target
;;                   (fn send-cb
;;                     ([resp]
;;                      ;;Merge new data
;;                      (merge! this resp nil)
;;                      ;;Render with hydrated data
;;                      (renderf ((:parser config) env sel)))
;;                     ([resp query]
;;                      (merge! this resp query)
;;                      (renderf ((:parser config) env sel)))
;;                     ([resp query remote]
;;                      (when-not (nil? remote)
;;                        (p/queue! this (keys resp) remote))
;;                      (merge! this resp query remote)
;;                      (p/reconcile! this remote))))))))
;;         @ret)))

;;   ;; p/IReconciler
;;   (remove-root! [_ target]
;;     (when-let [remove (:remove @state)]
;;       (remove)))

;;   ;; p/IReconciler
;;   ;;Only used by set-query
;;   (reindex! [this]
;;     (let [root (get @state :root)]
;;       (when (iquery? root)
;;         (let [indexer (:indexer config)
;;               c (first (get-in @indexer [:class->components root]))]
;;           (p/index-root indexer (or c root))))))

;;   ;; p/IReconciler
;;   (queue! [this ks] ;;ks can also include component and ref
;;     (p/queue! this ks nil))
;;   ;; p/IReconciler
;;   (queue! [_ ks remote]
;;     (if-not (nil? remote)
;;       (swap! state update-in [:remote-queue remote] into ks)
;;       (swap! state update-in [:queue] into ks)))

;;   ;; p/IReconciler
;;   (queue-sends! [_ sends] ;; sends is map of remote to query
;;     (swap! state update-in [:queued-sends]
;;       (:merge-sends config) sends))

;;   ;; p/IReconciler
;;   ;;If queued flag is not set it is set, and true is returned, otherwise false.
;;   (schedule-render! [_]
;;     (if-not (:queued @state)
;;       (do
;;         (swap! state assoc :queued true)
;;         true)
;;       false))

;;   ;; p/IReconciler
;;   ;;If sends-queued flag is not set it is set, and true is returned, otherwise
;;   ;;false.
;;   (schedule-sends! [_]
;;     (if-not (:sends-queued @state)
;;       (do
;;         (swap! state assoc :sends-queued true)
;;         true)
;;       false))

;;   ;; p/IReconciler
;;   (reconcile! [this]
;;     (p/reconcile! this nil))
;;   ;; TODO: need to reindex roots after reconcilation
;;   (reconcile! [this remote] ;;process the the :queue the reconciler's state
;;     (let [st @state
;;           q (if-not (nil? remote)
;;               (get-in st [:remote-queue remote])
;;               (:queue st))]
;;       (swap! state update-in [:queued] not)
;;       (if (not (nil? remote))
;;         (swap! state assoc-in [:remote-queue remote] [])
;;         (swap! state assoc :queue []))
;;       (if (empty? q)
;;         ;; TODO: need to move root re-render logic outside of batching logic
;;         ((:render st))
;;         (let [cs (transduce
;;                    (map #(p/key->components (:indexer config) %))
;;                    #(into %1 %2) #{} q)
;;               {:keys [ui->props]} config
;;               env (to-env config)
;;               root (:root @state)]
;;           #?(:cljs
;;              (doseq [c ((:optimize config) cs)]
;;                (let [props-change? (> (p/basis-t this) (t c))]
;;                  (when (mounted? c)
;;                    (let [computed   (get-computed (props c))
;;                          next-raw-props (ui->props env c)
;;                          next-props     (om.next/computed next-raw-props computed)]
;;                      (when (and
;;                              (some? (.-componentWillReceiveProps c))
;;                              (iquery? root)
;;                              props-change?)
;;                        (let [next-props (if (nil? next-props)
;;                                           (when-let [props (props c)]
;;                                             props)
;;                                           next-props)]
;;                          ;; `componentWilReceiveProps` is always called before `shouldComponentUpdate`
;;                          (.componentWillReceiveProps c
;;                            #js {:omcljs$value (om-props next-props (p/basis-t this))})))
;;                      (when (should-update? c next-props (get-state c))
;;                        (if-not (nil? next-props)
;;                          (update-component! c next-props)
;;                          (.forceUpdate c)) ;;so if next props is nil, computed doesn't get passed in either!!
;;                        ;; Only applies if we're doing incremental rendering, not
;;                        ;; the case in applications without queries
;;                        (when (and (iquery? root)
;;                                (not= c root)
;;                                props-change?)
;;                          (when-let [update-path (path c)]
;;                            (loop [p (parent c)]
;;                              (when (some? p)
;;                                (let [update-path' (subvec update-path (count (path p)))]
;;                                  (update-props! p (assoc-in (props p) update-path' next-raw-props))
;;                                  (merge-pending-props! p)
;;                                  (recur (parent p)))))))))))))))))

;;   ;;IReconciler
;;   (send! [this]
;;     ;;Just forwards queued-sends as returned from parser for all remotes to send
;;     ;;fn supplied to reconciler. Then calls merge! on value passed to cb
;;     (let [sends (:queued-sends @state)]
;;       (when-not (empty? sends)
;;         (swap! state
;;           (fn [state]
;;             (-> state
;;               (assoc :queued-sends {})
;;               (assoc :sends-queued false))))
;;         ((:send config) sends
;;           (fn send-cb
;;             ([resp]
;;              (merge! this resp nil))
;;             ([resp query]
;;              (merge! this resp query))
;;             ([resp query remote]
;;              (when-not (nil? remote)
;;                (p/queue! this (keys resp) remote))
;;              (merge! this resp query remote)
;;              (p/reconcile! this remote))))))))
