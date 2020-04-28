(ns pagora.aum.om.tree-to-db
  (:require
   [pagora.aum.om.util :as util])
  )

;;WIP


(defprotocol Ident
  (ident [this props] "Return the ident for this component"))

;; (defn- normalize* [query data refs union-seen]
;;   (cond
;;     (= '[*] query) data

;;     ;; union case
;;     (map? query)
;;     (let [class (-> query meta :component)
;;           ident   #?(:clj  (when-let [ident (-> class meta :ident)]
;;                              (ident class data))
;;                      :cljs (when (implements? Ident class)
;;                              (ident class data)))]
;;       (if-not (nil? ident)
;;         (vary-meta (normalize* (get query (first ident)) data refs union-seen)
;;           assoc :om/tag (first ident))
;;         (throw #?(:clj  (IllegalArgumentException. "Union components must implement Ident")
;;                   :cljs (js/Error. "Union components must implement Ident")))))

;;     (vector? data) data ;; already normalized

;;     :else
;;     (loop [q (seq query) ret data]
;;       (if-not (nil? q)
;;         (let [expr (first q)]
;;           (if (util/join? expr)
;;             (let [[k sel] (util/join-entry expr)
;;                   recursive? (util/recursion? sel)
;;                   union-entry (if (util/union? expr) sel union-seen)
;;                   sel     (if recursive?
;;                             (if-not (nil? union-seen)
;;                               union-seen
;;                               query)
;;                             sel)
;;                   class   (-> sel meta :component)
;;                   v       (get data k)]
;;               (cond
;;                 ;; graph loop: db->tree leaves ident in place
;;                 (and recursive? (util/ident? v)) (recur (next q) ret)
;;                 ;; normalize one
;;                 (map? v)
;;                 (let [x (normalize* sel v refs union-entry)]
;;                   (if-not (or (nil? class) (not #?(:clj  (-> class meta :ident)
;;                                                    :cljs (implements? Ident class))))
;;                     (let [i #?(:clj  ((-> class meta :ident) class v)
;;                                :cljs (ident class v))]
;;                       (swap! refs update-in [(first i) (second i)] merge x)
;;                       (recur (next q) (assoc ret k i)))
;;                     (recur (next q) (assoc ret k x))))

;;                 ;; normalize many
;;                 (vector? v)
;;                 (let [xs (into [] (map #(normalize* sel % refs union-entry)) v)]
;;                   (if-not (or (nil? class) (not #?(:clj  (-> class meta :ident)
;;                                                    :cljs (implements? Ident class))))
;;                     (let [is (into [] (map #?(:clj  #((-> class meta :ident) class %)
;;                                               :cljs #(ident class %))) xs)]
;;                       (if (vector? sel)
;;                         (when-not (empty? is)
;;                           (swap! refs
;;                             (fn [refs]
;;                               (reduce (fn [m [i x]]
;;                                         (update-in m i merge x))
;;                                 refs (zipmap is xs)))))
;;                         ;; union case
;;                         (swap! refs
;;                           (fn [refs']
;;                             (reduce
;;                               (fn [ret [i x]]
;;                                 (update-in ret i merge x))
;;                               refs' (map vector is xs)))))
;;                       (recur (next q) (assoc ret k is)))
;;                     (recur (next q) (assoc ret k xs))))

;;                 ;; missing key
;;                 (nil? v)
;;                 (recur (next q) ret)

;;                 ;; can't handle
;;                 :else (recur (next q) (assoc ret k v))))
;;             (let [k (if (seq? expr) (first expr) expr)
;;                   v (get data k)]
;;               (if (nil? v)
;;                 (recur (next q) ret)
;;                 (recur (next q) (assoc ret k v))))))
;;         ret))))

;; (defn tree->db
;;   "Given a Om component class or instance and a tree of data, use the component's
;;    query to transform the tree into the default database format. All nodes that
;;    can be mapped via Ident implementations wil be replaced with ident links. The
;;    original node data will be moved into tables indexed by ident. If merge-idents
;;    option is true, will return these tables in the result instead of as metadata."
;;   ([x data]
;;     (tree->db x data false))
;;   ([x data #?(:clj merge-idents :cljs ^boolean merge-idents) ]
;;    (let [refs (atom {})
;;          x    (if (vector? x) x (get-query x))
;;          ret  (normalize* x data refs nil)]
;;      (if merge-idents
;;        (let [refs' @refs]
;;          (assoc (merge ret refs')
;;            ::tables (into #{} (keys refs'))))
;;        (with-meta ret @refs)))))

(defn- has-error?
  #?(:cljs {:tag boolean})
  [x]
  (and (map? x) (contains? x ::error)))

(defn default-extract-errors [reconciler res query]
  (letfn [(extract* [query res errs]
            (let [class      (-> query meta :component)
                  top-error? (when (and (not (nil? class)) (has-error? res))
                               (swap! errs
                                 #(update-in % [#?(:clj  ((-> class meta :ident) class res)
                                                   :cljs (ident class res))]
                                   (fnil conj #{}) (::error res))))
                  ret        (when (nil? top-error?) {})]
              (cond
                ;; query root
                (vector? query)
                (if (vector? res)
                  (into [] (map #(extract* query % errs)) res)
                  (loop [exprs (seq query) ret ret]
                    (if-not (nil? exprs)
                      (let [expr (first exprs)
                            k    (as-> (util/expr->key expr) k
                                   (cond-> k
                                     (util/unique-ident? k) first))
                            data (get res k)]
                        (cond
                          (util/mutation? expr)
                          (let [mk   (util/mutation-key expr)
                                ret' (get res mk)]
                            (if (has-error? ret')
                              (let [x (-> expr meta :mutator)]
                                (swap! errs
                                  #(update-in % [x]
                                    (fnil conj #{}) (::error ret')))
                                (recur (next exprs) ret))
                              (recur (next exprs)
                                (when-not (nil? ret)
                                  (assoc ret mk ret')))))

                          (util/union? expr)
                          (let [jk     (util/join-key expr)
                                jv     (util/join-value expr)
                                class' (-> jv meta :component)]
                            (if (not (vector? data))
                              (let [ret' (extract*
                                           (get jv (first #?(:clj  ((-> class' meta :ident) class' data)
                                                             :cljs (ident class' data))))
                                           data errs)]
                                (recur (next exprs)
                                  (when-not (nil? ret)
                                    (assoc ret jk ret'))))
                              (let [ret' (into []
                                           (map #(extract*
                                                   (get jv
                                                     (first #?(:clj  ((-> class' meta :ident) class' %)
                                                               :cljs (ident class' %))))
                                                   % errs))
                                           data)]
                                (recur (next exprs)
                                  (when-not (nil? ret)
                                    (assoc ret jk ret'))))))

                          (util/join? expr)
                          (let [jk   (util/join-key expr)
                                jv   (util/join-value expr)
                                ret' (extract* jv data errs)]
                            (recur (next exprs)
                              (when-not (nil? ret)
                                (assoc ret jk ret'))))

                          (and (map? data) (has-error? data))
                          (do
                            (swap! errs
                              #(update-in %
                                [(or (when-not (nil? class)
                                       #?(:clj  ((-> class meta :ident) class res)
                                          :cljs (ident class res)))
                                     k)]
                                (fnil conj #{}) (::error data)))
                            (recur (next exprs) nil))

                          :else
                          (recur (next exprs)
                            (when-not (nil? ret)
                              (assoc ret k data)))))
                      ret))))))]
    (let [errs (atom {})
          ret  (extract* query res errs)]
      {:tree ret :errors @errs})))
