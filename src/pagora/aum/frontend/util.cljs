(ns pagora.aum.frontend.util
  (:require
   [taoensso.timbre :as timbre]
   [pagora.aum.environment :refer [is-development?]]
   [pagora.aum.om.next :as om]))

(defn om-data
  "Call this fn on the this in the render fn. It will return a map of props. state
  and computed."
  [this]
  {:props (dissoc (om/props this) :om.next/computed)
   :state (om/get-state this)
   :computed (om/get-computed this)})

(defn make-cmp
  "Returns a fn[parent-cmp props-or-kw & computed-arg] that when called will
  create a React element. Options can be map with :validator, :keyfn
  and :instrument? keys. If props-or-kw is a keyword its value will be retrieved
  from the parent-cmp props. Any reload-key from the parent-cmp will be added to
  the computed props of this cmp. This will only happen in development."
  [cmp-class & [options]]
  (let [factory (om/factory cmp-class options)]
    (if is-development?
      ;;Returned fn:
      (fn [parent-cmp props-or-kw & [computed-arg]]
        (let [[props computed]
              (try
                (let [{:keys [props computed]} (om-data parent-cmp)
                      {:keys [client/reload-key]} props
                      computed-reload-key (:reload-key computed)
                      computed (assoc computed-arg :reload-key
                                      (or reload-key computed-reload-key))
                      props (cond
                              (keyword? props-or-kw) (get props props-or-kw)
                              (map? props-or-kw) props-or-kw
                              :else nil)]
                  [props computed])
                (catch :default e (timbre/error (str "You've used make-cmp to make "
                                                     (aget cmp-class "displayName") ". Make sure to call it with [this prop-or-key & [computed]]"))
                       e))]
          (when (empty? props)
            (timbre/warn (str "You want to render " (aget cmp-class "displayName") ". But the props passed are empty.")))
          (factory (om/computed (or props nil) computed) )))
      (fn [this k & [computed]]
        (let [{:keys [props]} (om-data this)]
          (factory (om/computed (get props k) computed)))))))
