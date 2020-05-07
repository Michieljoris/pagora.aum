(ns pagora.aum.modules.semantic.core
  (:require
   [js.semantic-ui-react :as semantic-ui]
   [goog.object :as goog]
   [js.react :as react]))

(defn component
  "Get a component from sematic-ui-react:
    (component \"Button\")
    (component \"Menu\" \"Item\")"
  [k & ks]
  (let [cmp (if (seq ks)
              (apply goog/getValueByKeys semantic-ui k ks)
              (goog/get semantic-ui k))]
    (fn [& args]
      (let [[props & children] args
            [props children] (if (map? props)
                               [props children] [{} args])]
        (apply (.-createElement react) cmp (clj->js props) children)))))

(def container      (component "Container"))
(def grid           (component "Grid"))
(def column         (component "Grid" "Column"))
(def row            (component "Grid" "Row"))
(def button         (component "Button"))
(def checkbox       (component "Checkbox"))
(def confirm        (component "Confirm"))
(def segment        (component "Segment"))
(def dimmer         (component "Dimmer"))
(def loader         (component "Loader"))
(def message        (component "Message"))
(def message-header (component "Message" "Header"))
