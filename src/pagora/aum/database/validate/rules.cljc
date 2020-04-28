(ns pagora.aum.database.validate.rules
   #?(:cljs (:require-macros [pagora.aum.database.validate.Rule :refer [rule]]))
  (:require [pagora.clj-utils.core :as cu]
            #?(:clj [pagora.aum.database.validate.Rule :refer [rule]])
            [pagora.aum.database.query :refer [sql]]
            [taoensso.timbre :as timbre :refer [error info]]
            ))

(defn require-keys
  "Checks a for record if any of the disallowed-empty-keys are actually empty"
  [record disallowed-empty-keys info]
  (rule (let [actual-disallowed-empty-keys (cu/empty-keys record disallowed-empty-keys)]
          (empty? actual-disallowed-empty-keys))
        "Certain keys are not allowed to be empty."
        (merge info {:disallowed-empty-keys disallowed-empty-keys})))

(defn validate-not-in-use
 "Throws an exception if there's any rows in table where the value of
  the belongs-to-column is id"
  ([env table belongs-to-column id]
   (validate-not-in-use env table belongs-to-column id nil))
  ([env table belongs-to-column id cond]
   (rule
    (let [in-use-count (-> (sql env :count-belongs-to {:table table
                                                       :belongs-to-column belongs-to-column
                                                       :id id
                                                       :cond cond})
                           vals
                           first)
          in-use? (> in-use-count 0)]
      (not in-use?))
    (str "At least one record from " (name table) " has a belonging join via "
         belongs-to-column " with value " id)
    {:id id :error :deleting-in-use-record :table table})))
