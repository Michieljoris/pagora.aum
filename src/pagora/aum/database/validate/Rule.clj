(ns pagora.aum.database.validate.Rule)

(defmacro rule
  "Wraps the validation test in a fn and the message and info part of
  the rule in a fn  Saves some boilerplate in writing validations."
  [& args]
  (let [[rule message info] args]
    {:rule (list 'fn [] rule)
     :fail (list 'fn [] {:message (or message "validation failed") :info (or info {})})}))
