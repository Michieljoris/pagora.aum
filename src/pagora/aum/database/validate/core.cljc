(ns pagora.aum.database.validate.core
  (:require
   [taoensso.timbre :as timbre]))

;; (ns-unmap *ns* 'validate)

(defmulti validate
  (fn [method table {:keys [user parser-config] :as env} record mods modded-record]
    (when true (:validate-log parser-config)
      (timbre/info "Validating for:" [(:role user) method table]))
    [(:role user) method table]))

(defn eval-rules
  "Returns the value of the execution of the value of the :fail key of
  the first of rules for which execution of the value of the :rule key
  returns false. Rest of rules will not be evaluated after that.
  Returns nil if all rules pass."
  [rules]
  (let [rules (if (sequential? rules) rules [rules])]
    (loop [rules rules]
      (when-let [{:keys [rule fail]} (first rules)]
        (let [result (rule)]
          (if result
            (recur (rest rules))
            (fail)))))))


(defn Rules
  "Wraps rules in a vector. Any nested vectors (rules) are spliced in"
  [& args]
  (into [] (->> args
                (remove nil?)
                (mapcat (fn [e] (if (vector? e) e [e]))))))

;; (Rules :a :b nil (Rules :c :d))
;; => [:a :b nil :c :d]
;; (Rules)
;; => []

(defn do-validate [method table env record mods]
  (if-let [{:keys [message info]}
           (eval-rules (validate method table env record mods (merge record mods)))]
    (throw (ex-info message (assoc info :error :validation)))))

(defn throw-not-allowed [_ _ _ mods]
  (throw (ex-info "Action is not allowed" {:mods mods})))

;; (ns-unmap *ns* 'validate)
;; If no validate method is defined for a particular combination of role, method
;; and table by default it will not be allowed
(defmethod validate :default
  [method table env record mods modded-record]
  (throw (ex-info "No validation exists for this action so it is not allowed" {:mods mods
                                                                               :error :validation
                                                                               :method method
                                                                               :table table})))

;; (let [a 1]
;;   (eval-rules
;;    (Rules (rule true "msg1" :info)
;;           (rule false "msg2" :info2)
;;           )))

;; (defn group-id-has-to-be-set-on-create [env record mods modded-record]
;;   (let [mods {:a 2}]
;;     (rule (let [foo {:a 1}]
;;             (timbre/info "hello")
;;             (some? (:group-id mods)))
;;           "When creating a user, group-id has to be set"
;;           {:mods mods})))


;; (macroexpand
;; '(RULE (= 1 1)
;;        "When aaaacreating a user, group-id has to be set"
;;        :info)
;;  )

;; (let [a 1]
;;   (pprint (macroexpand (Rules
;;                         (rule (= a 1)
;;                               "When creating a user, group-id has to be set"
;;                               :info)

;;                         (rule/require-keys {} [:name :email :tent-subject-id] {:table :user})

;;                         (group-id-has-to-be-set-on-create nil nil {:a 1} nil))
;;                        ))
;;   (eval-rules (Rules
;;                (rule (= a 1)
;;                      "When aaaacreating a user, group-id has to be set"
;;                      :info)

;;                (rule/require-keys {} [:name :email :tent-subject-id] {:table :user})

;;                (group-id-has-to-be-set-on-create nil nil {:a 1} nil))))



;; (def r '{admin/save-record {:keys [],
;;                             :tempids {[:admin/by-id
;;                                        "1797d765-0987-491b-b993-f9a5ddd314a9"] [:admin/by-id
;;                                                                                 4]}}})
;; (-> r
;;     (get-in ['admin/save-record :tempids])
;;     vals
;;     first
;;     (get-in [1]))
