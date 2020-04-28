(ns  pagora.aum.dev.debug
  (:require
   [taoensso.timbre :as timbre]
   [cuerdas.core :as str]
   [pagora.aum.security :as security]
   #?@(:clj  [[taoensso.tufte :as tufte :refer (defnp p profiled profile)]]
       :cljs [[taoensso.tufte :as tufte :refer-macros (defnp p profiled profile)]])
   [clojure.pprint :refer [pprint]]
   ))


;; (tufte/add-basic-println-handler! {})
#?(:cljs
   (aset js/window "foo" (atom [])))


(defn now-in-ms []
  #?(:clj  (System/currentTimeMillis)
     :cljs (system-time)))

(defn mark-point [p & args]
  #?(:cljs
     (let [from (first @js/window.foo)
           start (-(.getTime (js/Date.)) from)]
       (swap! js/window.foo conj (concat [p start] args)))))

(defn reset-points []
  #?(:cljs
     (aset js/window "foo" (atom [(.getTime (js/Date.))]))))

(defn reset-accumulator []
  #?(:cljs
     (aset js/window "accumulator" (atom 0))))

(reset-accumulator)

(defn add-to-accumulator [ms]
  #?(:cljs
     (swap! js/window.accumulator + ms)))

(defn print-accumulator []
  #?(:cljs
     (js/setTimeout (fn []
                      (timbre/info :#r "--------------------------")
                      (timbre/info :#pp @js/window.accumulator)
                      (timbre/info :#r "--------------------------")
                      ) 1000)))


(defn print-points []
  #?(:cljs
     (js/setTimeout (fn []
                      (timbre/info :#r "--------------------------")
                      (timbre/info :#pp @js/window.foo)
                      (timbre/info :#r "--------------------------")
                      ) 1000)))

(defn warn-when [s max some-str]
  (let [e (now-in-ms)
        dt (- e s)
        dt #?(:cljs (js/Math.round dt) :clj dt)]
    (when (< 16 dt)
      (timbre/warn (str some-str " took " dt " ms")))
    dt
    ))

(defn print-permissions [env method table user]
  (print "Permissions to" method table "for user" user "are:\n")
  (pprint (env security/get-permissions method table user)))

(defn print-validation-fn
  ([env method table user] (print-validation-fn env method table user nil))
  ([env method table user some-validation-fn]
   (let [validation-fn (security/get-validation-fun (assoc env :user user) table method)]
     (print "Validation fn for" method table "for user" user "is:\n")
     (pprint validation-fn)
     (when some-validation-fn
       (if (= some-validation-fn validation-fn)
         (print "The same as passed in validation-fn!!!")
         (print "OH NO!!!!! Not the same validation fn"))))))
