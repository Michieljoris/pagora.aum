(ns pagora.aum.dev.core
  (:require
   [clojure.string :as str]
   [pagora.aum.dev.integrant.repl :as ig-repl]
   [taoensso.timbre :as timbre]))

(defn print-local-cp []
  (let [cp (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))
        cp (remove #(str/ends-with? % ".jar") cp)]
    (println (clojure.string/join "\n" cp))))

(defn init
  "Pass in aum-config. Sets up integrant for development. After calling this fn it's possible to call
  dev/go, dev/halt and dev/reset etc"
  [{:keys [ig-system-config] :as aum-config}]
  (ig-repl/set-prep! (constantly ig-system-config)))

(def go ig-repl/go)
(def clear ig-repl/clear)
(def halt ig-repl/halt)
(def suspend ig-repl/suspend)
(def resume ig-repl/resume)
(def reset ig-repl/reset)
(def reset-all ig-repl/reset-all)
