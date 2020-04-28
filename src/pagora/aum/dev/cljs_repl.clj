(ns pagora.aum.dev.cljs-repl
  (:require   [weasel.repl.websocket]
              [cider.piggieback]
              [taoensso.timbre :as timbre]))

(def weasel-repl-env (weasel.repl.websocket/repl-env :ip "0.0.0.0" :port 9001))

(defonce cljs-atom (atom nil))

(defn start-repl []
  (let [result (cider.piggieback/cljs-repl weasel-repl-env)]
    (reset! cljs-atom result)))
