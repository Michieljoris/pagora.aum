(ns pagora.aum.frontend.reconciler.debug
  (:require [taoensso.timbre :as timbre]))

(defn info-send [verbose? msg]
  (when verbose?
    (timbre/info :#b msg)))

(defn debug-send [verbose? query remote]
  (when verbose?
    (timbre/info :#b ">> Actually sending query to " remote)
    (timbre/info :#pp query)
    (timbre/info :#b "<< Actually sending query to " remote)))

(defn debug-response [verbose? response remote]
  (when verbose?
    (timbre/info :#magenta ">> Response from " remote)
    (timbre/info :#pp response)
    (timbre/info :#magenta "<< Response from " remote)))

(defn debug-novelty [verbose? novelty]
  (when verbose?
    (timbre/info :#g "novelty to merge:")
    (timbre/info :#pp novelty)))
