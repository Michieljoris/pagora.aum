(ns ^:figweel-load ^:figwheel-hooks pagora.aum.dev.core
  (:require
   [pagora.aum.om.next :as om]
   [pagora.aum.frontend.core :as aum]
   [taoensso.timbre :as timbre]))

(defn ^:after-load figwheel-reload-callback []
  "Gets called on source changes"
  (timbre/info :#g "=============== RELOAD ===============")
  (let [{:keys [reconciler app-state app-config RootComponent]} @aum/aum-state]
    (when (om/mounted? (om/class->any reconciler RootComponent))
      ;; We should query for client/reload-key in root cmp and
      ;; pass that along in computed, or add it to queries for the components we
      ;; want to rerender on reload. Again, only cmps that pass along computed with
      ;; this reload key will update.
      (let [uuid (random-uuid)]
        (swap! app-state assoc :client/reload-key uuid))
      ;; (put! (get-or-make-channel) {:event :on-jsload :data {}}
      ;;       #(timbre/info "Sent msg :on-jsload"))
      )))
