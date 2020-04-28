(ns pagora.aum.web-server.routes
  (:require
   ;; [web-server.file-transfer :as transfer]

   [pagora.aum.web-server.response :as resp]

   [integrant.core :as ig]
   [clojure.java.io :as io]
   [cuerdas.core :as str]
   [taoensso.timbre :as timbre :refer [info]]
   )
  (:import java.net.NetworkInterface))

(def ip
  (try
    (let [ifc (NetworkInterface/getNetworkInterfaces)
          ips (->> (enumeration-seq ifc)
                   (mapv #(bean %))
                   (filter #(false? (% :loopback)))
                   (mapv (fn [e]
                           (as-> (:interfaceAddresses e) v
                                 (.split (str v) " ")
                                 (first (nnext v))
                                 (if v (str (second (.split v "/"))) "")
                                 )))
                   (filter #(re-matches #"(^192\.168\..*)|(^10\..*)" %)))]
      (first ips))
    (catch Exception e
      ;; (info e)
      nil)))

;; Inserts script tag for vorlon.js using ip of first local lan interface if vorlon-script is true in config
;; Install vorlon: http://vorlonjs.com/
;; Run vorlon on commandline
;; Open vorlorn dashboard at localhost:1337

;; Other ways to load vorlon.js:
;; <!-- http://stackoverflow.com/questions/37020588/embedded-devices-javascript-debugging/37163704#37163704 -->
;; <!-- <script src="http://f48e6e75.ngrok.io/vorlon.js"></script> -->
;; <!-- <script src="http://localhost/vorlon.js"></script> -->

(defn make-html-string [config html-file-name]
  (timbre/info "getting:" html-file-name)
  (when (:vorlon-script config)
    (info (str "Navigate to http://" ip ":" (:server-port config) " on mobile device for vorlon remote debugging")))

  (cond-> (slurp (io/resource html-file-name))
    (:vorlon-script config) (str/replace "<!--vorlon-->"
                                         (str "<script src=\"http://" ip ":1337/vorlon.js\"></script>"))
    true (str/replace "<!--vorlon-->" "")))

(defn html-file-response [config html-file-name]
  (fn [request]
    {:status  200
     :headers {"Content-Type"  "text/html; charset=utf-8"
               "Cache-Control" "no-cache"}
                                        ;:cookies (create-cookie request)
     :body    (make-html-string config html-file-name)
     }))

(defn aum-routes [{:keys [app-html-file-name devcards-html-file-name] :as config}]
  {""         (html-file-response config app-html-file-name)
   "/"        (html-file-response config app-html-file-name)
   ;; "/file-download/" {:get [[true (partial transfer/file-download [app-path "file-download"])]]}
   "/devcards" (html-file-response config devcards-html-file-name)
   ;; "/file-upload" {:post (resp/wrap-authenticate transfer/file-upload)}
   }
  )

  ;; Routes
(defn make-routes [{:keys [app-path extra-routes] :as config
                    :or {app-path ""
                         extra-routes (constantly {})}}
                   {:keys [sente-route]}]
  (let [app-path (str/trim app-path "/")
        routes (merge (aum-routes config)
                      sente-route
                      (extra-routes config)
                      {true resp/not-found})]
    ["" (if (pos? (count app-path))
          {(str "/" app-path) routes
           true resp/not-found}
          routes)]))

(defmethod ig/init-key ::routes [k {:keys [config websocket]}]
  (when (:integrant-log config) (timbre/info :#g "[INTEGRANT] creating" (name k)))
  (let [routes (make-routes config websocket)]
    routes))
