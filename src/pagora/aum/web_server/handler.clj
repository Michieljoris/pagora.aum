(ns pagora.aum.web-server.handler
  (:require

   [pagora.aum.security :as security]
   ;; [app.integrations :as integrations]

   [integrant.core :as ig]
   [bidi.ring :as bidi]
   [cheshire.core :as json]
   [clj-bugsnag.ring :as bugsnag.ring]
   [cuerdas.core :as str]
   [ring.logger :refer [wrap-with-logger]]
   [ring.logger.protocols :refer [Logger]]
   [ring.middleware.resource :refer [wrap-resource]]
   ;; [new-reliquary.ring :refer [wrap-newrelic-transaction]]
   ;; [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
   [ring.middleware
    [cors :refer [wrap-cors]]
    [defaults :refer [wrap-defaults]]
    [cookies :refer [wrap-cookies]]
    [json :refer [wrap-json-params]]
    [stacktrace :refer [wrap-stacktrace]]]
   [ring.middleware.session.cookie :refer [cookie-store]]
   [ring.util
    [mime-type :refer [ext-mime-type]]
    [response :refer [content-type get-header]]]
   [taoensso.timbre :as timbre :refer [spy debug error info warn]]
))

(def anti-forgery-setting
  {:error-handler (fn [request] {:status  403
                                 :headers {"Content-Type" "application/response"}
                                 :body    (json/generate-string "Invalid anti-forgery token")})
   :read-token    (fn [req]
                    (or (get (-> req :headers) "x-csrf-token")
                        (-> req :params :csrf-token)))})

;; https://github.com/ring-clojure/ring-defaults
(def site-defaults
  "A default configuration for a browser-accessible website, based on current
  best practice."
  {:params    {:urlencoded true
               :multipart  true
               :nested     true
               :keywordize true}
   :cookies   false                                         ;;wrapper added already
   :session   {:flash        true
               :store        (cookie-store {:key "ahY9poQuaghahc7I"})
               :cookie-attrs {:http-only true}}

   :security  {:anti-forgery         anti-forgery-setting
               :xss-protection       {:enable? true, :mode :block}
               :frame-options        :sameorigin
               :content-type-options :nosniff
               ;; :ssl-redirect true
               ;; :hsts true
               }

   ;; Alternative would be to add resource routes to
   ;; site-routes in routes.clj (mj)
   ;; https://github.com/adzerk-oss/boot-cljs/wiki/Serving-files
   ;; :static    {:resources "/"}

   :responses {:not-modified-responses true
               :absolute-redirects     true
               :content-types          false                ;see wrap-content-type-and-encoding
               :default-charset        "utf-8"}})

;; Links starting and finished log messages with a unique ode
(def ^:dynamic *request-id* nil)

(defn add-request-id [handler]
  (fn [request]
    (binding [*request-id* (rand-int 0xffff)]
      ;; (throw (Exception. "test cookie foo"))
      (handler request))))

(defrecord TimbreLogger [config]
  Logger
  (add-extra-middleware [_ handler] handler)
  (log [_ level throwable message]
    (let [msg (format "(%04x) %s" *request-id* message)]
      (when (:http-log config)
        (case level
          :error
          (error "OH NOES! We have an error!"
                 msg
                 (when throwable (.getMessage throwable)))
          :info (info msg)
          :warn (warn msg)
          :debug (debug msg)
          :trace nil                                        ; let's ignore trace messages
          ;; else
          (println (name level) "-" msg))))))

(defn make-timbre-logger [config]
  (TimbreLogger. config))

(defn header
  "Returns an updated Ring response with the specified header added."
  [resp name value]
  (assoc-in resp [:headers name] (str value)))

(defn gzip-encoding [resp]
  (header resp "Content-Encoding" "gzip"))

(defn cache-control [resp {:keys [query-string]}]
  (if (str/starts-with? query-string "v=")                  ;fingerprinted
    (header resp "Cache-Control" "max-age=31536000")
    resp))

(defn content-type-and-encoding-response
  [resp req opts]
  (if (get-header resp "Content-Type")
    resp
    (let [uri (:uri req)
          mime-type (ext-mime-type uri (:mime-types opts))
          gzip? (= mime-type "application/gzip")
          {:keys [gz-mime-types]} (:config opts)]

      (if (and gz-mime-types gzip?)
        (let [uri' (second (re-find #"(.*)\.[^./\\]+$" uri))
              mime-type' (ext-mime-type uri' (:mime-types opts))]
          (if (contains? gz-mime-types mime-type')
            (-> resp
                (content-type mime-type')
                (gzip-encoding)
                (cache-control req))  ;one year
            (content-type resp (or mime-type "application/octet-stream"))))
        (content-type resp (or mime-type "application/octet-stream"))))))

;; (content-type-and-encoding-response {:uri "baz.f/oo.css.gz"} nil nil)


(defn wrap-content-type-and-encoding
  "Middleware that adds a content-type header to the response if one is not
  set by the handler. Uses the ring.util.mime-type/ext-mime-type function to
  guess the content-type from the file extension in the URI. If no
  content-type can be found, it defaults to 'application/octet-stream'.
  Accepts the following options:
  :mime-types - a map of filename extensions to mime-types that will be
                used in addition to the ones defined in
                ring.util.mime-types/default-mime-types

 If content type is application/gzip and secondary type is javascript
  or css encoding header is set to gzip and mime type to js/css
"
  {:arglists '([handler] [handler options])}
  [handler & [opts]]
  (fn [req]
    (when-let [resp (handler req)]
      (content-type-and-encoding-response resp req opts))))

(defn wrap-cors-maybe [handler {:keys [config cors-settings]}]
  (if (:enable-cors config)
    (apply wrap-cors handler cors-settings)
    (fn [req]
      (handler req))))

;; Wrap handler with ring middleware. Read from bottom up. So wrap-stacktrace
;; gets called first. After handler returns its response, wrappers get called in
;; reverse order again (from the top down).
(defn make-handler [{:keys [db-conn db-config clj-env] :as config} routes]
  (-> (bidi/make-handler routes)
       ;; (wrap-newrelic-transaction)
       ;; (wrap-anti-forgery)
       (wrap-defaults site-defaults)
       (wrap-json-params)
       (wrap-resource "/public")
       (wrap-content-type-and-encoding {:config config})
       (wrap-cors-maybe {:config config
                         :cors-settings [:access-control-allow-origin [#".*"] ; disable for prod env
                                         :access-control-allow-methods [:get :put :post :delete]]})
       (wrap-with-logger {:logger (make-timbre-logger config)})
       (add-request-id)
       ;; ^^^ Exceptions in fns above get caught by bugsnag and wrap-stacktrace ^^^
       ;; (bugsnag.ring/wrap-bugsnag
       ;;  {:api-key (:bugsnag-api-key-server config)
       ;;   :environment clj-env
       ;;   ;; Project namespace prefix, used to hide irrelevant stack trace elements
       ;;   ;; :project-ns "your-project-ns-prefix"
       ;;   ;; A optional version for your app, this is displayed in bugsnag.
       ;;   ;; If not provided the latest git sha will be used - this means that
       ;;   ;; the git repo is available when you run your app.
       ;;   ;;TODO: !!!!!!!!
       ;;   ;; :version           (:version integrations/build-info)
       ;;   ;; A optional function to extract a user object from a ring request map
       ;;   ;; Used to count how many users are affected by a crash
       ;;   :user-from-request (fn [request]
       ;;                        (security/get-user-by-remember-token {:db-conn   db-conn
       ;;                                                              :db-config db-config}
       ;;                                                             request
       ;;                                                             [:id :group-id]))
       ;;   })
       (wrap-cookies)                                        ;; Needed for bugsnag (it's not included with wrap-defaults)
       ;;TODO: in production this should show something less scary than a
       ;;stacktrace. though hopefully will never happen.
       (wrap-stacktrace)
       ))

(defn example-handler [req]
  (println "app is called")
  ;; (throw (ex-info "oops" {}))
                                        ;test exception catching
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "Hello HTTP!"})

(defmethod ig/init-key ::handler [k {:keys [routes config]}]
  (when (:integrant-log config) (timbre/info :#g "[INTEGRANT] creating" (name k)))
  (make-handler config routes))
