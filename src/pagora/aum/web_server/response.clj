(ns pagora.aum.web-server.response
  (:require
   [pagora.aum.security :as security]
   [cheshire.core :as json]
   [cheshire.generate :as gen]
   [ring.util.response :refer [content-type response status]]
   [taoensso.timbre :as timbre :refer [debug]]
   [clojure.string :as str])
  (:import (com.fasterxml.jackson.core JsonGenerator JsonFactory JsonGenerator$Feature)
           (java.io StringWriter Writer))
  )

(gen/add-encoder org.joda.time.DateTime
                 (fn [c jsonGenerator]
                   (.writeString jsonGenerator (str c))))

(defn json-response
  [request data & [status]]
  {:status  200
   :headers {"Content-Type" "application/json; charset=utf-8"}
   ;:cookies (create-cookie request)
   :body    (json/generate-string data)})

(defn csv-file-response [request file & [status]]
  ; Strip the randomized numbers from the file-name (because of the tempfile)
  (let [name (.getName file)
        name (str/replace name #"\d" "")]
    {:status  200
     :headers {"Content-Disposition" (str "inline ; filename=\"" name "\"")}
     :body    file}))

;; =============================================================================
;; Not found

(defn json-404 [req]
  (json-response req {"error" "Deze url bestaat niet."} 404))

(defn not-found [req]
  (-> (response "not found!")
      (content-type "text/plain")
      (status 404)))

(defn forbidden [req]
  (-> (response "")
      (content-type "application/json")
      (status 403)))


;(defn login
;  "Checks email/password against database and if authenticated returnes curated
;  user, otherwise :not-authorized"
;  [{:keys [db-conn] :as env} {:keys [email password]}]
;  (let [current-user (user-underscore->hyphen (q/select-user-by-email db-conn {:email email}))
;        authenticated? (and current-user
;                            (password-check password (:encrypted-password current-user)))]
;    (if authenticated?
;      (curate-user env current-user [:remember-token])
;      :not-authorized)))

;; (defn authenticate-user [email password]
;;   (let [user (q/get-user-by-email email)]
;;     (when (and user (not (:deleted user))
;;                (security/password-check password (:encrypted_password user)))
;;       user)))


;; (defn wrap-authenticate [f]
;;   (fn [request]
;;     (if (:skip-wrap-authenticate config)
;;       (f (assoc request :current-user (:skip-wrap-authenticate config)))
;;       (let [remember-token (-> request :cookies (get "remember_token") :value)
;;             user (q/get-user-by-remember-token remember-token)
;;             params (:params request)
;;             user (if user
;;                    user
;;                    (let [email (:email params)
;;                          password (:password params)]
;;                      (when (and email password)
;;                        (authenticate-user
;;                         email
;;                         password))))]
;;         (if (nil? user)
;;           (forbidden request)
;;           (f (assoc request :current-user user)))))))

;; =============================================================================
;; Not used

;; Example handler
(defn app [req]
  (println "app is called")
  (throw (ex-info "oops" {}))                               ;test exception catching
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "Hello HTTP!"})


(defn login!
  "Here's where you'll add your server-side login/auth procedure (Friend, etc.).
  In our simplified example we'll just always successfully authenticate the user
  with whatever user-id they provided in the auth request."
  [ring-request]
  (let [{:keys [session params]} ring-request
        {:keys [user-id]} params]
    (debug "Login request: %s" params)
    (timbre/debugf "Login request: %s" params)
    {:status 200 :session (assoc session :uid user-id)}))
