(ns )

(ns pagora.aum.modules.download.frontend.pdf-plugin.core
  (:require
   [pagora.aum.modules.download.frontend.data :refer [make-data make-url]]
   [cuerdas.core :as str]
   [bilby.frontend.pdfkit.core :as pdfkit]))

;;To use this download plugin move pdfkit.ext.js from js-scripts/pdfkit to a
;;root src cljs path such as src/cljs and add this line to src/cljs/deps.cljs

;;"pdfkit.ext.js"

;;So it becomes something like this:

;; {:foreign-libs []
;;  :externs ["drake.ext.js"
;;            "dragula.ext.js"
;;            "crypto.ext.js"
;;            "bugsnag.ext.js"
;;            "pdfkit.ext.js"]}

;; Move the pdfkit and blob-stream js files from js-scripts/pdfkit to a resource
;; path. Or use the cdn for pdfkit.

(def scripts {:pdfkit {:url "https://cdn.jsdelivr.net/npm/pdfkit@0.10.0/js/pdfkit.standalone.min.js"
                       ;; "/admin_new/pdfkit.standalone.min.js"
                       :global "PDFDocument"}
              :blob-stream {:url
                            ;; "/admin_new/scripts/blob-stream.js"
                            "/admin_new/scripts/blob-stream-min.js"
                            :global "blobStream"}})

(defn load-script [url on-load]
  (let [script (.createElement js/document "script")]
    (set! (.. script -src) url)
    (set! (.. script -onload) on-load)
    (js/document.head.appendChild script)))

(defn load-scripts [{:keys [script-kws on-load]}]
  (let [loaded (atom (zipmap script-kws (repeat false)))]
    (add-watch loaded nil (fn [k ref old-state new-state]
                            (when (every? true? (vals new-state))
                              (on-load))))
    (doseq [script-kw script-kws]
      (let [{:keys [url global]} (get scripts script-kw)]
        (if (aget js/window global)
          (swap! loaded assoc script-kw true)
          (do
            (swap! loaded assoc script-kw false)
            (load-script url #(swap! loaded assoc script-kw true))))))))


(comment
  (defn label-value-line [label value]
    (let [col1-left-pos 50
          col-top 50
          col-width 100
          col2-left-pos (+ col-width col1-left-pos 40)]
      [:style
       [:text {:width col-width} (str (-> label
                                          str/capital
                                          (str/replace "-" " ")) ":")
        col1-left-pos col-top]

       (let [value (if (or (nil? value) (str/blank? value)) " " (str value))]
         [:text {:width col-width} value
          col2-left-pos col-top])])))

;; (defn label-value-line [label value]
;;   [:style
;;    [:text {:indent 0 :move-down -1} (str (-> label
;;                                              str/capital
;;                                              (str/replace "-" " ")) ":")]
;;    ;; (timbre/info :#pp {:label label
;;    ;;                    :value value
;;    ;;                    :blank? (str/blank? value)})

;;    (let [value (if (or (nil? value) (str/blank? value)) " " (str value))]
;;      [:text {:indent 250 } value])])

(defn label-value-line [label value]
   (let [value (if (or (nil? value) (str/blank? value)) " " (str value))]
     [:style
      [:text {:underline false :fill-color "dimgrey" :continued true} (str (-> label
                                        str/capital
                                        (str/replace "-" " ")) ": ")]
      [:text {:underline false :fill-color "black"} value]]))

(defn record-block [record columns]
  (into [:style]
        (->> columns (reduce (fn [block column]
                               (conj block (label-value-line (name column) (get record column))))
                             []))))

(defn make-pdf [rows {:keys [on-finish columns]}]
  (let [pdf [:pdf {:info
                   {;; :title "My Document"
                    ;; :author "Me"
                    ;; :subject "Important Things"
                    ;; :keywords "Wump"
                    }
                   :layout "portrait"}

             (into
              [:page]
              (->> rows (mapv (fn [row]
                                (conj (record-block row columns)
                                      [:text {:move-down 1.2}])
                                ))))]
        pdf-document (pdfkit/pdf pdf)
        blob-stream (.pipe pdf-document (js/blobStream))]

    ;; (aset js/window "blob-stream-finish-called" false)

    (.on blob-stream "finish" #(on-finish (.toBlob blob-stream "application/pdf"))
         ;; #(do
         ;;                         (when-not (aget js/window "blob-stream-finish-called")
         ;;                          ;;For some really weird reason this callback is called twice by blob-stream.
         ;;                          ;;Setting an atom doesn't work, so setting global on window!!!
         ;;                          (aset js/window "blob-stream-finish-called" true)
         ;;                          (on-finish (.toBlob blob-stream "application/pdf"))))
         )
    (.end pdf-document)))

(defmethod make-data :pdf [_ {:keys [rows columns on-data]}]
  (load-scripts {:script-kws [:pdfkit :blob-stream]
                            :on-load #(do
                                        ;; (on-progress {:status :scripts-loaded})
                                        (make-pdf rows {:on-finish on-data
                                                        :columns columns}))}))

(defmethod make-url :pdf [_ {:keys [data]}]
  (js/URL.createObjectURL data))
