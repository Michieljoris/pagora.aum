(ns pagora.aum.modules.download.frontend.data)

(defmulti make-data
  (fn [type rows] type))

(defmulti make-url
  (fn [type] type))

(defmethod make-url :default [_ {:keys [data]}]
  (str "data:text/plain ;charset=utf-8,"
       (js/encodeURIComponent data)))
