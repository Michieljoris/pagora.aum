(ns pagora.aum.om.misc
  (:refer-clojure :exclude #?(:clj  [replace]
                              :cljs [replace]))
  (:require
   [clojure.zip :as zip]
   [pagora.aum.om.util :as util])
  )

(defn replace [template new-query]
  (-> template (zip/replace new-query) zip/root))

(defn- query-zip
  "Return a zipper on a query expression."
  [root]
  (zip/zipper
    #(or (vector? %) (map? %) (seq? %))
    seq
    (fn [node children]
      (let [ret (cond
                  (vector? node) (vec children)
                  (map? node)    (into {} children)
                  (seq? node)    children)]
        (with-meta ret (meta node))))
    root))
;; https://clojuredocs.org/clojure.zip/zipper
;; https://clojuredocs.org/clojure.zip
;; (def zp (query-zip '[{:a [:b :c]} ({:d [:e :f]} {:p 1})]))
;; (-> zp zip/down zip/right zip/down zip/right zip/down zip/down first) ;;=> :p
;; (pp (zip/root zp))
;; (pp (-> zp zip/down first))
;; (pp (-> zp zip/down zip/node))


(defn- move-to-key
  "Move from the current zipper location to the specified key. loc must be a
   hash map node."
  [loc k]
  (loop [loc (zip/down loc)]
    (let [node (zip/node loc)]
      (if (= k (first node))
        (-> loc zip/down zip/right)     ;return value of key
        (recur (zip/right loc))))))     ;move to next key on the right

;; (def zp (query-zip '{:a 1 :b 2 :c 3}))
;; (-> zp zip/down zip/node)
;; (move-to-key zp :b)                     ;=> [2 {...}]


(defn query-template
  "Given a query and a path into a query return a zipper focused at the location
   specified by the path. This location can be replaced to customize / alter
   the query."
  [query path]
  (letfn [(query-template* [loc path]
            (if (empty? path)
              loc
              (let [node (zip/node loc)]
                (if (vector? node) ;; SUBQUERY
                  (recur (zip/down loc) path)
                  (let [[k & ks] path
                        k' (util/expr->key node)]
                    (if (= k k')
                      (if (or (map? node)
                              (and (seq? node) (map? (first node))))
                        (let [loc'  (move-to-key (cond-> loc (seq? node) zip/down) k)
                              node' (zip/node loc')]
                          (if (map? node') ;; UNION
                            (if (seq ks)
                              (recur
                                (zip/replace loc'
                                  (zip/node (move-to-key loc' (first ks))))
                                (next ks))
                              loc')
                            (recur loc' ks))) ;; JOIN
                        (recur (-> loc zip/down zip/down zip/down zip/right) ks)) ;; CALL
                      (recur (zip/right loc) path)))))))]
    (query-template* (query-zip query) path)))


;; (first (query-template [{:a [{:b {:c [:d :e] :f [:g :h]}}]}] [:a :b :f])) ;=> [:g :h]

