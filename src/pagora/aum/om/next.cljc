(ns pagora.aum.om.next
  (:refer-clojure :exclude #?(:clj  [deftype replace var? force]
                              :cljs [var? key replace force]))
  #?(:cljs (:require-macros [pagora.aum.om.next :refer [defui invariant]]))
  (:require
   #?@(:clj  [
              [clojure.main]
              [cljs.core :refer [deftype specify! this-as js-arguments]]
              [clojure.reflect :as reflect]
              ;; [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
              [cljs.util]
              ]
       :cljs [[goog.string :as gstring]
              [taoensso.tufte :as tufte :refer-macros (defnp p profiled profile)]
              [goog.object :as gobj]
              [goog.log :as glog]
              [js.react]
              [js.react-dom]
              [pagora.aum.om.next.cache :as c]])
   [pagora.aum.om.next.impl.parser :as parser]

   [pagora.aum.dev.debug :refer [mark-point now-in-ms warn-when]]
   [pagora.aum.om.db-to-tree :refer [db->tree]]
   ;; [om.tree-to-db :refer [tree->db]]
   ;; [om.reconciler :refer [Reconciler]]
   [pagora.aum.om.focus :refer [focus-query focus->path]]
   [pagora.aum.om.misc :refer [query-template replace]]
            
   [pagora.aum.om.tempid :as tempid]
   [pagora.aum.om.transit :as transit]
   [pagora.aum.om.util :as util]
   [clojure.zip :as zip]
   [pagora.aum.om.next.protocols :as p]
   [clojure.pprint :refer [pprint]]
   [cljs.analyzer :as ana]
   [cljs.analyzer.api :as ana-api]
   [taoensso.timbre :as timbre]
   [clojure.string :as str])

  #?(:clj  (:import [java.io Writer])
     :cljs (:import [goog.debug Console])))
;; (tufte/add-basic-println-handler! {})


;; =============================================================================


;;Defui
;;Utilities
;;Query Protocols & Helpers
;;Reconciler API
;;React bridging
;;Transactions
;;Parser
;;Indexer
;;Reconciler


;; defui
(defn collect-statics [dt]
  (letfn [(split-on-static [forms]
            (split-with (complement '#{static}) forms))
          (split-on-symbol [forms]
            (split-with (complement symbol?) forms))]
    (loop [dt (seq dt) dt' [] statics {:fields {} :protocols []}]
      (if dt
        (let [[pre [_ sym & remaining :as post]] (split-on-static dt)
              dt' (into dt' pre)]
          (if (seq post)
            (cond
              (= sym 'field)
              (let [[field-info dt] (split-at 2 remaining)]
                (recur (seq dt) dt'
                  (update-in statics [:fields] conj (vec field-info))))
              (symbol? sym)
              (let [[protocol-info dt] (split-on-symbol remaining)]
                (recur (seq dt) dt'
                  (update-in statics [:protocols]
                    into (concat [sym] protocol-info))))
              :else (throw #?(:clj  (IllegalArgumentException. "Malformed static")
                              :cljs (js/Error. "Malformed static"))))
            (recur nil dt' statics)))
        {:dt dt' :statics statics}))))


;; (collect-statics '(defui Bla
;;                     static field :a 1
;;                     static IQuery
;;                     (query [this] :foo)
;;                     static Ident
;;                     Object
;;                     (render [this] :bar)))
;; => {:dt [defui Bla Object (render [this] :bar)], :statics {:fields {:a 1}, :protocols [IQuery (query [this] :foo) Ident]}}

(defn- validate-statics [dt];
  (when-let [invalid (some #{"Ident" "IQuery" "IQueryParams"}
                       (map #(-> % str (str/split #"/") last)
                         (filter symbol? dt)))]
     (throw
       #?(:clj  (IllegalArgumentException.
                  (str invalid " protocol declaration must appear with `static`."))
          :cljs (js/Error.
                  (str invalid " protocol declaration must appear with `static`."))))))

(def lifecycle-sigs
  '{initLocalState [this]
    shouldComponentUpdate [this next-props next-state]
    componentWillReceiveProps [this next-props]
    componentWillUpdate [this next-props next-state]
    componentDidUpdate [this prev-props prev-state]
    componentWillMount [this]
    componentDidMount [this]
    componentWillUnmount [this]
    render [this]})

(defn validate-sig [[name sig :as method]]
  (let [sig' (get lifecycle-sigs name)]
    (assert (= (count sig') (count sig))
      (str "Invalid signature for " name " got " sig ", need " sig'))))

#?(:clj
   (def reshape-map-clj
     {:reshape
      {'render
       (fn [[name [this :as args] & body]]
         `(~name [this#]
           (let [~this this#]
             (binding [pagora.aum.om.next/*reconciler* (pagora.aum.om.ext/get-reconciler this#)
                       pagora.aum.om.next/*depth*      (inc (pagora.aum.om.next/depth this#))
                       pagora.aum.om.next/*shared*     (pagora.aum.om.next/shared this#)
                       pagora.aum.om.next/*instrument* (pagora.aum om.next/instrument this#)
                       pagora.aum.om.next/*parent*     this#]
               (let [ret# (do ~@body)
                     props# (:props this#)]
                 (when-not @(:omcljs$mounted? props#)
                   (swap! (:omcljs$mounted? props#) not))
                 ret#)))))
       'componentWillMount
       (fn [[name [this :as args] & body]]
         `(~name [this#]
           (let [~this    this#
                 indexer# (get-in (pagora.aum.om.next/get-reconciler this#) [:config :indexer])]
             (when-not (nil? indexer#)
               (pagora.aum.om.next.protocols/index-component! indexer# this#))
             ~@body)))}
      :defaults
      `{~'initLocalState
        ([this#])
        ~'componentWillMount
        ([this#]
         (let [indexer# (get-in (pagora.aum.om.next/get-reconciler this#) [:config :indexer])]
           (when-not (nil? indexer#)
             (pagora.aum.om.next.protocols/index-component! indexer# this#))))
        ~'render
        ([this#])}}))

(def reshape-map
  {:reshape
   {'initLocalState
    (fn [[name [this :as args] & body]]
      `(~name ~args
         (let [ret# (do ~@body)]
           (cljs.core/js-obj "omcljs$state" ret#))))
    'componentWillReceiveProps
    (fn [[name [this next-props :as args] & body]]
      `(~name [this# next-props#]
         (let [~this this#
               ~next-props (pagora.aum.om.next/-next-props next-props# this#)]
           ~@body)))
    'componentWillUpdate
    (fn [[name [this next-props next-state :as args] & body]]
      `(~name [this# next-props# next-state#]
         (let [~this       this#
               ~next-props (pagora.aum.om.next/-next-props next-props# this#)
               ~next-state (or (goog.object/get next-state# "omcljs$pendingState")
                               (goog.object/get next-state# "omcljs$state"))
               ret#        (do ~@body)]
           (when (cljs.core/implements? pagora.aum.om.next/Ident this#)
             (let [ident# (pagora.aum.om.next/ident this# (pagora.aum.om.next/props this#))
                   next-ident# (pagora.aum.om.next/ident this# ~next-props)]
               (when (not= ident# next-ident#)
                 (let [idxr# (get-in (pagora.aum.om.next/get-reconciler this#) [:config :indexer])]
                   (when-not (nil? idxr#)
                     (swap! (:indexes idxr#)
                       (fn [indexes#]
                         (-> indexes#
                           (update-in [:ref->components ident#] disj this#)
                           (update-in [:ref->components next-ident#] (fnil conj #{}) this#)))))))))
           (pagora.aum.om.next/merge-pending-props! this#)
           (pagora.aum.om.next/merge-pending-state! this#)
           ret#)))
    'componentDidUpdate
    (fn [[name [this prev-props prev-state :as args] & body]]
      `(~name [this# prev-props# prev-state#]
         (let [~this       this#
               ~prev-props (pagora.aum.om.next/-prev-props prev-props# this#)
               ~prev-state (goog.object/get prev-state# "omcljs$previousState")]
           ~@body
           (pagora.aum.om.next/clear-prev-props! this#))))
    'componentWillMount
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this    this#
               indexer# (get-in (pagora.aum.om.next/get-reconciler this#) [:config :indexer])]
           (when-not (nil? indexer#)
             (pagora.aum.om.next.protocols/index-component! indexer# this#))
           ~@body)))
    'componentWillUnmount
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this    this#
               r#       (pagora.aum.om.next/get-reconciler this#)
               cfg#     (:config r#)
               st#      (:state cfg#)
               indexer# (:indexer cfg#)]
           (when (and (not (nil? st#))
                      (get-in @st# [:pagora.aum.om.next/queries this#]))
             (swap! st# update-in [:pagora.aum.om.next/queries] dissoc this#))
           (when-not (nil? indexer#)
             (pagora.aum.om.next.protocols/drop-component! indexer# this#))
           ~@body)))
    'render
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this this#]
           ;; (timbre/info :#b "+++++++++++++++++++++++++++++++++++++++++++++")
           (binding [pagora.aum.om.next/*reconciler* (pagora.aum.om.next/get-reconciler this#)
                     pagora.aum.om.next/*depth*      (inc (pagora.aum.om.next/depth this#))
                     pagora.aum.om.next/*shared*     (pagora.aum.om.next/shared this#)
                     pagora.aum.om.next/*instrument* (pagora.aum.om.next/instrument this#)
                     pagora.aum.om.next/*parent*     this#]
             ;; (timbre/info "from body of ")
             ;; (js/console.log this#)
            ~@body))))}
   :defaults
   `{~'isMounted
     ([this#]
      (boolean
        (or (some-> this# .-_reactInternalFiber .-stateNode)
            ;; Pre React 16 support. Remove when we don't wish to support
            ;; React < 16 anymore - Antonio
            (some-> this# .-_reactInternalInstance .-_renderedComponent))))
     ~'shouldComponentUpdate
     ([this# next-props# next-state#]
      (let [next-children# (. next-props# -children)
            next-props# (goog.object/get next-props# "omcljs$value")
            next-props# (cond-> next-props#
                          (instance? pagora.aum.om.next/OmProps next-props#) pagora.aum.om.next/unwrap)]
        (or (not= (pagora.aum.om.next/props this#)
                  next-props#)
            (and (.. this# ~'-state)
                 (not= (goog.object/get (. this# ~'-state) "omcljs$state")
                       (goog.object/get next-state# "omcljs$state")))
            (not= (.. this# -props -children)
                  next-children#))))
     ~'componentWillUpdate
     ([this# next-props# next-state#]
      (when (cljs.core/implements? pagora.aum.om.next/Ident this#)
        (let [ident# (pagora.aum.om.next/ident this# (pagora.aum.om.next/props this#))
              next-ident# (pagora.aum.om.next/ident this# (pagora.aum.om.next/-next-props next-props# this#))]
          (when (not= ident# next-ident#)
            (let [idxr# (get-in (pagora.aum.om.next/get-reconciler this#) [:config :indexer])]
              (when-not (nil? idxr#)
                (swap! (:indexes idxr#)
                  (fn [indexes#]
                    (-> indexes#
                      (update-in [:ref->components ident#] disj this#)
                      (update-in [:ref->components next-ident#] (fnil conj #{}) this#)))))))))
       (pagora.aum.om.next/merge-pending-props! this#)
       (pagora.aum.om.next/merge-pending-state! this#))
     ~'componentDidUpdate
     ([this# prev-props# prev-state#]
       (pagora.aum.om.next/clear-prev-props! this#))
     ~'componentWillMount
     ([this#]
       (let [indexer# (get-in (pagora.aum.om.next/get-reconciler this#) [:config :indexer])]
         (when-not (nil? indexer#)
           (pagora.aum.om.next.protocols/index-component! indexer# this#))))
     ~'componentWillUnmount
     ([this#]
       (let [r#       (pagora.aum.om.next/get-reconciler this#)
             cfg#     (:config r#)
             st#      (:state cfg#)
             indexer# (:indexer cfg#)]
         (when (and (not (nil? st#))
                    (get-in @st# [:pagora.aum.om.next/queries this#]))
           (swap! st# update-in [:pagora.aum.om.next/queries] dissoc this#))
         (when-not (nil? indexer#)
           (pagora.aum.om.next.protocols/drop-component! indexer# this#))))}})

;; Takes dt seq and 'reshapes' existing react methods to om-next versions.
;; Basically prepending code to passed in code. If react method isn't defined,
;; adds default for a number of methods, see reshape method above..
(defn reshape [dt {:keys [reshape defaults]}]
  (letfn [(reshape* [x]
            (if (and (sequential? x)
                     (contains? reshape (first x)))
              (let [reshapef (get reshape (first x))]
                (validate-sig x)
                (reshapef x))
              x))
          (add-defaults-step [ret [name impl]]
            (if-not (some #{name} (map first (filter seq? ret)))
              (let [[before [p & after]] (split-with (complement '#{Object}) ret)]
                (into (conj (vec before) p (cons name impl)) after))
              ret))
          (add-defaults [dt]
            (reduce add-defaults-step dt defaults))
          (add-object-protocol [dt]
            (if-not (some '#{Object} dt)
              (conj dt 'Object)
              dt))]
    (->> dt (map reshape*) vec add-object-protocol add-defaults)))

#?(:clj
   (defn- add-proto-methods* [pprefix type type-sym [f & meths :as form]]
     (let [pf (str pprefix (name f))
           emit-static (when (-> type-sym meta :static)
                         `(~'js* "/** @nocollapse */"))]
       (if (vector? (first meths))
         ;; single method case
         (let [meth meths]
           [`(do
               ~emit-static
               (set! ~(#'cljs.core/extend-prefix type-sym (str pf "$arity$" (count (first meth))))
                 ~(with-meta `(fn ~@(#'cljs.core/adapt-proto-params type meth)) (meta form))))])
         (map (fn [[sig & body :as meth]]
                `(do
                   ~emit-static
                   (set! ~(#'cljs.core/extend-prefix type-sym (str pf "$arity$" (count sig)))
                     ~(with-meta `(fn ~(#'cljs.core/adapt-proto-params type meth)) (meta form)))))
           meths)))))

#?(:clj (intern 'cljs.core 'add-proto-methods* add-proto-methods*))

#?(:clj
   (defmacro update-protocol-var [p type env]
     (let [{:keys [major minor qualifier]} cljs.util/*clojurescript-version*]
       (if (or (> major 1)
               (and (== major 1) (> minor 10))
               (and (== major 1) (== minor 10) (>= qualifier 597)))
         `(#'cljs.core/update-protocol-var ~p ~type ~env)
         `(#'cljs.core/warn-and-update-protocol ~p ~type ~env)))))

#?(:clj
   (defn- proto-assign-impls [env resolve type-sym type [p sigs]]
     (update-protocol-var p type env)
     (let [psym      (resolve p)
           pprefix   (#'cljs.core/protocol-prefix psym)
           skip-flag (set (-> type-sym meta :skip-protocol-flag))
           static?   (-> p meta :static)
           type-sym  (cond-> type-sym
                       static? (vary-meta assoc :static true))
           emit-static (when static?
                         `(~'js* "/** @nocollapse */"))]
       (if (= p 'Object)
         (#'cljs.core/add-obj-methods type type-sym sigs)
         (concat
           (when-not (skip-flag psym)
             (let [{:keys [major minor qualifier]} cljs.util/*clojurescript-version*]
               (if (or (> major 1)
                       (and (== major 1) (> minor 9))
                       (and (== major 1) (== minor 9) (>= qualifier 293)))
                [`(do
                    ~emit-static
                    (set! ~(#'cljs.core/extend-prefix type-sym pprefix) cljs.core/PROTOCOL_SENTINEL))]
                [`(do
                    ~emit-static
                    (set! ~(#'cljs.core/extend-prefix type-sym pprefix) true))])))
           (mapcat
             (fn [sig]
               (if (= psym 'cljs.core/IFn)
                 (#'cljs.core/add-ifn-methods type type-sym sig)
                 (#'cljs.core/add-proto-methods* pprefix type type-sym sig)))
             sigs))))))

#?(:clj (intern 'cljs.core 'proto-assign-impls proto-assign-impls))

#?(:clj (defn- extract-static-methods [protocols]
          (letfn [(add-protocol-method [existing-methods method]
                    (let [nm              (first method)
                          new-arity       (rest method)
                          k               (keyword nm)
                          existing-method (get existing-methods k)]
                      (if existing-method
                        (let [single-arity?    (vector? (second existing-method))
                              existing-arities (if single-arity?
                                                 (list (rest existing-method))
                                                 (rest existing-method))]
                          (assoc existing-methods k (conj existing-arities new-arity 'fn)))
                        (assoc existing-methods k (list 'fn new-arity)))))]
            (when-not (empty? protocols)
              (let [result (->> protocols
                             (filter #(not (symbol? %)))
                             (reduce
                               (fn [r impl] (add-protocol-method r impl))
                               {}))]
                (if (contains? result :params)
                  result
                  (assoc result :params '(fn [this]))))))))

#?(:clj
   (defn defui*-clj [name forms]
     (let [docstring (when (string? (first forms))
                       (first forms))
           forms (cond-> forms
                   docstring rest)
           {:keys [dt statics]} (collect-statics forms)
           [other-protocols obj-dt] (split-with (complement '#{Object}) dt)
           klass-name (symbol (str name "_klass"))
           lifecycle-method-names (set (keys lifecycle-sigs))
           {obj-dt false non-lifecycle-dt true} (group-by
                                                  (fn [x]
                                                    (and (sequential? x)
                                                         (not (lifecycle-method-names (first x)))))
                                                  obj-dt)
           class-methods (extract-static-methods (:protocols statics))]
       `(do
          ~(when-not (empty? non-lifecycle-dt)
             `(defprotocol ~(symbol (str name "_proto"))
                ~@(map (fn [[m-name args]] (list m-name args)) non-lifecycle-dt)))
          (declare ~name)
          (defrecord ~klass-name [~'state ~'refs ~'props ~'children]
            ;; TODO: non-lifecycle methods defined in the JS prototype - António
            pagora.aum.om.next.protocols/IReactLifecycle
            ~@(rest (reshape obj-dt reshape-map-clj))

            ~@other-protocols

            ~@(:protocols statics)

            ~@(when-not (empty? non-lifecycle-dt)
                (list* (symbol (str name "_proto"))
                  non-lifecycle-dt))

            pagora.aum.om.next.protocols/IReactComponent
            (~'-render [this#]
             (p/componentWillMount this#)
             (p/render this#)))
          (defmethod clojure.core/print-method ~(symbol (str (munge *ns*) "." klass-name))
            [o# ^Writer w#]
            (.write w# (str "#object[" (ns-name *ns*) "/" ~(str name) "]")))
          (let [c# (fn ~name [state# refs# props# children#]
                     (~(symbol (str (munge *ns*) "." klass-name ".")) state# refs# props# children#))]
            (def ~(with-meta name
                    (merge (meta name)
                      (when docstring
                        {:doc docstring})))
              (with-meta c#
                (merge {:component c#
                        :component-ns (ns-name *ns*)
                        :component-name ~(str name)}
                  ~class-methods))))))))

;; This returns a piece of code that if executed defines a constructor function
;; named name, and some more code to set prototype to make the function a
;; constructor for a React component
;; for specify! see:
;; http://davedellacosta.com/cljs-protocols
;; http://dev.clojure.org/display/design/specify+i.e.+reify+for+instances
(defn defui*
  ([name form] (defui* name form nil))
  ([name forms env]
   (letfn [(field-set! [obj [field value]]
             `(set! (. ~obj ~(symbol (str "-" field))) ~value))]
     (let [docstring (when (string? (first forms))
                       (first forms))
           forms (cond-> forms
                   docstring rest)
           {:keys [dt statics]} (collect-statics forms)
           _ (validate-statics dt)
           rname (if env
                   (:name (ana/resolve-var (dissoc env :locals) name))
                   name)
           ctor  `(defn ~(with-meta name
                           (merge {:jsdoc ["@constructor"]}
                             (meta name)
                             (when docstring
                               {:doc docstring})))
                    []
                    (this-as this#
                      (.apply js/React.Component this# (js-arguments))
                      (if-not (nil? (.-initLocalState this#))
                        (set! (.-state this#) (.initLocalState this#))
                        (set! (.-state this#) (cljs.core/js-obj)))
                      this#))
           set-react-proto! `(set! (.-prototype ~name)
                                 (goog.object/clone js/React.Component.prototype))
           ctor  (if (-> name meta :once)
                   `(when-not (cljs.core/exists? ~name)
                      ~ctor
                      ~set-react-proto!)
                   `(do
                      ~ctor
                      ~set-react-proto!))
           display-name (if env
                          (str (-> env :ns :name) "/" name)
                          'js/undefined)]
       `(do
          ~ctor
          (specify! (.-prototype ~name) ~@(reshape dt reshape-map))
          (set! (.. ~name -prototype -constructor) ~name)
          (set! (.. ~name -prototype -constructor -displayName) ~display-name)
          (set! (.. ~name -prototype -om$isComponent) true)
          ~@(map #(field-set! name %) (:fields statics))
          (specify! ~name
            ~@(mapv #(cond-> %
                       (symbol? %) (vary-meta assoc :static true)) (:protocols statics)))
          (specify! (. ~name ~'-prototype) ~@(:protocols statics))
          (set! (.-cljs$lang$type ~rname) true)
          (set! (.-cljs$lang$ctorStr ~rname) ~(str rname))
          (set! (.-cljs$lang$ctorPrWriter ~rname)
            (fn [this# writer# opt#]
              (cljs.core/-write writer# ~(str rname)))))))))

(defmacro defui [name & forms]
  (if (boolean (:ns &env))
    (defui* name forms &env)
    #?(:clj (defui*-clj name forms))))

;; (clojure.pprint/pprint (defui* Bla '(static Ident (ident [this] :foo)
;;                                             static IQuery (query [this] bar)
;;                                             static field foo "aaaaaaaaaaaaaaaaaaaaaaaaaa")))

(defmacro ui
  [& forms]
  (let [t (with-meta (gensym "ui_") {:anonymous true})]
    `(do (defui ~t ~@forms) ~t)))


;; TODO: #?:clj invariant - António
(defn invariant*
  [condition message env]
  (let [opts (ana-api/get-options)
        fn-scope (:fn-scope env)
        fn-name (some-> fn-scope first :name str)]
    (when-not (:elide-asserts opts)
      `(let [l# pagora.aum.om.next/*logger*]
         (when-not ~condition
           (goog.log/error l#
             (str "Invariant Violation"
               (when-not (nil? ~fn-name)
                 (str " (in function: `" ~fn-name "`)"))
               ": " ~message)))))))

;; Kind of an assert that logs error on condition = false, and uses google console
;; logger or configured logger. Logs fn where invariant fails if it can.
(defmacro invariant
  [condition message]
  (when (boolean (:ns &env))
    (invariant* condition message &env)))

#?(:clj
   (defmethod print-method clojure.lang.AFunction
     [o ^Writer w]
     (let [obj-ns   (-> o meta :component-ns)
           obj-name (-> o meta :component-name)]
       (if obj-name
         (.write w (str obj-ns "/" obj-name))
         (#'clojure.core/print-object o w)))))

;; =============================================================================
;; CLJS

;; If goog.DEBUG==true logs to console, and calls possible om.next logger
#?(:cljs
   (defonce ^{:dynamic true} *logger*
     (when ^boolean goog.DEBUG
       (.setCapturing (Console.) true)
       (glog/getLogger "om.next2"))))

;; =============================================================================
;; Globals & Dynamics

(def ^:private roots (atom {}))
(def ^{:dynamic true} *raf* nil)

;; Rebound in render of component, see defui.
;; Set as public because of beautiful-dnd. It needs rebinding in the render-item callback.
(def ^{:dynamic true :private false } *reconciler* nil)
(def ^{:dynamic true :private false} *parent* nil)
(def ^{:dynamic true :private false} *shared* nil)
(def ^{:dynamic true :private false} *instrument* nil)
(def ^{:dynamic true :private false} *depth* 0)

;; =============================================================================
;; Utilities

(defn nil-or-map?
  #?(:cljs {:tag boolean})
   [x]
  (or (nil? x) (map? x)))

;; =============================================================================
;; Query Protocols & Helpers

(defprotocol Ident
  (ident [this props] "Return the ident for this component"))

(defprotocol IQueryParams
  (params [this] "Return the query parameters"))

#?(:clj  (extend-type Object
           IQueryParams
           (params [this]
             (when-let [ps (-> this meta :params)]
               (ps this))))
   :cljs (extend-type default
           IQueryParams
           (params [_])))

(defprotocol IQuery
  (query [this] "Return the component's unbound query"))

(defprotocol ILocalState
  (-set-state! [this new-state] "Set the component's local state")
  (-get-state [this] "Get the component's local state")
  (-get-rendered-state [this] "Get the component's rendered local state")
  (-merge-pending-state! [this] "Get the component's pending local state"))

(defn- var? [x]
  (and (symbol? x)
       #?(:clj  (.startsWith (str x) "?")
          :cljs (gstring/startsWith (str x) "?"))))

(defn- var->keyword [x]
  (keyword (.substring (str x) 1)))

(defn- replace-var [expr params]
  (if (var? expr)
    (get params (var->keyword expr) expr)
    expr))

;; Recursively replaces all istances of variables ('?some-var) in query with the
;; value in params
(defn bind-query [query params]
  (let [qm (meta query)
        tr (map #(bind-query % params))
        ret (cond
              (seq? query) (apply list (into [] tr query))
              #?@(:cljs [(and (exists? cljs.core/IMapEntry)
                              (implements? ^:cljs.analyzer/no-resolve cljs.core/IMapEntry query))
                         (into [] tr query)])
              #?@(:clj [(instance? clojure.lang.IMapEntry query) (into [] tr query)])
              (coll? query) (into (empty query) tr query)
              :else (replace-var query params))]
    (cond-> ret
      (and qm #?(:clj  (instance? clojure.lang.IObj ret)
                 :cljs (satisfies? IMeta ret)))
      (with-meta qm))))

 ;; (bind-query [{[:group/by-id '?v] [:id :name]} '?v `(~'?v) (list '?v) {'?v '?v} (list {'?v ['?v '?v]} {'?v '?v})] {:v :foo})
;; => [{[:group/by-id :foo] [:id :name]} :foo (:foo) (:foo) {:foo :foo} ({:foo [:foo :foo]} {:foo :foo})]
;; (bind-query {:set-group-id true
;;              :remote-query [:id :name]
;;              :where [:and [[:group-id '?group-id] [:template := 1]]]
;;              ;; :where [:template := 1]
;;              :with-meta :count
;;              } {:group-id 10})

(declare component? get-reconciler props class-path get-indexer path react-type)

(defn- component->query-data [component]
  (some-> (get-reconciler component)
    :config :state deref ::queries (get component)))

(defn get-unbound-query
  "Return the unbound query for a component."
  [component]
  (:query (component->query-data component) (query component)))

(defn get-params
  "Return the query params for a component."
  [component]
  (:params (component->query-data component) (params component)))

(defn- get-component-query
  ([component]
   (get-component-query component (component->query-data component)))
  ([component query-data]
   (let [q  (:query query-data (query component))
         c' (-> q meta :component)]
     (assert (nil? c')
       (str "Query violation, " component " reuses " c' " query"))
     (with-meta
       (bind-query q (:params query-data (params component)))
       {:component (react-type component)}))))

(defn iquery?
  #?(:cljs {:tag boolean})
  [x]
  #?(:clj  (if (fn? x)
             (some? (-> x meta :query))
             (let [class (cond-> x (component? x) class)]
               (extends? IQuery class)))
     :cljs (implements? IQuery x)))

(defn- get-class-or-instance-query
  "Return a IQuery/IParams local bound query. Works for component classes
   and component instances. Does not use the indexer."
  [x]
  (if (component? x)
    (get-component-query x)
    (let [q #?(:clj  ((-> x meta :query) x)
               :cljs (query x))
          c (-> q meta :component)]
      (assert (nil? c) (str "Query violation, " x , " reuses " c " query"))
      (with-meta (bind-query q (params x)) {:component x}))))

(defn- get-indexed-query
  "Get a component's static query from the indexer. For recursive queries, recurses
   up the data path. Falls back to `get-class-or-instance-query` if nothing is
   found in the indexer."
  [component class-path-query-data data-path]
  (let [qs (filter #(= data-path (-> % zip/root (focus->path data-path)))
             class-path-query-data)
        qs (if (empty? qs) class-path-query-data qs)]
    (if-not (empty? qs)
      (let [q (first qs)
            node (zip/node q)]
        (if-not (util/recursion? node)
          node
          (recur component class-path-query-data (pop data-path))))
      (get-class-or-instance-query component))))

(defn get-query
  "Return a IQuery/IParams instance bound query. Works for component classes
   and component instances. See also pagora.aum.om.next/full-query."
  [x]
  (when #?(:clj  (iquery? x)
           :cljs (implements? IQuery x))
    (if (component? x)
      (if-let [query-data (component->query-data x)]
        (get-component-query x query-data)
        (let [cp (class-path x)
              r (get-reconciler x)
              data-path (into [] (remove number?) (path x))
              class-path-query-data (get (:class-path->query @(get-indexer r)) cp)]
          (get-indexed-query x class-path-query-data data-path)))
      (get-class-or-instance-query x))))

(defn tag [x class]
  (vary-meta x assoc :component class))

;; =============================================================================
;; React Bridging

#?(:cljs (deftype ^:private OmProps [props basis-t]))

#?(:cljs
   (defn- om-props [props basis-t]
     (OmProps. props basis-t)))

#?(:cljs
   (defn- om-props-basis [om-props]
     (.-basis-t om-props)))

#?(:cljs (def ^:private nil-props (om-props nil -1)))

#?(:cljs
   (defn- unwrap [om-props]
     (.-props om-props)))

#?(:clj
   (defn- munge-component-name [x]
     (let [ns-name (-> x meta :component-ns)
           cl-name (-> x meta :component-name)]
       (munge
         (str (str/replace (str ns-name) "." "$") "$" cl-name)))))

#?(:clj
   (defn- compute-react-key [cl props]
     (when-let [idx (-> props meta :om-path)]
       (str (munge-component-name cl) "_" idx))))

#?(:cljs
   (defn- compute-react-key [cl props]
     (if-let [rk (:react-key props)]
       rk
       (if-let [idx (-> props meta :om-path)]
         (str (. cl -name) "_" idx)
         js/undefined))))

#?(:clj
   (defn factory
     "Create a factory constructor from a component class created with
      pagora.aum.om.next/defui."
     ([class]
      (factory class nil))
     ([class {:keys [validator keyfn instrument?]
              :or {instrument? true} :as opts}]
      {:pre [(fn? class)]}
      (fn self
        ([] (self nil))
        ([props & children]
         (when-not (nil? validator)
           (assert (validator props)))
         (if (and *instrument* instrument?)
           (*instrument*
             {:props    props
              :children children
              :class    class
              :factory  (factory class (assoc opts :instrument? false))})
           (let [react-key (cond
                             (some? keyfn) (keyfn props)
                             (some? (:react-key props)) (:react-key props)
                             :else (compute-react-key class props))
                 ctor class
                 ref (:ref props)
                 props {:omcljs$reactRef   ref
                        :omcljs$reactKey   react-key
                        :omcljs$value      (cond-> props
                                             (map? props) (dissoc :ref))
                        :omcljs$mounted?   (atom false)
                        :omcljs$path       (-> props meta :om-path)
                        :omcljs$reconciler *reconciler*
                        :omcljs$parent     *parent*
                        :omcljs$shared     *shared*
                        :omcljs$instrument *instrument*
                        :omcljs$depth      *depth*}
                 component (ctor (atom nil) (atom nil) props children)]
             (when ref
               (assert (some? *parent*))
               (swap! (:refs *parent*) assoc ref component))
             (reset! (:state component) (.initLocalState component))
             component)))))))

#?(:cljs
   (defn factory
     "Create a factory constructor from a component class created with
      pagora.aum.om.next/defui."
     ([class] (factory class nil))
     ([class {:keys [validator keyfn instrument?]
              :or {instrument? true} :as opts}]
      {:pre [(fn? class)]}
      (fn self [props & children]
        (when-not (nil? validator)
          (assert (validator props)))
        (if (and *instrument* instrument?)
          (*instrument*
            {:props    props
             :children children
             :class    class
             :factory  (factory class (assoc opts :instrument? false))})
          (let [key (if-not (nil? keyfn)
                      (keyfn props)
                      (compute-react-key class props))
                ref (:ref props)
                ref (cond-> ref (keyword? ref) str)
                t   (if-not (nil? *reconciler*)
                      (p/basis-t *reconciler*)
                      0)]
            ;; (timbre/info :#r "Creating element")
            ;; (js/console.log class)
            ;; (js/console.log *reconciler*)
            ;; (js/console.log  *shared*)
            ;; (js/console.log  *parent*)
            ;; (js/console.log  *depth*)
            (js/React.createElement class
              #js {:key               key
                   :ref               ref
                   :omcljs$reactKey   key
                   :omcljs$value      (om-props props t)
                   :omcljs$path       (-> props meta :om-path)
                   :omcljs$reconciler *reconciler*
                   :omcljs$parent     *parent*
                   :omcljs$shared     *shared*
                   :omcljs$instrument *instrument*
                   :omcljs$depth      *depth*}
              (util/force-children children))))))))

(defn component?
  "Returns true if the argument is an Om component."
  #?(:cljs {:tag boolean})
  [x]
  (if-not (nil? x)
    #?(:clj  (or (instance? pagora.aum.om.next.protocols.IReactComponent x)
                 (satisfies? p/IReactComponent x))
       :cljs (true? (. x -om$isComponent)))
    false))

(defn- state [c]
  {:pre [(component? c)]}
  (.-state c))

(defn- get-prop
  "PRIVATE: Do not use"
  [c k]
  #?(:clj  (get (:props c) k)
     :cljs (gobj/get (.-props c) k)))

#?(:cljs
   (defn- get-props*
     [x k]
     (if (nil? x)
       nil-props
       (let [y (gobj/get x k)]
         (if (nil? y)
           nil-props
           y)))))

#?(:cljs
   (defn- get-prev-props [x]
     (get-props* x "omcljs$prev$value")))

#?(:cljs
   (defn- get-next-props [x]
     (get-props* x "omcljs$next$value")))

#?(:cljs
   (defn- get-props [x]
     (get-props* x "omcljs$value")))

#?(:cljs
   (defn- set-prop!
     "PRIVATE: Do not use"
     [c k v]
     (gobj/set (.-props c) k v)))

(defn get-reconciler
  [c]
  {:pre [(component? c)]}
  (get-prop c #?(:clj  :omcljs$reconciler
                 :cljs "omcljs$reconciler")))

#?(:cljs
   (defn- props*
     ([x y]
      (max-key om-props-basis x y))
     ([x y z]
      (max-key om-props-basis x (props* y z)))))

#?(:cljs
   (defn- prev-props*
     ([x y]
      (min-key om-props-basis x y))
     ([x y z]
      (min-key om-props-basis
        (props* x y) (props* y z)))))

#?(:cljs
   (defn -prev-props [prev-props component]
     (let [cst   (.-state component)
           props (.-props component)]
       (unwrap
         (prev-props*
           (props* (get-props prev-props) (get-prev-props cst))
           (props* (get-props cst) (get-props props)))))))

#?(:cljs
   (defn -next-props [next-props component]
     (unwrap
       (props*
         (-> component .-props get-props)
         (get-props next-props)
         (-> component .-state get-next-props)))))

#?(:cljs
   (defn- merge-pending-props! [c]
     {:pre [(component? c)]}
     (let [cst     (. c -state)
           props   (.-props c)
           pending (gobj/get cst "omcljs$next$value")
           prev    (props* (get-props cst) (get-props props))]
       (gobj/set cst "omcljs$prev$value" prev)
       (when-not (nil? pending)
         (gobj/remove cst "omcljs$next$value")
         (gobj/set cst "omcljs$value" pending)))))

#?(:cljs
   (defn- clear-prev-props! [c]
     (gobj/remove (.-state c) "omcljs$prev$value")))

#?(:cljs
   (defn- t
     "Get basis t value for when the component last read its props from
      the global state."
     [c]
     (om-props-basis
       (props*
         (-> c .-props get-props)
         (-> c .-state get-props)))))

(defn- parent
   "Returns the parent Om component."
   [component]
   (get-prop component #?(:clj  :omcljs$parent
                          :cljs "omcljs$parent")))

(defn depth
   "PRIVATE: Returns the render depth (a integer) of the component relative to
    the mount root."
   [component]
   (when (component? component)
     (get-prop component #?(:clj  :omcljs$depth
                            :cljs "omcljs$depth"))))

(defn react-key
   "Returns the components React key."
   [component]
   (get-prop component #?(:clj  :omcljs$reactKey
                          :cljs "omcljs$reactKey")))

#?(:clj
   (defn react-type
     "Returns the component type, regardless of whether the component has been
      mounted"
     [component]
     {:pre [(component? component)]}
     (let [[klass-name] (str/split (reflect/typename (type component)) #"_klass")
           last-idx-dot (.lastIndexOf klass-name ".")
           ns (clojure.main/demunge (subs klass-name 0 last-idx-dot))
           c (subs klass-name (inc last-idx-dot))]
       @(or (find-var (symbol ns c))
            (find-var (symbol ns (clojure.main/demunge c)))))))

#?(:cljs
   (defn react-type
     "Returns the component type, regardless of whether the component has been
      mounted"
     [x]
     (or (gobj/get x "type") (type x))))

(defn- path
  "Returns the component's Om data path."
  [c]
  (get-prop c #?(:clj  :omcljs$path
                 :cljs "omcljs$path")))

(defn shared
  "Return the global shared properties of the Om Next root. See :shared and
   :shared-fn reconciler options."
  ([component]
   (shared component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [shared #?(:clj  (get-prop component :omcljs$shared)
                   :cljs (gobj/get (. component -props) "omcljs$shared"))
         ks     (cond-> k-or-ks
                  (not (sequential? k-or-ks)) vector)]
     (cond-> shared
       (not (empty? ks)) (get-in ks)))))

(defn instrument [component]
  {:pre [(component? component)]}
  (get-prop component #?(:clj  :omcljs$instrument
                         :cljs "omcljs$instrument")))

#?(:cljs
   (defn- update-props! [c next-props]
     {:pre [(component? c)]}
     ;; We cannot write directly to props, React will complain
     (doto (.-state c)
       (gobj/set "omcljs$next$value"
         (om-props next-props (p/basis-t (get-reconciler c)))))))

#?(:clj
   (defn props [component]
     {:pre [(component? component)]}
     (:omcljs$value (:props component))))

#?(:cljs
   (defn props
     "Return a components props."
     [component]
     {:pre [(component? component)]}
     ;; When force updating we write temporarily props into state to avoid bogus
     ;; complaints from React. We record the basis T of the reconciler to determine
     ;; if the props recorded into state are more recent - props will get updated
     ;; when React actually re-renders the component.
     (unwrap
       (props*
         (-> component .-props get-props)
         (-> component .-state get-props)))))

(defn computed
  "Add computed properties to props. Note will replace any pre-existing
   computed properties."
  [props computed-map]
  (when-not (nil? props)
    (if (vector? props)
      (cond-> props
        (not (empty? computed-map)) (vary-meta assoc :om.next/computed computed-map))
      (cond-> props
        (not (empty? computed-map)) (assoc :om.next/computed computed-map)))))

(defn get-computed
  "Return the computed properties on a component or its props."
  ([x]
   (get-computed x []))
  ([x k-or-ks]
   (when-not (nil? x)
     (let [props (cond-> x (component? x) props)
           ks    (into [:om.next/computed]
                   (cond-> k-or-ks
                     (not (sequential? k-or-ks)) vector))]
       (if (vector? props)
         (-> props meta (get-in ks))
         (get-in props ks))))))

(declare schedule-render!)

#?(:clj
   (defn set-state!
     [component new-state]
     {:pre [(component? component)]}
     (if (satisfies? ILocalState component)
       (-set-state! component new-state)
       (reset! (:state component) new-state))))

#?(:cljs
   (defn set-state!
     "Set the component local state of the component. Analogous to React's
   setState."
     [component new-state]
     {:pre [(component? component)]}
     (if (implements? ILocalState component)
       (-set-state! component new-state)
       (gobj/set (.-state component) "omcljs$pendingState" new-state))
     (if-let [r (get-reconciler component)]
       (do
         (mark-point :set-state! component)
         (p/queue! r [component])
         (schedule-render! r))
       (do
         (timbre/info :#r "forceUpdate in set-state!1107")

         (.forceUpdate component)))))

(defn get-state
  "Get a component's local state. May provide a single key or a sequential
   collection of keys for indexed access into the component's local state."
  ([component]
   (get-state component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [cst (if #?(:clj  (satisfies? ILocalState component)
                    :cljs (implements? ILocalState component))
                 (-get-state component)
               #?(:clj  @(:state component)
                  :cljs (when-let [state (. component -state)]
                          (or (gobj/get state "omcljs$pendingState")
                              (gobj/get state "omcljs$state")))))]
     (get-in cst (if (sequential? k-or-ks) k-or-ks [k-or-ks])))))

(defn update-state!
  "Update a component's local state. Similar to Clojure(Script)'s swap!"
  ([component f]
   (set-state! component (f (get-state component))))
  ([component f arg0]
   (set-state! component (f (get-state component) arg0)))
  ([component f arg0 arg1]
   (set-state! component (f (get-state component) arg0 arg1)))
  ([component f arg0 arg1 arg2]
   (set-state! component (f (get-state component) arg0 arg1 arg2)))
  ([component f arg0 arg1 arg2 arg3]
   (set-state! component (f (get-state component) arg0 arg1 arg2 arg3)))
  ([component f arg0 arg1 arg2 arg3 & arg-rest]
   (set-state! component
     (apply f (get-state component) arg0 arg1 arg2 arg3 arg-rest))))

(defn get-rendered-state
  "Get the rendered state of component. pagora.aum.om.next/get-state always returns the
   up-to-date state."
  ([component]
   (get-rendered-state component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [cst (if #?(:clj  (satisfies? ILocalState component)
                    :cljs (implements? ILocalState component))
               (-get-rendered-state component)
               #?(:clj  (get-state component)
                  :cljs (some-> component .-state (gobj/get "omcljs$state"))))]
     (get-in cst (if (sequential? k-or-ks) k-or-ks [k-or-ks])))))

#?(:cljs
   (defn- merge-pending-state! [c]
     (if (implements? ILocalState c)
       (-merge-pending-state! c)
       (when-let [pending (some-> c .-state (gobj/get "omcljs$pendingState"))]
         (let [state    (.-state c)
               previous (gobj/get state "omcljs$state")]
           (gobj/remove state "omcljs$pendingState")
           (gobj/set state "omcljs$previousState" previous)
           (gobj/set state "omcljs$state" pending))))))

(defn react-set-state!
  ([component new-state]
   (react-set-state! component new-state nil))
  ([component new-state cb]
   {:pre [(component? component)]}
   #?(:clj  (do
              (set-state! component new-state)
              (cb))
      :cljs (.setState component #js {:omcljs$state new-state} cb))))

(declare full-query to-env schedule-sends! reconciler? ref->components force)

(defn gather-sends
  "Given an environment, a query and a set of remotes return a hash map of remotes
   mapped to the query specific to that remote."
  [{:keys [parser] :as env} q remotes]
  (into {}
        (comp ;;transducer
         (map #(vector % (parser env q %))) ;;gets called first, sets remote key to returned query for earch remote
         (filter (fn [[_ v]] (pos? (count v))))) ;;remove remote key if empty query returned for that remote
        remotes))

(defn transform-reads
  "Given r (a reconciler) and a query expression including a mutation and
   any simple reads, return the equivalent query expression where the simple
   reads have been replaced by the full query for each component that cares about
   the specified read."
  [r tx]
  (if (-> r :config :easy-reads)
    (letfn [(with-target [target q]
              (if-not (nil? target)
                [(force (first q) target)]
                q))
            (add-focused-query [k target tx c]
              (let [transformed (->> (focus-query (get-query c) [k])
                                     (with-target target)
                                     (full-query c))]
                (into tx (remove (set tx)) transformed)))]
      (loop [exprs (seq tx) tx' []]
        (if-not (nil? exprs)
          (let [expr (first exprs)
                ast (parser/expr->ast expr)
                key (:key ast)
                tgt (:target ast)]
            (if (keyword? key)
              (let [cs (ref->components r key)]
                (recur (next exprs)
                       (reduce #(add-focused-query key tgt %1 %2)
                               (cond-> tx'
                                 (empty? cs) (conj expr))
                               cs)))
              (recur (next exprs) (conj tx' expr))))
          tx')))
    tx))

(defn set-query!
  "Change the query of a component. Takes a map containing :params and/or
   :query. :params should be a map of new bindings and :query should be a query
   expression. Will schedule a re-render as well as remote re-sends if
   necessary."
  ([x params&query]
    (set-query! x params&query nil))
  ([x {:keys [params query]} reads]
   {:pre [(or (reconciler? x)
              (component? x))
          (or (not (nil? params))
              (not (nil? query)))
          (or (nil? reads)
              (vector? reads))]}
   (let [r    (if (component? x)
                (get-reconciler x)
                x)
         c    (when (component? x) x)
         xs   (if-not (nil? c) [c] [])
         root (:root @(:state r))
         cfg  (:config r)
         st   (:state cfg)
         id   #?(:clj  (java.util.UUID/randomUUID)
                 :cljs (random-uuid))]
     #?(:cljs (.add (:history cfg) id @st))
     #?(:cljs
        (when-let [l (:logger cfg)]
          (glog/info l
            (cond-> (str (when-let [ident (when (implements? Ident c)
                                            (ident c (props c)))]
                           (str (pr-str ident) " ")))
                    (reconciler? x) (str "reconciler ")
                    query (str "changed query '" query ", ")
                    params (str "changed params " params " ")
                    true (str (pr-str id))))))
     (swap! st update-in [:om.next/queries (or c root)] merge
       (merge (when query {:query query}) (when params {:params params})))
     (when (and (not (nil? c)) (nil? reads))
       (p/queue! r [c]))
     (when-not (nil? reads)
       (p/queue! r reads))
     (p/reindex! r)
     (let [rootq (if (not (nil? c))
                   (full-query c)
                   (when (nil? reads)
                     (get-query root)))
           sends (gather-sends (to-env cfg)
                   (into (or rootq []) (transform-reads r reads)) (:remotes cfg))]
       (when-not (empty? sends)
         (doseq [[remote _] sends]
           (p/queue! r xs remote))
         (p/queue-sends! r sends)
         (schedule-sends! r)))
     nil)))

(defn update-query!
  "Update a component's query and query parameters with a function."
  ([component f]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)})))
  ([component f arg0]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)}
       arg0)))
  ([component f arg0 arg1]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)}
       arg0 arg1)))
  ([component f arg0 arg1 arg2]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)}
       arg0 arg1 arg2)))
  ([component f arg0 arg1 arg2 arg3 & arg-rest]
   (set-query! component
     (apply f {:query  (get-unbound-query component)
               :params (get-params component)}
       arg0 arg1 arg2 arg3 arg-rest))))

(defn mounted?
  "Returns true if the component is mounted."
  #?(:cljs {:tag boolean})
  [x]
  #?(:clj  (and (component? x) @(get-prop x :omcljs$mounted?))
     :cljs (and (component? x) ^boolean (.isMounted x))))

(defn react-ref
  "Returns the component associated with a component's React ref."
  [component name]
  #?(:clj  (some-> @(:refs component) (get name))
     :cljs (some-> (.-refs component) (gobj/get name))))

(defn children
  "Returns the component's children."
  [component]
  #?(:clj  (:children component)
     :cljs (.. component -props -children)))

#?(:cljs
   (defn- update-component! [c next-props]
     {:pre [(component? c)]}
     (update-props! c next-props)
     (mark-point :forceUpdate-start c)
     (let [s  (now-in-ms)
           ret (.forceUpdate c)]
       (mark-point :forceUpdate-end c)
       (let [component-name (.. c -constructor -displayName) ]
         (warn-when s 0 (str "forceUpdate of cmp " component-name)))
       ret)))

#?(:cljs
   (defn should-update?
     ([c next-props]
      (should-update? c next-props nil))
     ([c next-props next-state]
      {:pre [(component? c)]}
      (let [ret (.shouldComponentUpdate c
                                        #js {:omcljs$value next-props}
                                        #js {:omcljs$state next-state})]
        ;; (when ret
        ;;   (timbre/info :#r "shouldUpdate")
        ;;   (js/console.log c)
        ;;   (timbre/info :#pp {:next-props next-props})
        ;;   (timbre/info :#pp {:next-state next-state}))
        ret)
      )))

(defn- raw-class-path
  "Return the raw component class path associated with a component. Contains
   duplicates for recursive component trees."
  [c]
  (loop [c c ret (list (react-type c))]
    (if-let [p (parent c)]
      (if (iquery? p)
        (recur p (cons (react-type p) ret))
        (recur p ret))
      ret)))

(defn class-path
  "Return the component class path associated with a component. This is the path as set in the react hierarchy"
  [c]
  {:pre [(component? c)]}
  (let [raw-cp (raw-class-path c)]
    (loop [cp (seq raw-cp) ret [] seen #{}]
      (if cp
        (let [c (first cp)]
          (if (contains? seen c)
            (recur (next cp) ret seen)
            (recur (next cp) (conj ret c) (conj seen c))))
        (seq ret)))))

(defn- recursive-class-path?
  "Returns true if a component's classpath is recursive"
  #?(:cljs {:tag boolean})
  [c]
  {:pre [(component? c)]}
  (not (apply distinct? (raw-class-path c))))

(defn subquery
  "Given a class or mounted component x and a ref to an instantiated component
   or class that defines a subquery, pick the most specific subquery. If the
   component is mounted subquery-ref will be used, subquery-class otherwise."
  [x subquery-ref subquery-class]
  {:pre [(or (keyword? subquery-ref) (string? subquery-ref))
         (fn? subquery-class)]}
  (let [ref (cond-> subquery-ref (keyword? subquery-ref) str)]
    (if (and (component? x) (mounted? x))
      (get-query (react-ref x ref))
      (get-query subquery-class))))

(defn get-ident
  "Given a mounted component with assigned props, return the ident for the
   component."
  [x]
  {:pre [(component? x)]}
  (let [m (props x)]
    (assert (not (nil? m)) "get-ident invoked on component with nil props")
    (ident x m)))

;; =============================================================================
;; Reconciler API

(declare reconciler?)

(defn- basis-t [reconciler]
  (p/basis-t reconciler))

;;Called if state changes, calls reconcile on reconciler at raf, or like 16ms from now
#?(:cljs
   (defn- queue-render! [f]
     (let [f' (fn []
                (let [s (now-in-ms)
                      ret (f)]
                  (warn-when s 16 "Reconciling in total")
                  ret))]

       (cond
         (fn? *raf*) (*raf* f')

         (not (exists? js/requestAnimationFrame))
         (js/setTimeout f' 16)

         :else
         (js/requestAnimationFrame f'))))
   ;; (defn- queue-render! [f]
   ;;   (cond
   ;;     (fn? *raf*) (*raf* f)

   ;;     (not (exists? js/requestAnimationFrame))
   ;;     (js/setTimeout f 16)

   ;;     :else
   ;;     (js/requestAnimationFrame f)))
   )

#?(:cljs
   (defn schedule-render! [reconciler]
     (when (p/schedule-render! reconciler)
       (swap! js/window.foo conj [:schedule-reconcile!-for-next-raf])
       (queue-render! #(p/reconcile! reconciler)))))

(defn schedule-sends! [reconciler]
  ;;Only calls send! on reconciler if :sends-queued is not set in state of reconciler.
  ;;It sets it before returning true
  (when (p/schedule-sends! reconciler)
    #?(:clj  (p/send! reconciler)
       :cljs (js/setTimeout #(p/send! reconciler) 0))))

(declare remove-root!)

(defn add-root!
  "Given a root component class and a target root DOM node, instantiate and
   render the root class using the reconciler's :state property. The reconciler
   will continue to observe changes to :state and keep the target node in sync.
   Note a reconciler may have only one root. If invoked on a reconciler with an
   existing root, the new root will replace the old one."
  ([reconciler root-class target]
   (add-root! reconciler root-class target nil))
  ([reconciler root-class target options]
   {:pre [(reconciler? reconciler) (fn? root-class)]}
   (when-let [old-reconciler (get @roots target)]
     (remove-root! old-reconciler target))
   (swap! roots assoc target reconciler)
   (p/add-root! reconciler root-class target options)))

(defn remove-root!
  "Remove a root target (a DOM element) from a reconciler. The reconciler will
   no longer attempt to reconcile application state with the specified root."
  [reconciler target]
  (p/remove-root! reconciler target))

;; =============================================================================
;; Transactions

(defprotocol ITxIntercept
  (tx-intercept [c tx]
    "An optional protocol that component may implement to intercept child
     transactions."))

(defn- to-env [x]
  (let [config (if (reconciler? x) (:config x) x)]
    (select-keys config [:state :shared :parser :logger :pathopt])))

;;Ref being an ident
(defn transact* [r c ref tx]
  (mark-point :transact* {:c c :ref ref :tx tx})
  (let [cfg  (:config r)
        ;; _ (timbre/info :#cp cfg)
        ref  (if (and c #?(:clj  (satisfies? Ident c)
                           :cljs (implements? Ident c)) (not ref))
               (ident c (props c))
               ref)
        ;;env in mutation gets component and reconciler but also ref (ident) of
        ;;component if it implements Ident, of if passed in directly to transaction with reconciler
        env  (merge
              (to-env cfg)
              {:reconciler r :component c}
              (when ref
                {:ref ref}))
        id   #?(:clj  (java.util.UUID/randomUUID)
                :cljs (random-uuid))
        #?@(:cljs
            [_    (.add (:history cfg) id @(:state cfg))
             _    (when-let [l (:logger cfg)]
                    ;; (js/console.log r)
                    ;; (js/console.log c)
                    ;; (js/console.log ref)
                    ;; (js/console.log tx)
                    ;; (js/console.log )
                    ;; (js/console.log ref)
                    (let [cmp-name (or (when ref (str (pr-str ref)))
                                       (when c
                                         (gobj/getValueByKeys c #js ["_reactInternalFiber" "type" "displayName"]))
                                       "?")]
                      (js/console.log  (str "%cTransaction by " cmp-name) "color:green")
                      (js/console.log tx)
                      ;; (js/console.log (str "%c" (with-out-str (pprint tx))) "color:green")
                      )
                    ;; (glog/info l
                    ;;   (str (when ref (str (pr-str ref) " "))
                    ;;     "transacted !!'" tx ", " (pr-str id)))
                    )])
        old-state @(:state cfg)
        v    ((:parser cfg) env tx) ;; result for nil target (denormalized tree of data)
        _ (mark-point :after-parse-in-transact {:v (map #(remove symbol?) (keys v))
                                                :v-keys (keys v)
                                                :v2 (into [] (remove symbol?) (keys v))})
        ;;with mutation-keys and their result maps
        remotes (or (:remotes (meta tx) (:remotes cfg)))
        snds (gather-sends env tx remotes) ;;result map (remote->query) for all remotes
        ;; _ (timbre/info :#r "sends" snds)
        ;;xs will be a vector like [c ref]
        ;;
        ;;c is not added if it's nil
        ;;ref is not added if it's nil
        xs   (cond-> []
               (not (nil? c)) (conj c)
               (not (nil? ref)) (conj ref))]
    (doseq [[k {:keys [om.next/error]}] v]
      (when error
        ;;TODO hook in bugsnag here!!
        (timbre/info :#r "Transaction " k " has thrown an exception")
        #?(:cljs (js/console.error error)
           :clj (timbre/error error))))
    (mark-point :xs xs :sends snds :remotes remotes)

    (timbre/info :#r "queueing:" (into xs (remove symbol?) (keys v)))
    (mark-point :queueing-in-transact*)
    (p/queue! r (into xs (remove symbol?) (keys v))) ;;[c ref :exp1  :k2] k1, k2 etc is any key added to tx
    ;; get added to :queue of reconciler
    ;;these keys could have been expressions, or been transformed by
    ;;transform-reads to more complicated expressions, but after parsing they've
    ;;been reduced to simple keys again??. With as value whatever the parser made
    ;;of them. Or rather your read and mutate methods did with them. They could be an ident still.

    ;;This kicks it all off:
    (when-not (empty? snds)
      (doseq [[remote _] snds]
        (p/queue! r xs remote)) ;;  also queue [c ref] for every remote, goes added to {:remote-queue :{remote [..]}} of reconciler

      (p/queue-sends! r snds) ;;and queue map of remote->query, which is basically used in send! method of reconciler
      (schedule-sends! r)) ;; calls p/send! on reconciler
    ;;which basically directly calls the send function supplied to the reconciler

    (when-let [f (:tx-listen cfg)]
      (let [tx-data (merge env
                           {:old-state old-state
                            :new-state @(:state cfg)})]
        (f tx-data {:tx tx
                    :ret v
                    :sends snds})))
    v))

(defn annotate-mutations
  "Given a query expression annotate all mutations by adding a :mutator -> ident
   entry to the metadata of each mutation expression in the query."
  [tx ident]
  (letfn [(annotate [expr ident]
            (cond-> expr
              (util/mutation? expr) (vary-meta assoc :mutator ident)))]
    (into [] (map #(annotate % ident)) tx)))

(defn some-iquery? [c]
  (loop [c c]
    (cond
      (nil? c) false
      (iquery? c) true
      :else (recur (parent c)))))

(defn transact!
  "Given a reconciler or component run a transaction. tx is a parse expression
   that should include mutations followed by any necessary read. The reads will
   be used to trigger component re-rendering.

  NOTE: call it with a reconciler iso component and transform-reads won't be called on tx.
  NOTE: call it from component that doesn't implement IQuery and transform-reads won't be called on tx

  NOTE: call it from component that does have IQuery implemented,
  transform-reads is called on tx and you can also intercept
  transaction from parent component. Like:

  ITxIntercept
   (tx-intercept [this tx] <return some tx>)

  tx-intercept of parents higher in the hierarchy can override once again.

   Example:

     (om/transact! widget
       '[(do/this!) (do/that!)
         :read/this :read/that])"
  ([x tx]
   {:pre [(or (component? x)
              (reconciler? x))
          (vector? tx)]}
   (mark-point :transact tx)
   (let [meta-tx (meta tx)
         tx (cond-> tx
              (and (component? x) (satisfies? Ident x))
              (annotate-mutations (get-ident x)))
         tx (with-meta tx meta-tx)]
     (mark-point :transact2 {:tx tx :meta-tx meta-tx})
     (cond
       (reconciler? x) (transact* x nil nil tx)
       (not (iquery? x)) (do ;;does not implement IQuery
                           (invariant (some-iquery? x)
                             (str "transact! should be called on a component"
                                  "that implements IQuery or has a parent that"
                                  "implements IQuery"))
                           (transact* (get-reconciler x) nil nil tx))
       :else (do
               (loop [p (parent x) x x tx tx]
                 (if (nil? p)
                   (let [r (get-reconciler x)
                         tx (transform-reads r tx)
                         tx (with-meta tx meta-tx)]
                     (transact* r x nil tx))
                   (let [[x' tx] (if #?(:clj  (satisfies? ITxIntercept p)
                                        :cljs (implements? ITxIntercept p))
                                   [p (tx-intercept p tx)]
                                   [x tx])]
                     (recur (parent p) x' tx))))))))
  ;;Apparently you can call transact! also with an extra arg, ref.
  ([r ref tx]
   (transact* r nil ref tx)))

;; =============================================================================
;; Parser

(defn parser
  "Create a parser. The argument is a map of two keys, :read and :mutate. Both
   functions should have the signature (Env -> Key -> Params -> ParseResult)."
  [{:keys [read mutate] :as opts}]
  {:pre [(map? opts)]}
  (parser/parser opts))

(defn dispatch
  "Helper function for implementing :read and :mutate as multimethods. Use this
   as the dispatch-fn."
  [_ key _] key)

(defn query->ast
  "Given a query expression convert it into an AST."
  [query-expr]
  (parser/query->ast query-expr))

(defn ast->query [query-ast]
  "Given an AST convert it back into a query expression."
  (parser/ast->expr query-ast true))

(defn- get-dispatch-key [prop]
  (cond-> prop
    (or (not (util/ident? prop))
        (= (second prop) '_))
    ((comp :dispatch-key parser/expr->ast))))

;; =============================================================================
;; Indexer

(defn- cascade-query
  "Cascades a query up the classpath. class-path->query is a map of classpaths
   to their queries. classpath is the classpath where we start cascading (typically
   the direct parent's classpath of the component changing query). data-path is
   the data path in the classpath's query to the new query. new-query is the
   query to be applied to the classpaths. union-keys are any keys into union
   queries found during index building; they are used to access union queries
   when cascading the classpath, and to generate the classpaths' rendered-paths,
   which skip these keys."
  [class-path->query classpath data-path new-query union-keys]
  (loop [cp classpath
         data-path data-path
         new-query new-query
         ret {}]
    (if-not (empty? cp)
      (let [rendered-data-path (into [] (remove (set union-keys)) data-path)
            filter-data-path (cond-> rendered-data-path
                               (not (empty? rendered-data-path)) pop)
            qs (filter #(= filter-data-path
                           (-> % zip/root (focus->path filter-data-path)))
                 (get class-path->query cp))
            qs' (into #{}
                  (map (fn [q]
                         (let [new-query (if (or (map? (zip/node q))
                                               (some #{(peek data-path)} union-keys))
                                           (let [union-key (peek data-path)]
                                             (-> (query-template (zip/root q)
                                                   rendered-data-path)
                                                 zip/node
                                                 (assoc union-key new-query)))
                                           new-query)]
                           (-> (zip/root q)
                             (query-template rendered-data-path)
                             (replace new-query)
                             (focus-query filter-data-path)
                             (query-template filter-data-path)))))
                  qs)]
        (recur (pop cp) (pop data-path)
          (-> qs' first zip/node) (assoc ret cp qs')))
      ret)))

(defrecord Indexer [indexes extfs]
  #?(:clj  clojure.lang.IDeref
     :cljs IDeref)
  #?(:clj  (deref [_] @indexes)
     :cljs (-deref [_] @indexes))

  p/IIndexer
  (index-root [_ x]
    (let [prop->classes     (atom {})
          class-path->query (atom {})
          rootq             (get-query x)
          root-class        (cond-> x (component? x) react-type)]
      (letfn [(build-index* [class query path classpath union-expr union-keys]
                (invariant (or (not (iquery? class))
                             (and (iquery? class)
                               (not (empty? query))))
                  (str "`IQuery` implementation must return a non-empty query."
                    " Check the `IQuery` implementation of component `"
                    (if (component? class)
                      (.. class -constructor -displayName)
                      (.. class -prototype -constructor -displayName)) "`."))
                (let [recursive? (some #{class} classpath)
                      classpath  (cond-> classpath
                                   (and (not (nil? class))
                                        (not recursive?))
                                   (conj class))
                      dp->cs     (get-in @indexes [:data-path->components])]
                  (when class
                    ;; path could have changed when setting queries, so we only use
                    ;; rootq on the first call (when there's no class-path->query
                    ;; information) - António
                    (let [root-query (if (empty? path)
                                       rootq
                                       (-> @class-path->query
                                         (get [root-class]) first zip/root))]
                      (swap! class-path->query update-in [classpath] (fnil conj #{})
                        (query-template (focus-query root-query path) path))))
                  (let [recursive-join? (and recursive?
                                          (some (fn [e]
                                                  (and (util/join? e)
                                                    (not (util/recursion?
                                                           (util/join-value e)))))
                                            query)
                                          (= (distinct path) path))
                        recursive-union? (and recursive?
                                           union-expr
                                           (= (distinct path) path))]
                    (when (or (not recursive?)
                            recursive-join?
                            recursive-union?)
                      (cond
                        (vector? query)
                        (let [{props false joins true} (group-by util/join? query)]
                          (swap! prop->classes
                            #(merge-with into %
                               (zipmap
                                 (map get-dispatch-key props)
                                 (repeat #{class}))))
                          (doseq [join joins]
                            (let [[prop query']  (util/join-entry join)
                                  prop-dispatch-key (get-dispatch-key prop)
                                  recursion?     (util/recursion? query')
                                  union-recursion? (and recursion? union-expr)
                                  query'         (if recursion?
                                                   (if-not (nil? union-expr)
                                                     union-expr
                                                     query)
                                                   query')
                                  path'          (conj path prop)
                                  rendered-path' (into []
                                                       (remove
                                                         (set union-keys)
                                                         (conj path
                                                               prop-dispatch-key)))
                                  cs (get dp->cs rendered-path')
                                  cascade-query? (and (= (count cs) 1)
                                                   (= (-> query' meta :component)
                                                     (react-type (first cs)))
                                                   (not (map? query')))
                                  query''        (if cascade-query?
                                                   (get-query (first cs))
                                                   query')]
                              (swap! prop->classes
                                #(merge-with into % {prop-dispatch-key #{class}}))
                              (when (and cascade-query? (not= query' query''))
                                (let [cp->q' (cascade-query @class-path->query classpath
                                               path' query'' union-keys)]
                                  (swap! class-path->query merge cp->q')))
                              (let [class' (-> query'' meta :component)]
                                (when-not (and recursion? (nil? class'))
                                  (build-index* class' query''
                                    path' classpath (if recursion? union-expr nil) union-keys))))))

                        ;; Union query case
                        (map? query)
                        (doseq [[prop query'] query]
                          (let [path'          (conj path prop)
                                class'         (-> query' meta :component)
                                cs             (filter #(= class' (react-type %))
                                                 (get dp->cs path))
                                cascade-query? (and class' (= (count cs) 1))
                                query''        (if cascade-query?
                                                 (get-query (first cs))
                                                 query')]
                            (when (and cascade-query? (not= query' query''))
                              (let [qs        (get @class-path->query classpath)
                                    q         (first qs)
                                    qnode     (zip/node
                                                (cond-> q
                                                  (nil? class) (query-template path)))
                                    new-query (assoc qnode
                                                prop query'')
                                    q'        (cond-> (zip/replace
                                                        (query-template (zip/root q) path)
                                                        new-query)
                                                (nil? class)
                                                (-> zip/root
                                                  (focus-query (pop path))
                                                  (query-template (pop path))))
                                    qs'       (into #{q'} (remove #{q}) qs)
                                    cp->q'    (merge {classpath qs'}
                                                (cascade-query @class-path->query
                                                  (pop classpath) path
                                                  (zip/node q') union-keys))]
                                (swap! class-path->query merge cp->q')))
                            (build-index* class' query'' path' classpath query (conj union-keys prop)))))))))]
        (build-index* root-class rootq [] [] nil [])
        (swap! indexes merge
          {:prop->classes     @prop->classes
           :class-path->query @class-path->query}))))

  ;;p/IIndexer
  (index-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in ((:index-component extfs) indexes c)
                        [:class->components (react-type c)]
                        (fnil conj #{}) c)
              data-path (into [] (remove number?) (path c))
              indexes (update-in ((:index-component extfs) indexes c)
                        [:data-path->components data-path]
                        (fnil conj #{}) c)
              ident     (when #?(:clj  (satisfies? Ident c)
                                 :cljs (implements? Ident c))
                          (let [ident (ident c (props c))]
                            (invariant (util/ident? ident)
                              (str "malformed Ident. An ident must be a vector of "
                                "two elements (a keyword and an EDN value). Check "
                                "the Ident implementation of component `"
                                (.. c -constructor -displayName) "`."))
                            (invariant (some? (second ident))
                              (str "component " (.. c -constructor -displayName)
                                "'s ident (" ident ") has a `nil` second element."
                                " This warning can be safely ignored if that is intended."))
                            ident))]
          (if-not (nil? ident)
            (cond-> indexes
              ident (update-in [:ref->components ident] (fnil conj #{}) c))
            indexes)))))

  ;;p/IIndexer
  (drop-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in ((:drop-component extfs) indexes c)
                        [:class->components (react-type c)]
                        disj c)
              data-path (into [] (remove number?) (path c))
              indexes (update-in ((:drop-component extfs) indexes c)
                        [:data-path->components data-path]
                        disj c)
              ident     (when #?(:clj  (satisfies? Ident c)
                                 :cljs (implements? Ident c))
                        (ident c (props c)))]
          (if-not (nil? ident)
            (cond-> indexes
              ident (update-in [:ref->components ident] disj c))
            indexes)))))

  ;;p/IIndexer
  (key->components [_ k]
    (let [indexes @indexes]
      (if (component? k)
        #{k}
        (if-let [cs ((:ref->components extfs) indexes k)]
          cs
          (transduce (map #(get-in indexes [:class->components %]))
            (completing into)
            (get-in indexes [:ref->components k] #{})
            (get-in indexes [:prop->classes k])))))))

(defn indexer
  "Given a function (Component -> Ref), return an indexer."
  ([]
    (indexer
      {:index-component (fn [indexes component] indexes)
       :drop-component  (fn [indexes component] indexes)
       :ref->components (fn [indexes ref] nil)}))
  ([extfs]
   (Indexer.
     (atom
       {:class->components {}
        :data-path->components {}
        :ref->components   {}})
     extfs)))

(defn get-indexer
  "PRIVATE: Get the indexer associated with the reconciler."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (-> reconciler :config :indexer))

(defn ref->components
  "Return all components for a given ref."
  [x ref]
  (when-not (nil? ref)
    (let [indexer (if (reconciler? x) (get-indexer x) x)]
      (p/key->components indexer ref))))

(defn ref->any
  "Get any component from the indexer that matches the ref."
  [x ref]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (first (p/key->components indexer ref))))

(defn class->any
  "Get any component from the indexer that matches the component class."
  [x class]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (first (get-in @indexer [:class->components class]))))

(defn class-path->queries
  "Given x (a reconciler or indexer) and y (a component or component class
   path), return the queries for that path."
  [x y]
  (let [indexer (if (reconciler? x) (get-indexer x) x)
        cp      (if (component? y) (class-path y) y)]
    (into #{} (map zip/root)
      (get-in @indexer [:class-path->query cp]))))

(defn full-query
  "Returns the absolute query for a given component, not relative like
   pagora.aum.om.next/get-query."
  ([component]
   (when (iquery? component)
     (if (nil? (path component))
       (replace
        (first
         (get-in @(-> component get-reconciler get-indexer)
                 [:class-path->query (class-path component)]))
        (get-query component))
       (full-query component (get-query component)))))
  ([component query]
   (when (iquery? component)
     (let [xf    (cond->> (remove number?)
                   (recursive-class-path? component) (comp (distinct)))
           path' (into [] xf (path component)) ;;this is the om-path as attached as meta data to the map by the parser
           cp    (class-path component) ;;this is the path of ctr fns into the react component hierarchy
           ;;Get the queries as indexed by class-path, these already have zip info attached.
           qs    (get-in @(-> component get-reconciler get-indexer)
                         [:class-path->query cp])]
       ;; (timbre/info :#g cp)
       ;; (timbre/info :#g path')
       ;; (timbre/info (path component))
       ;; ;; (timbre/info :#pp (first qs))
       ;; (timbre/info (zip/root (first qs)))
       ;; (timbre/info (focus->path (zip/root (first qs)) [:route/checklist-templates :root-qbucket-list :question]))
       ;; (timbre/info (focus->path (zip/root (first qs)) [:route/checklist-templates :root-qbucket-list :question]))
       ;; (timbre/info :#pp (focus->path path'))
       ;; (timbre/info path')
       
       (if-not (empty? qs)
         ;; handle case where child appears multiple times at same class-path
         ;; but with different queries
         (let [q (->> qs
                      (filter #(= path'
                                  (mapv get-dispatch-key
                                        (-> % zip/root (focus->path path')))))
                      first)]

           ;; (defn replace [template new-query]
           ;;   (-> template (zip/replace new-query) zip/root))
           (if-not (nil? q)
             (replace q query)
             (throw
              (ex-info (str "No queries exist at the intersection of component path " cp " and data path " path')
                       {:type :om.next/no-queries}))))
         (throw
          (ex-info (str "No queries exist for component path " cp)
                   {:type :om.next/no-queries})))))))

(defn- normalize* [query data refs union-seen]
  (cond
    (= '[*] query) data

    ;; union case
    (map? query)
    (let [class (-> query meta :component)
          ident   #?(:clj  (when-let [ident (-> class meta :ident)]
                             (ident class data))
                     :cljs (when (implements? Ident class)
                             (ident class data)))]
      (if-not (nil? ident)
        (vary-meta (normalize* (get query (first ident)) data refs union-seen)
          assoc :om/tag (first ident))
        (throw #?(:clj  (IllegalArgumentException. "Union components must implement Ident")
                  :cljs (js/Error. "Union components must implement Ident")))))

    (vector? data) data ;; already normalized

    :else
    (loop [q (seq query) ret data]
      (if-not (nil? q)
        (let [expr (first q)]
          (if (util/join? expr)
            (let [[k sel] (util/join-entry expr)
                  recursive? (util/recursion? sel)
                  union-entry (if (util/union? expr) sel union-seen)
                  sel     (if recursive?
                            (if-not (nil? union-seen)
                              union-seen
                              query)
                            sel)
                  class   (-> sel meta :component)
                  v       (get data k)]
              (cond
                ;; graph loop: db->tree leaves ident in place
                (and recursive? (util/ident? v)) (recur (next q) ret)
                ;; normalize one
                (map? v)
                (let [x (normalize* sel v refs union-entry)]
                  (if-not (or (nil? class) (not #?(:clj  (-> class meta :ident)
                                                   :cljs (implements? Ident class))))
                    (let [i #?(:clj  ((-> class meta :ident) class v)
                               :cljs (ident class v))]
                      (swap! refs update-in [(first i) (second i)] merge x)
                      (recur (next q) (assoc ret k i)))
                    (recur (next q) (assoc ret k x))))

                ;; normalize many
                (vector? v)
                (let [xs (into [] (map #(normalize* sel % refs union-entry)) v)]
                  (if-not (or (nil? class) (not #?(:clj  (-> class meta :ident)
                                                   :cljs (implements? Ident class))))
                    (let [is (into [] (map #?(:clj  #((-> class meta :ident) class %)
                                              :cljs #(ident class %))) xs)]
                      (if (vector? sel)
                        (when-not (empty? is)
                          (swap! refs
                            (fn [refs]
                              (reduce (fn [m [i x]]
                                        (update-in m i merge x))
                                refs (zipmap is xs)))))
                        ;; union case
                        (swap! refs
                          (fn [refs']
                            (reduce
                              (fn [ret [i x]]
                                (update-in ret i merge x))
                              refs' (map vector is xs)))))
                      (recur (next q) (assoc ret k is)))
                    (recur (next q) (assoc ret k xs))))

                ;; missing key
                (nil? v)
                (recur (next q) ret)

                ;; can't handle
                :else (recur (next q) (assoc ret k v))))
            (let [k (if (seq? expr) (first expr) expr)
                  v (get data k)]
              (if (nil? v)
                (recur (next q) ret)
                (recur (next q) (assoc ret k v))))))
        ret))))

(defn tree->db
  "Given a Om component class or instance and a tree of data, use the component's
   query to transform the tree into the default database format. All nodes that
   can be mapped via Ident implementations wil be replaced with ident links. The
   original node data will be moved into tables indexed by ident. If merge-idents
   option is true, will return these tables in the result instead of as metadata."
  ([x data]
    (tree->db x data false))
  ([x data #?(:clj merge-idents :cljs ^boolean merge-idents) ]
   (let [refs (atom {})
         x    (if (vector? x) x (get-query x))
         ret  (normalize* x data refs nil)]
     (if merge-idents
       (let [refs' @refs]
         (assoc (merge ret refs')
           ::tables (into #{} (keys refs'))))
       (with-meta ret @refs)))))

;; =============================================================================
;; Reconciler

(defn default-merge-ident
  [_ tree ref props]
  (cond-> tree
    (map? props)
    (update-in ref merge props)))

(defn- merge-idents [tree config refs query]
  (let [{:keys [merge-ident indexer]} config
        ident-joins (into {} (comp
                               (map #(cond-> % (seq? %) first))
                               (filter #(and (util/join? %)
                                             (util/ident? (util/join-key %)))))
                          query)]
    (letfn [ (step [tree' [ident props]]
              (if (:normalize config)
                (let [c-or-q (or (get ident-joins ident) (ref->any indexer ident))
                      props' (tree->db c-or-q props)
                      refs   (meta props')]
                  ((:merge-tree config)
                    (merge-ident config tree' ident props') refs))
                (merge-ident config tree' ident props)))]
      (reduce step tree refs))))



;;========================================

(defn- sift-idents [res]
  (let [{idents true rest false} (group-by #(vector? (first %)) res)]
    [(into {} idents) (into {} rest)]))

(defn- merge-novelty!
  [reconciler state res query]
  (let [config      (:config reconciler)
        [idts res'] (sift-idents res)
        res'        (if (:normalize config)
                      (tree->db
                        (or query (:root @(:state reconciler)))
                        res' true)
                      res')]
    (-> state
      (merge-idents config idts query)
      ((:merge-tree config) res'))))

;;Res is delta, so whatever needs to merged with app-state
(defn default-merge [reconciler state res query]
  {:keys    (into [] (remove symbol?) (keys res))
   :next    (merge-novelty! reconciler state res query)
   :tempids (->> (filter (comp symbol? first) res)
              (map (comp :tempids second))
              (reduce merge {}))})

;;Called by send cb function (in all its forms). Can also be called by app at
;;any time
(defn merge!
  "Merge a state delta into the application state. Affected components managed
   by the reconciler will re-render."
  ([reconciler delta]
    (merge! reconciler delta nil))
  ([reconciler delta query]
   (merge! reconciler delta query nil))
  ([reconciler delta query remote]
   (let [config (:config reconciler)
         state (:state config)
         merge* (:merge config) ;;default-merge
         {:keys [keys next tempids]} (merge* reconciler @state delta query)]
     (when (nil? remote)
       (mark-point :queue!-in-merge! keys)
       (p/queue! reconciler keys))
     (reset! state
       (if-let [migrate (:migrate config)]
         (merge (select-keys next [:om.next/queries])
           (migrate next
             (or query (get-query (:root @(:state reconciler))))
             tempids (:id-key config)))
         next)))))

;;========================================

(defrecord Reconciler [config state]
  #?(:clj  clojure.lang.IDeref
     :cljs IDeref)
  #?(:clj  (deref [this] @(:state config))
     :cljs (-deref [_] @(:state config)))

  p/IReconciler
  ;;Returns t of reconciler. Incremented each time app state is changed
  (basis-t [_] (:t @state))


  ;; p/IReconciler
  (add-root! [this root-class target options]
    (let [ret   (atom nil)
          rctor (factory root-class) ;;root constructor
          guid  #?(:clj  (java.util.UUID/randomUUID)
                   :cljs (random-uuid))]
      ;; Populate index
      (when (iquery? root-class)
        (p/index-root (:indexer config) root-class))
      ;; Normalize app-state if needed
      (when (and (:normalize config)
                 (not (:normalized @state)))
        (let [new-state (tree->db root-class @(:state config))
              refs      (meta new-state)]
          (reset! (:state config) (merge new-state refs))
          (swap! state assoc :normalized true)))
      ;; Actual react rendering by passing hydrated data (react props) to root
      ;; factory, and then calling reactDom.render on it and target
      (let [renderf (fn [data]
                      (binding [*reconciler* this
                                *shared*     (merge
                                              (:shared config)
                                              (when (:shared-fn config)
                                                ((:shared-fn config) data)))
                                *instrument* (:instrument config)]
                        (let [c (cond
                                  #?@(:cljs [(not (nil? target))
                                             ((:root-render config) (rctor data) target)])
                                  (nil? @ret) (rctor data)
                                  :else (when-let [c' @ret]
                                          #?(:clj (do
                                                    (reset! ret nil)
                                                    (rctor data))
                                             :cljs (when (mounted? c')
                                                     (timbre/info :#r "forceupdate c' data 2221")

                                                     (.forceUpdate c' data)))))]
                          (when (and (nil? @ret) (not (nil? c)))
                            (swap! state assoc :root c)
                            (reset! ret c)))))
            ;;Gets query for root class, calls parser on it, this returns
            ;;hydrated data tree which gets passed to renderf
            parsef  (fn []
                      (let [sel (get-query (or @ret root-class))]
                        (assert (or (nil? sel) (vector? sel))
                                "Application root query must be a vector")
                        (if-not (nil? sel)
                          (let [env (to-env config)
                                v   ((:parser config) env sel)]
                            (when-not (empty? v)
                              (renderf v)))
                          (renderf @(:state config)))))]
        ;;Update reconciler state
        (swap! state merge
               {:target target :render parsef :root root-class
                :remove (fn []
                          (remove-watch (:state config) (or target guid))
                          (swap! state
                                 #(-> %
                                      (dissoc :target) (dissoc :render) (dissoc :root)
                                      (dissoc :remove)))
                          (when-not (nil? target)
                            ((:root-unmount config) target)))})
        ;;Whenever app-state changes, bump basis-t, and call schedule-render!,
        ;;which calls queue-render!, which calls reconcile on reconciler ater
        ;;16ms
        (add-watch (:state config) (or target guid)
                   (fn [_ _ _ _]
                     (swap! state update-in [:t] inc)
                     #?(:cljs
                        (if-not (iquery? root-class)
                          (queue-render! parsef)
                          ;;Also calls queue-render! if :queued flag of reconciler isn't set
                          (do
                            ;; (timbre/info :#y "Scheduling render!")
                            (schedule-render! this))))))
        ;;Do initial render
        (parsef)
        ;;Do any pending remote calls
        (when-let [sel (get-query (or (and target @ret) root-class))]
          (let [env  (to-env config)
                snds (gather-sends env sel (:remotes config))]
            (when-not (empty? snds)
              (when-let [send (:send config)]
                (send snds ;;map of remote expressions keyed by remote target
                      (fn send-cb
                        ([resp]
                         ;;Merge new data
                         (merge! this resp nil)
                         ;;Render with hydrated data
                         (renderf ((:parser config) env sel)))
                        ([resp query]
                         (merge! this resp query)
                         (renderf ((:parser config) env sel)))
                        ([resp query remote]
                         (when-not (nil? remote)
                           (p/queue! this (keys resp) remote))
                         (merge! this resp query remote)
                         (p/reconcile! this remote))))))))
        @ret)))

  ;; p/IReconciler
  (remove-root! [_ target]
    (when-let [remove (:remove @state)]
      (remove)))

  ;; p/IReconciler
  ;;Only used by set-query
  (reindex! [this]
    (let [root (get @state :root)]
      (when (iquery? root)
        (let [indexer (:indexer config)
              c (first (get-in @indexer [:class->components root]))]
          (p/index-root indexer (or c root))))))

  ;; p/IReconciler
  (queue! [this ks] ;;ks can also include component and ref
    (mark-point :queue!-in-preconciler)
    (p/queue! this ks nil))
  ;; p/IReconciler
  (queue! [_ ks remote]
    (if-not (nil? remote)
      (swap! state update-in [:remote-queue remote] into ks)
      (do
        (mark-point :queueing-keys ks (:queue @state))
        (swap! state update-in [:queue] into ks))))

  ;; p/IReconciler
  (queue-sends! [_ sends] ;; sends is map of remote to query
    (swap! state update-in [:queued-sends]
           (:merge-sends config) sends))

  ;; p/IReconciler
  ;;If queued flag is not set it is set, and true is returned, otherwise false.
  (schedule-render! [_]
    (if-not (:queued @state)
      (do
        (swap! state assoc :queued true)
        true)
      false))

  ;; p/IReconciler
  ;;If sends-queued flag is not set it is set, and true is returned, otherwise
  ;;false.
  (schedule-sends! [_]
    (if-not (:sends-queued @state)
      (do
        (swap! state assoc :sends-queued true)
        true)
      false))

  ;; p/IReconciler
  (reconcile! [this]
    (p/reconcile! this nil))
  ;; TODO: need to reindex roots after reconcilation

  ;;The big magic: whenever app state changes a render is scheduled, which
  ;;happens at the next animation frame, which calls this method:
  (reconcile! [this remote] ;;process the the :queue the reconciler's state
    (let [st @state
          q (if-not (nil? remote)
              (get-in st [:remote-queue remote])
              (:queue st))]
      (mark-point :reconcile! {:q q})
      (swap! state update-in [:queued] not)
      (if (not (nil? remote))
        (swap! state assoc-in [:remote-queue remote] [])
        (swap! state assoc :queue []))
      (if (empty? q)
        ;; TODO: need to move root re-render logic outside of batching logic
        ((:render st))
        (let [cs (transduce
                  (map #(p/key->components (:indexer config) %))
                  #(into %1 %2)
                  #{} q)
              {:keys [ui->props]} config
              env (to-env config)
              root (:root @state)]

          (mark-point :reconcile-doseq {;; :env env
                                        :q q
                                        :my-cs (map #(p/key->components (:indexer config) %) q)
                                        :cs cs})
          #?(:cljs
             (doseq [c ((:optimize config) cs)]
               (let [props-change? (> (p/basis-t this) (t c))]
                 (when (mounted? c)
                   (mark-point :processing-component c)
                   (let [computed   (get-computed (props c))
                         next-raw-props (ui->props env c)
                         next-props     (pagora.aum.om.next/computed next-raw-props computed)]

                     ;;Call react willReceiveProps hook if props have changed
                     (when (and
                            (some? (.-componentWillReceiveProps c))
                            (iquery? root)
                            props-change?)
                       (let [next-props (if (nil? next-props)
                                          (when-let [props (props c)]
                                            props)
                                          next-props)]

                         (mark-point :processing-component-will-receive-props c)
                         ;; `componentWilReceiveProps` is always called before `shouldComponentUpdate`
                         (.componentWillReceiveProps c
                                                     #js {:omcljs$value (om-props next-props (p/basis-t this))})))

                     ;; (js/console.log c)
                     ;; (timbre/info "should update?" (should-update? c next-props (get-state c))
                     ;;              "|equal props? " (= next-props (props c))
                     ;;              "|equal state? " (= (goog.object/get (. c -state) "omcljs$state")
                     ;;                                  (get-state c)))
                     ;; (timbre/info :#pp next-props)
                     ;; (timbre/info (goog.object/get (. c -state) "omcljs$state"))
                     ;; (timbre/info :#pp (get-state c))

                     ;;Updating a component:
                     (when (should-update? c next-props (get-state c))
                       (if-not (nil? next-props)
                         (do
                           (mark-point :processing-cmp-calling-update-component)
                           (update-component! c next-props))
                         (do
                           (mark-point :foreUpdate {:c c :next-props next-props})
                           (.forceUpdate c))) ;;so if next props is nil, computed doesn't get passed in either!!

                       ;; Only applies if we're doing incremental rendering, not
                       ;; the case in applications without queries
                       (when (and (iquery? root)
                                  (not= c root)
                                  props-change?)
                         (when-let [update-path (path c)]
                           (loop [p (parent c)]
                             (when (some? p)
                               (let [update-path' (subvec update-path (count (path p)))]
                                 (update-props! p (assoc-in (props p) update-path' next-raw-props))
                                 (merge-pending-props! p)
                                 (recur (parent p)))))))))))))))))

  ;;IReconciler
  (send! [this]
    ;;Just forwards queued-sends as returned from parser for all remotes to send
    ;;fn supplied to reconciler. Then calls merge! on value passed to cb
    (let [sends (:queued-sends @state)]
      (when-not (empty? sends)
        (swap! state
               (fn [state]
                 (-> state
                     (assoc :queued-sends {})
                     (assoc :sends-queued false))))
        ((:send config) sends
         (fn send-cb
           ([resp]
            (merge! this resp nil))
           ([resp query]
            (merge! this resp query))
           ([resp query remote]
            (when-not (nil? remote)
              (p/queue! this (keys resp) remote))
            (merge! this resp query remote)
            (p/reconcile! this remote))))))))

(defn default-ui->props
  [{:keys [parser #?(:clj pathopt :cljs ^boolean pathopt)] :as env} c]

  (mark-point :ui->props-start)
  (let [ui (when (and pathopt #?(:clj  (satisfies? Ident c)
                                 :cljs (implements? Ident c)) (iquery? c))
             (let [id (ident c (props c))
                   ;; _ (timbre/info "ui->props query" [{id (get-query c)}])
                   ui (parser env [{id (get-query c)}])]
               ;; (timbre/info "ui->props ui")
               ;; (timbre/info :#cp ui)
               (get ui id)))]

    (mark-point :ui->props-start-a {:pathopt pathopt :ui ui})
    (let [result (if-not (nil? ui)
                   ui
                   (let [fq (full-query c)]

                     (mark-point :ui->props-full-query ;; {:fq fq}
                                 )
                     (when-not (nil? fq)
                       (let [s  #?(:clj  (System/currentTimeMillis)
                                   :cljs (system-time))
                             ui (parser env fq)
                             e  #?(:clj  (System/currentTimeMillis)
                                   :cljs (system-time))]
                         #?(:cljs
                            (when-let [l (:logger env)]
                              (let [dt (- e s)
                                    component-name (.. c -constructor -displayName)]
                                (when (< 16 dt)
                                  ;; (timbre/warn (str component-name " query took " dt " msecs"))
                                  ;; (glog/warning l (str component-name " query took " dt " msecs"))
                                  ))))
                         (get-in ui (path c))))))]

      (mark-point :ui->props-end )
      result)))

(defn default-merge-tree
  [a b]
  (if (map? a)
    (merge a b)
    b))

(defn default-migrate
  "Given app-state-pure (the application state as an immutable value), a query,
   tempids (a hash map from tempid to stable id), and an optional id-key
   keyword, return a new application state value with the tempids replaced by
   the stable ids."
  ([app-state-pure query tempids]
    (default-migrate app-state-pure query tempids nil))
  ([app-state-pure query tempids id-key]
   (letfn [(dissoc-in [pure [table id]]
             (assoc pure table (dissoc (get pure table) id)))
           (step [pure [old [_ id :as new]]]
             (-> pure
               (dissoc-in old)
               (assoc-in new
                 (cond-> (merge (get-in pure old) (get-in pure new))
                   (not (nil? id-key)) (assoc id-key id)))))]
     (if-not (empty? tempids)
       (let [pure' (reduce step app-state-pure tempids)]
         (tree->db query
           (db->tree query pure' pure'
             (fn [ident] (get tempids ident ident))) true))
       app-state-pure))))

(defn- has-error?
  #?(:cljs {:tag boolean})
  [x]
  (and (map? x) (contains? x ::error)))

(defn default-extract-errors [reconciler res query]
  (letfn [(extract* [query res errs]
            (let [class      (-> query meta :component)
                  top-error? (when (and (not (nil? class)) (has-error? res))
                               (swap! errs
                                 #(update-in % [#?(:clj  ((-> class meta :ident) class res)
                                                   :cljs (ident class res))]
                                   (fnil conj #{}) (::error res))))
                  ret        (when (nil? top-error?) {})]
              (cond
                ;; query root
                (vector? query)
                (if (vector? res)
                  (into [] (map #(extract* query % errs)) res)
                  (loop [exprs (seq query) ret ret]
                    (if-not (nil? exprs)
                      (let [expr (first exprs)
                            k    (as-> (util/expr->key expr) k
                                   (cond-> k
                                     (util/unique-ident? k) first))
                            data (get res k)]
                        (cond
                          (util/mutation? expr)
                          (let [mk   (util/mutation-key expr)
                                ret' (get res mk)]
                            (if (has-error? ret')
                              (let [x (-> expr meta :mutator)]
                                (swap! errs
                                  #(update-in % [x]
                                    (fnil conj #{}) (::error ret')))
                                (recur (next exprs) ret))
                              (recur (next exprs)
                                (when-not (nil? ret)
                                  (assoc ret mk ret')))))

                          (util/union? expr)
                          (let [jk     (util/join-key expr)
                                jv     (util/join-value expr)
                                class' (-> jv meta :component)]
                            (if (not (vector? data))
                              (let [ret' (extract*
                                           (get jv (first #?(:clj  ((-> class' meta :ident) class' data)
                                                             :cljs (ident class' data))))
                                           data errs)]
                                (recur (next exprs)
                                  (when-not (nil? ret)
                                    (assoc ret jk ret'))))
                              (let [ret' (into []
                                           (map #(extract*
                                                   (get jv
                                                     (first #?(:clj  ((-> class' meta :ident) class' %)
                                                               :cljs (ident class' %))))
                                                   % errs))
                                           data)]
                                (recur (next exprs)
                                  (when-not (nil? ret)
                                    (assoc ret jk ret'))))))

                          (util/join? expr)
                          (let [jk   (util/join-key expr)
                                jv   (util/join-value expr)
                                ret' (extract* jv data errs)]
                            (recur (next exprs)
                              (when-not (nil? ret)
                                (assoc ret jk ret'))))

                          (and (map? data) (has-error? data))
                          (do
                            (swap! errs
                              #(update-in %
                                [(or (when-not (nil? class)
                                       #?(:clj  ((-> class meta :ident) class res)
                                          :cljs (ident class res)))
                                     k)]
                                (fnil conj #{}) (::error data)))
                            (recur (next exprs) nil))

                          :else
                          (recur (next exprs)
                            (when-not (nil? ret)
                              (assoc ret k data)))))
                      ret))))))]
    (let [errs (atom {})
          ret  (extract* query res errs)]
      {:tree ret :errors @errs})))

(defn reconciler
  "Construct a reconciler from a configuration map.

   Required parameters:
     :state        - the application state. If IAtom value is not supplied the
                     data will be normalized into the default database format
                     using the root query. This can be disabled by explicitly
                     setting the optional :normalize parameter to false.
     :parser       - the parser to be used

   Optional parameters:
     :shared       - a map of global shared properties for the component tree.
     :shared-fn    - a function to compute global shared properties from the root props.
                     the result is merged with :shared.
     :send         - required only if the parser will return a non-empty value when
                     run against the supplied :remotes. send is a function of two
                     arguments, the map of remote expressions keyed by remote target
                     and a callback which should be invoked with the result from each
                     remote target. Note this means the callback can be invoked
                     multiple times to support parallel fetching and incremental
                     loading if desired. The callback should take the response as the
                     first argument and the the query that was sent as the second
                     argument.
     :normalize    - whether the state should be normalized. If true it is assumed
                     all novelty introduced into the system will also need
                     normalization.
     :remotes      - a vector of keywords representing remote services which can
                     evaluate query expressions. Defaults to [:remote]
     :root-render  - the root render function. Defaults to ReactDOM.render
     :root-unmount - the root unmount function. Defaults to
                     ReactDOM.unmountComponentAtNode
     :logger       - supply a goog.log compatible logger
     :tx-listen    - a function of 2 arguments that will listen to transactions.
                     The first argument is the parser's env map also containing
                     the old and new state. The second argument is a map containing
                     the transaction, its result and the remote sends that the
                     transaction originated."
  [{:keys [state shared shared-fn
           parser indexer
           ui->props normalize
           send merge-sends remotes
           merge merge-tree merge-ident
           prune-tree
           optimize
           history
           root-render root-unmount
           pathopt
           migrate id-key
           instrument tx-listen
           easy-reads]
    :or {ui->props    default-ui->props
         indexer      pagora.aum.om.next/indexer
         merge-sends  #(merge-with into %1 %2)
         remotes      [:remote]
         merge        default-merge
         merge-tree   default-merge-tree
         merge-ident  default-merge-ident
         prune-tree   default-extract-errors
         optimize     (fn [cs] (sort-by depth cs))
         history      100
         root-render  #?(:clj  (fn [c target] c)
                         :cljs #(js/ReactDOM.render %1 %2))
         root-unmount #?(:clj   (fn [x])
                         :cljs #(js/ReactDOM.unmountComponentAtNode %))
         pathopt      false
         migrate      default-migrate
         easy-reads   true}
    :as config}]
  {:pre [(map? config)]}
  (let [idxr   (indexer)
        norm?  #?(:clj  (instance? clojure.lang.Atom state)
                  :cljs (satisfies? IAtom state))
        state-is-atom? norm? ;;NOTE
        state' (if norm? state (atom state))
        logger (if (contains? config :logger)
                 (:logger config)
                 #?(:cljs *logger*))
        ret    (Reconciler.
                ;;:config key of reconciler
                 {:state state' :shared shared :shared-fn shared-fn
                  :parser parser :indexer idxr
                  :ui->props ui->props
                  :send send :merge-sends merge-sends :remotes remotes
                  :merge merge :merge-tree merge-tree :merge-ident merge-ident
                  :prune-tree prune-tree
                  :optimize optimize
                  ;; If state is not an atom it is not expected to be
                  ;; normalized, if it is an atom and it isn't normalized you
                  ;; can  set normalize to true to normalize it

                  ;; :normalize (or (not norm?) normalize)
                  :normalize (or (not state-is-atom?) normalize)
                  :history #?(:clj  []
                              :cljs (c/cache history))
                  :root-render root-render :root-unmount root-unmount
                  :logger logger :pathopt pathopt
                  :migrate migrate :id-key id-key
                  :instrument instrument :tx-listen tx-listen
                  :easy-reads easy-reads}
                 ;;:state key (an atom) of reconciler

                 ;;State of the reconciler:
                 (atom {:queue []
                        :remote-queue {} ;;{:remote (..)
                                         ;; :remote2 (..)}
                        :queued-sends {} ;;{:foo (..) :bar (..)}

                        :queued false  ;;schedule-render
                        :sends-queued false ;;schedule-sends

                        :target nil ;;dom node
                        :root nil ;; Component Root Class
                        :render nil
                        :remove nil ;;is a fn
                        :t 0
                        :normalized norm? ;;state-is-atom?
                        }))]
    ret))

(defn reconciler?
  "Returns true if x is a reconciler."
  #?(:cljs {:tag boolean})
  [x]
  #?(:cljs (implements? p/IReconciler x)
     :clj  (or (instance? pagora.aum.om.next.protocols.IReconciler x)
               (satisfies? p/IReconciler x))))

(defn app-state
  "Return the reconciler's application state atom. Useful when the reconciler
   was initialized via denormalized data. From config as passed when constructing reconciler"
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (-> reconciler :config :state))

(defn app-root
  "Return the application's root component from the reconciler's state."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (get @(:state reconciler) :root))

(defn force-root-render!
  "Force a re-render of the root. Not recommended for anything except
   recomputing :shared. Calls render method in reconciler's state"
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  ((get @(:state reconciler) :render)))

(defn from-history
  "Given a reconciler and UUID return the application state snapshost
   from history associated with the UUID. The size of the reconciler history
   may be configured by the :history option when constructing the reconciler."
  [reconciler uuid]
  {:pre [(reconciler? reconciler)]}
  (.get (-> reconciler :config :history) uuid))

(defn tempid
  "Return a temporary id."
  ([] (tempid/tempid))
  ([id] (tempid/tempid id)))

(defn tempid?
  "Return true if x is a tempid, false otherwise"
  #?(:cljs {:tag boolean})
  [x]
  (tempid/tempid? x))

(defn reader
  "Create a Om Next transit reader. This reader can handler the tempid type.
   Can pass transit reader customization opts map."
  ([] (transit/reader))
  ([opts] (transit/reader opts)))

(defn writer
  "Create a Om Next transit writer. This writer can handler the tempid type.
   Can pass transit writer customization opts map."
  ([] (transit/writer))
  ([opts] (transit/writer opts)))

(defn force
  "Given a query expression return an equivalent query expression that can be
   spliced into a transaction that will force a read of that key from the
   specified remote target."
  ([expr]
    (force expr :remote))
  ([expr target]
    (with-meta (list 'quote expr) {:target target})))



;; (force {:a [:b]} :foo)
;; (meta (force {:a [:b]} :foo))

;; (meta ^{:foo :bar}{:a [:b]})

