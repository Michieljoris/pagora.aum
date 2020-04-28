(ns pagora.aum.modules.events.experimental.events-consumer)
(comment
  (ns pagora.aum.modules.events.experimental.events-consumer
    (:require
     ;; [digicheck.common.util :refer [json->clj]]
     ;; [aum.reconciler.parser.mutate-helpers :refer [is-dirty]]
     [digicheck.common.util :as u :refer [hyphen->underscore underscore->hyphen]]
     [clojure.data.json :as json]
     [digicheck.macros :refer [assert-x]]

     [fixtures.core :as fixtures]
     [aum.database
      [inspect :refer [table-name]]
      [queries :as query]]

     [digicheck.database.connection :as db-conn]
     [aum.database.schema :as schema]

     [hugsql.core :as hugsql]
     [aum.database.query :refer [sql make-query-params]]
     ;; String manipulation
     [clojure.test :refer [deftest is are]]
     [cuerdas.core :as str]
     [cljs.core]
     [om.next :as om]
     [clojure.data.json :as json]
     [digicheck.macros :refer [if-let*]]
     [taoensso.timbre :as timbre]
     [clojure.pprint :refer [pprint]]
     )
    ))

(comment
  ;; * One more idea One more idea: Only have three events: UPDATE, CREATE and
  ;; DELETE. Extra info for CREATE event: table and id and time. Extra info for
  ;; DELETE: table, id and whole (deleted) record. UPDATE extra info: table, id,
  ;; column, old value, new value. Then that's done. No more events to define.
  ;; Then you can make more specific event by creating a rule like (and (=
  ;; event :UPDATE (= table :dossier) (= id ANY) (= column :status) (=
  ;; new-value "Completed")) and call that :dossier-status-completed. And use that
  ;; to build more complex rules. I'm completely ignoring optimization, edge
  ;; cases, action verifying, which events to feed into it. In any case, you can
  ;; then define any event you want in a ui. People can define their own events.
  ;; And link them to actions, like sending email. Variables like table, id,
  ;; new-value are passed to the action, so for instance they can be used in an
  ;; email. If that's the action.


  ;; Events:


  ;; #+NAME: events
  ;; #+BEGIN_SRC clojure
  ;; {:type :update :table :dossier :id 1 :column :name :new-value "new-name" :old-value "old-name"}

  ;; {:type :update :table :checklist :id 1 :column :status :new-value "completed" :old-value "open" :updated-at "some-time"}

  ;; {:type :create :table :answer :id 1 :created-at "some-time"}
  ;; {:type :update :table :dossier :id 1 :column :status :new-value "completed" :old-value "open" :updated-at "some-time"} ;;!!!!!!!!!!!!11

  ;; {:type :update :table :answer :id 1 :column :value  :new-value "YES" :old-value "NO" :updated-at "some-time"}
  ;; {:type :update :table :remark :id 1 :column :value  :new-value "Some remark" :old-value "" :updated-at "some-time"}

  ;; {:type :create :table :checklist :id 100 :created-at "some-time"}
  ;; {:type :update :table :checklist :id 100 :column :name  :new-value "new checklist's name" :old-value nil :updated-at "some-time"}
  ;; ;;more values set at creation

  ;; {:type :update :table :answer :id 1 :column :value  :new-value "NO" :old-value "YES" :updated-at "some-time"}



  ;; (and
  ;;  (and (= event :UPDATE) (= event.table :checklist) (= event.id '*) (= event.column :status) (= event.new-value "Completed") (= (event.entity.name "some name"))

  ;; )

  ;; )

  ;; (defn make-compound-event [type table id column new-value]
  ;; )
  ;; #+END_SRC

  ;; Hi. I've put something together, more for fun, but might be practical, It is
  ;; able to take in a batch of events and evaluate rules. The rules can be as
  ;; complicated as you wish, and there are only three events needed. In the
  ;; little experimenting I did it seems to work ok. You have only
  ;; update/create/delete events, you can then evaluate a statement about the
  ;; state of the system at any moment in time. It's not needed to reconstruct the
  ;; whole system, only the bits needed to evaluate the rule. So it's kinda easy
  ;; for instance to make a rule about change of status, but also for instance to
  ;; for if status becames so and so AND the name of the dossier is so and so. And
  ;; with a bit more trickery you can can find out about the status of any other
  ;; record as well. So also of related records. So the status of the parent
  ;; dossier for instance. About performance I haven't worried too much. Every
  ;; rule eval takes a few sql queries at the least for every event. Lots of
  ;; rules, lots of events can add up. But the events db can have it's own sql
  ;; server and data dir. And you could have a worker to evaluate the rules per
  ;; group. Also can think of some more optimizations. Of course if you fleshed
  ;; this out you might run into (big) problems. Don't know yet really. But it's a
  ;; nice thing to experiment with perhaps for proper workflow later on. If you
  ;; like we can go through it next week somewhere? You thought about this stuff
  ;; more than I probably by now, curious what you think. Anyway, back to ui and
  ;; css and pixel shifting for me..

  (hugsql/def-db-fns "database/sql/events.sql" {:quoting :mysql})
  (hugsql/def-sqlvec-fns "database/sql/events.sql" ;; {:quoting :mysql}
    )

  (def db-conn-params {:url "//localhost:3306/"
                       :db-name "events"
                       ;; :db-name "chinchilla_development"
                       :print-spec true
                       :user "root"
                       :password ""})

  (def db-conn (db-conn/make-db-connection db-conn-params))

  (defn sql-process-params [env fun params]
    (condp = fun
      :events-select params
      nil))

  (defn sql-process-result [env fun params result]
    (condp = fun
      :events-select result
      nil)
    )

  (def env {:db-conn db-conn
            :schema (schema/get-schema db-conn)
            :parser-config {:sql-log true}
            :sql {:process-params-plus sql-process-params
                  :process-result-plus sql-process-result
                  :hugsql-ns-plus "events.consumer"}})


  (defn make-db-fixture
    [db-conn-params fixtures cols-types]
    (let [mysql-conn (db-conn/make-db-connection (assoc db-conn-params :db-name "mysql"))
          db-conn (db-conn/make-db-connection db-conn-params)
          db-name (:db-name db-conn-params)]
      (fixtures/setup-test-db db-name mysql-conn db-conn fixtures cols-types false)))

  ;; (clojure.pprint/pprint env)

  ;; (sql env :events-select {})
  ;; (sql env :bulk-update {:table :user :updates {:name "bla"}
  ;;                        :where-cols [:id := 1]
  ;;                        :where [:id := 2]})

  ;; (sql env :insert-record {:table :event
  ;;                          :cols [:type]
  ;;                          :vals ["update"]})

  (do
    (def events
      [
       {:type :create :table :dossier :entity-id 1 :column "" :value (json/write-str {:id 1 :name "dossier-name"})}
       {:type :update :table :dossier :entity-id 1 :column :name :value "new-name 1"
        }

       {:type :create :table :checklist :entity-id 1 :column "" :value (json/write-str {:id 1 :name "checklist-name"
                                                                                        :dossier-id 1
                                                                                        })}
       {:type :update :table :checklist :entity-id 1 :column :status :value "completed"}

       {:type :create :table :answer :entity-id 1 :column "" :value (json/write-str {:id 1 :name "answer-name"
                                                                                     :question-type 3})}
       {:type :update :table :dossier :entity-id 1 :column :status :value "completed"}

       {:type :update :table :answer  :entity-id 1 :column :value  :value "YES"}
       {:type :create :table :remark :entity-id 1 :column "" :value (json/write-str {:id 1 :value "checklist-name"})}
       {:type :update :table :remark  :entity-id 1 :column :value  :value "Some remark"}

       {:type :update :table :dossier :entity-id 1 :column :name :value "new-name 2"
        }

       {:type :update :table :checklist  :entity-id 1 :column :name  :value "new checklist's name"}
       ;;more values set at creation

       { :type :update :table :answer :entity-id 1 :column :value  :value "NO"}
       ]
      )
    (defn stringify-keywords [v]
      (mapv (fn [r]
              (into {} (map (fn [[k v]]
                              [k (if (keyword? v) (name v) v)])
                            r))
              )
            v))
    (defn number-events [events]
      (map-indexed (fn [index event]
                     (assoc event :id (inc index)))
                   events))

    ;; (stringify-keywords events)

    (make-db-fixture db-conn-params {:events (-> events
                                                 number-events
                                                 stringify-keywords)}
                     {:id :int  :type "varchar(255)"
                      :table "varchar(255)"
                      :entity_id :int
                      :column "varchar(255)"
                      :value :text
                      })
    )
  ;; (make-db-fixture db-conn-params fixtures)

  (defn query-events [{:keys [where limit order-by]}]
    (let [query-params
          (make-query-params {:db-config {:pluralize-table-names? true}}
                             {:table :event
                              :cols [:id :type :table :entity-id :column :value]
                              :where where
                              :limit limit
                              :order-by order-by})]
      (sql env :get-cols-from-table query-params)))

  (defn json-read-str [s]
    (try (json/read-str s)
         (catch Exception e)))

  (do
    (defn reconstruct-entity [table entity-id at]
      (let [where-conditions [[:table := "dossier"] [:entity-id := entity-id]]
            where-conditions (cond-> where-conditions
                               at (conj [:id :<= at]))
            entity-events (query-events {:where [:and where-conditions]})
            _ (assert-x (or (empty? entity-events)
                            (= (:type (first entity-events)) "create"))
                        "First entity event should be a create!!!" {:entity-events entity-events})
            create-event (first entity-events)
            entity (json-read-str (:value create-event))
            updates (rest entity-events)
            entity (reduce (fn [e {:keys [id column value]}]
                             (assoc e (name column) value))
                           entity updates)]

        entity
        )
      )
    (doseq [event-id (range (count events))]
      (pprint [(inc event-id) (reconstruct-entity :dossier 1 (inc event-id))]
              )))

  (do
    (defn find-value [{:keys [table id]} event-id k]
      (let [where [:and [[:table := (name table)]
                         [:id :< event-id]
                         [:or [[:column := (name k)]
                               [:type := "create"]]]
                         ]]
            limit {:count 1}
            order-by {:id :desc}

            query-params
            (make-query-params {:db-config {:pluralize-table-names? true}}
                               {:table :event
                                :cols [:id :type :table :entity-id :column :value]
                                :where where
                                :limit limit
                                :order-by order-by
                                })
            table-events (sql env :get-cols-from-table query-params)
            {:keys [type value]} (first table-events)
            value (if (= type "create")
                    (get (json-read-str value) (name k))
                    value)]
        ;; (pprint value)
        ;; (pprint (sql env :get-cols-from-table query-params))
        value)
      )

    (defn get-value-at [{:keys [id type table column value] :as event} k]
      (let [k (name k)]
        (timbre/info "Getting value for " k "for event " event "")
        ;; Find event with creation-event.id <= id < event.id where some-event.type=update and some-event.column=column or
        ;; (some-event.type = :create and some-event.table = event.table and some-event.id = event.id)
        ;; limit 1 and sort-by id desc
        (condp = type
          :create (let [record (json-read-str value)]
                    (get record k))
          :update (condp = k
                    (name column) value
                    (find-value event (:id event) k)
                    )))
      )

    (defmacro defrule [rule-name rule]
      `(def ~rule-name (quote ~rule))
      )

    (def rule1 '(and (= (:type event) :create)
                     (= (get-value-at event :name) "new-answer")
                     (= (:table event) :answer)
                     ;; (= ( id event) '*)
                     ;; (= (:column event) :name)
                     )
      ;; (= (:name (entity event)) "Bla")
      ;; (= event.entity.dossier.status "completed")
      )
    ;; (pprint bla)

    (defmacro eval-rule [rule event]
      (let [r (eval rule)
            e (eval event)]
        `(let [~'event ~e]
           ~r))
      )
    ;; (pprint (macroexpand '(defrule foo (and (= event :foo)))))
    ;; (pprint (macroexpand '(eval-rule rule1 (get events 2))))
    ;; (pprint (eval-rule rule1 (get events 2)))

    (def rule2 '(and ;; (= (:type event) :update)
                 (= (get-value-at event :name) "new-name 2")
                 (= (:table event) :dossier)
                 ;; (= ( id event) '*)
                 ;; (= (:column event) :name)
                 )
      ;; (= (:name (entity event)) "Bla")
      ;; (= event.entity.dossier.status "completed")
      )
    ;; (eval-rule rule2 (get (vec (number-events events)) 9))
    (defrule is-checklist-rule '(= table :checklist))

    (def rule3 '(let [{:keys [table]} event]
                  (if (= table :checklist)
                    (let [dossier-id (get-value-at event :dossier-id)
                          checklist-dossiers-name (find-value {:table :dossier :id dossier-id }
                                                              (:id event)
                                                              :name)]
                      ;; (pprint "------------")
                      ;; (pprint dossier-id)
                      ;; (pprint checklist-dossiers-name)
                      ;; (pprint "------------")

                      (and ;; (= (:type event) :update)
                       (= (get-value-at event :name) "checklist-name")
                       (= table :checklist)
                       ;; ~is-checklist-rule
                       (= checklist-dossiers-name "new-name 1"))
                      ))

                  ;; (= ( id event) '*)
                  ;; (= (:column event) :name)
                  )
      ;; (= (:name (entity event)) "Bla")
      ;; (= event.entity.dossier.status "completed")
      )
    (pprint (macroexpand '(eval-rule rule3 (get (vec (number-events events)) 3))))
    (eval-rule rule3 (get (vec (number-events events)) 3))))
;; This rule cares about the checklist's name, and its dossier's status
