--NOTE Don't forget to add sql fun to build_sql.cljc if you want it to work in tests or in mock mode!!! NOTE

-- :name test-fn :? :1
SELECT
--~ (if (seq (:cols params)) ":i*:cols" "*")
FROM users

--- *************** Getting/updating of user(s) for authorization purposes
-- :name select-user-by-remember_token :? :1
SELECT
--~ (if (seq (:cols params)) ":i*:cols" "*")
FROM users WHERE remember_token=:remember_token

-- :name select-user-by-email :? :1
SELECT * FROM users WHERE email=:email

-- :name select-user-by-id :? :1
SELECT * FROM users WHERE id=:id

-- :name set-user-remember_token! :!
UPDATE users SET remember_token=:remember_token WHERE users.id=:id

--- *************** Interpretation of om/next queries
-- :name get-cols-from-table :? :*

--- NOTE: current version of hugsql (0.4.9) doesn't pick up this hint.
--- https://github.com/layerware/hugsql/issues/71
select  /*+ MAX_EXECUTION_TIME(1000) */
--~ (if (seq (:cols params)) ":i*:cols" "*")
from :i:table
--~ (when (:where-clause params) ":snip:where-clause")
--~ (when (:order-by-clause params) ":snip:order-by-clause")
--~ (when (:limit-clause params) ":snip:limit-clause")

-- :name enable-only-full-group-by :!
SET GLOBAL sql_mode=(SELECT CONCAT(@@sql_mode,",ONLY_FULL_GROUP_BY"));

-- :name disable-only-full-group-by :!
SET GLOBAL sql_mode=(SELECT REPLACE(@@sql_mode, "ONLY_FULL_GROUP_BY", ""));

-- :name get-sql-mode
SELECT @@sql_mode

-- :name get-joined-rows :? :*
select /*+ MAX_EXECUTION_TIME(1) */
--~ (if (:count? params) (str "count(DISTINCT " (:t2-name params) ".id) as count" ) "DISTINCT :i*:cols")
from :i:t1-name
--~ (when (:t1=t2? params) ":i:t1-alias")
inner join :i:join-table
on
--~ (str (:t1-alias params) ".id=" (:join-table params) "." (:t1-foreign-key params))
inner join :i:t2-name
--~ (when (:t1=t2? params) ":i:t2-alias")
on
--~ (str (:t2-alias params) ".id=" (:join-table params) "." (:t2-foreign-key params))
--~ (when (:where-clause params) ":snip:where-clause")
-- GROUP BY CONCAT(
-- --~ (str  (:join-table params) "." (:t1-foreign-key params) ", \"-\", " (:t2-alias params) ".id")
-- )
--~ (when (:order-by-clause params) ":snip:order-by-clause")
--~ (when (:limit-clause params) ":snip:limit-clause")

--- *************** Mutations
-- :name insert-record :i!
/*~
(if (:no-timestamp? params)
"insert into :i:table (:i*:cols) VALUES (:v*:vals)"
"insert into :i:table (:i*:cols, updated_at, created_at) VALUES (:v*:vals, :v:created-at, :v:updated-at)")
~*/


-- :name insert-records :! :n
insert into :i:table (:i*:cols) values :tuple*:vals


-- :name update-record :! :n
update :i:table
set
--~  (clojure.string/join "," (map #(str ":i:cols." % " = :v:vals." %) (range (count (:cols params)))))
--~  (when (not (:no-timestamp? params)) ", updated_at = :v:updated-at")
--~ ":snip:where-clause"

-- :name bulk-update :!
update :i:table
set
--~  (clojure.string/join "," (map #(str ":i:cols." % " = :v:vals." %) (range (count (:cols params)))))
--~  (when (not (:no-timestamp? params)) ", updated_at = :v:updated-at")
--~ ":snip:where-clause"

-- :name delete-record :! :n
delete from :i:table
--~ ":snip:where-clause"

-- :name insert-event :i!
insert into :i:table (:i*:cols, created_at) VALUES (:v*:vals, :v:created-at);

-- *************** misc queries
-- :name count-belongs-to :? :1
select count(*) from :i:table where :i:belongs-to-column = :id
--~ (when (:cond params) ":sql:cond")

-- :name get-now :? :1
-- :doc Get the database date and time
SELECT NOW() AS now

-- :name count :? :1
select count(*) as `count` from :i:table
--~ (when (:where-clause params) ":snip:where-clause")

-- :name count-by-join :? :*
select :i:t1-foreign-key, count(*) as `count` from :i:table
--~ (when (:where-clause params) ":snip:where-clause")
group by :i:t1-foreign-key


-- *************** db introspection
-- :name show-tables :? :*
-- :doc Show all the tables of the database
show tables;

-- :name describe-table :? :*
-- :doc Regular describe for a table
describe :i:table;

-- :name global-time-zone :? :1
SELECT @@global.time_zone;

-- :name session-time-zone :? :1
SELECT @@session.time_zone;

-- :name set-global-time-zone :!
SET @@global.time_zone = '+00:00';

-- :name set-session-time-zone :!
SET @@session.time_zone = '+00:00';

--- *************** Test db
-- :name drop-db :!
drop database if exists :i:db-name;

-- :name create-db :!
create database if not exists :i:db-name;

-- :name create-table :!
CREATE TABLE :i:table-name (:i*:columns)

-- :name insert-rows :! :n
-- :doc Insert multiple rows with :tuple* parameter type
INSERT INTO :i:table-name (:i*:cols) VALUES :tuple*:rows;

-- :name ids :? :1
SELECT LAST_INSERT_ID() AS last_id,ROW_COUNT() AS row_count;

-- Not used
-- :snip select-snip
select :i*:cols

-- :snip from-snip
from :i*:tables

-- :snip assign-snip
:i:pv.prop = :v:pv.val

-- :name select-foo
SELECT * from fields



-- :snip clause-snip
--~ (:prefix params)
(:snip*:cond)

-- :snip cond-snip
/*~
(str (or (:prefix params)) " "
(condp = (:p-types params)
:iv ":i:cond.0 :sql:cond.1 :v:cond.2"
:iv* ":i:cond.0 :sql:cond.1 (:v*:cond.2)"
:vi ":v:cond.0 :sql:cond.1 :i:cond.2"
:ii ":i:cond.0 :sql:cond.1 :i:cond.2"
:vv ":v:cond.0 :sql:cond.1 :v:cond.2"
:in ":i:cond.0 :sql:cond.1 NULL"
""))
~*/


-- :name user+subscription :? :*
select
--~ (if (:count? params) "count(distinct a.id) as count" (if (seq (:cols params)) ":i*:cols" "a.*"))
from users as a inner join subscriptions as b on a.id=b.user_id
--~ (when (:where-clause params) ":snip:where-clause")
--~ (when (not (:count? params)) "GROUP BY a.id")
--~ (when (:order-by-clause params) ":snip:order-by-clause")
--~ (when (:limit-clause params) ":snip:limit-clause")
