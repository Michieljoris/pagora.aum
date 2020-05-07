(ns pagora.aum.modules.auth.db-config.auth
  #?(:cljs (:require-macros [pagora.aum.database.validate.Rule :refer [rule]]))
  (:require [pagora.aum.database.validate.core :as bv :refer [Rules]]
            [pagora.aum.database.validate.rules :as rule :refer [require-keys]]
            #?(:clj [pagora.aum.database.validate.Rule :refer [rule]])
            [pagora.aum.modules.auth.db-config.util :refer [master-account-admin-scope account-admin-scope
                                                            not-deleted-scope
                                                            validate-master-account-admin-access
                                                            validate-account-admin-access
                                                            account-id-has-to-be-valid
                                                            ]]
            [pagora.aum.database.query :refer [sql]]
            [taoensso.timbre :as timbre :refer [error info]])
  )


(def config {:root true
             :table-name "auth"
             :columns [:id :account-id :user-id :role-id]
             :read {:blacklist []}})
