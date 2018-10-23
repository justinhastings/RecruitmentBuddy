(ns asgnx.aws.core
  (:require [clojure.core.async :as async :refer [chan >! <! put! go]]
            [clojure.walk :as walk]))


(defmacro aws [n c p]
  `(let [rchan# (chan 1)]
    (go
      (try
         (~n ~c (cljs.core/clj->js ~p)
           (fn [err# d#]
             (put! rchan# [err# (walk/keywordize-keys
                                 (cljs.core/js->clj d#))])))
         (catch :default e# (println "Error " e#)(put! rchan# [e# nil]))))
    rchan#))
