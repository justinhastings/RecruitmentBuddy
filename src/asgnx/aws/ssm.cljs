(ns asgnx.aws.ssm
  (:require [clojure.core.async :as async :refer [<! >! chan]]
            [clojure.walk :as walk]
            [cljs.nodejs :as nodejs]
            [clojure.string :as string]
            [cognitect.transit :as t])
  (:require-macros [cljs.core.async :refer [go]]
                   [asgnx.aws.core :refer [aws]]))

(def AWS (nodejs/require "aws-sdk"))
(.update (aget AWS "config") (clj->js {:region "us-east-1"}))

(def SSM (nodejs/require "aws-sdk/clients/ssm"))
(def ssm-client (new SSM))

(defn name-val-map
  "Converts a list of the form [{kn s1 kv s2}...{kn s3 kv s4}]]
   to {s1 s2
       s3 s4}

   Example:

  (name-val-map [{:Name foo :Value 1} {:Name bar :Value 2}] :Name :Value)
   =>
   {:foo 1 :bar 2}
  "
  [l kn kv]
  (apply merge (map #(hash-map (keyword (kn %)) (kv %)) l)))

(defn get-secrets! [names]
  (go
    (some->
      (<! (aws .getParameters
            ssm-client
            {:Names names :WithDecryption true}))
      second
      :Parameters
      (name-val-map :Name :Value))))
