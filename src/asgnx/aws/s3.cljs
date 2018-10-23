(ns asgnx.aws.s3
  (:require [asgnx.kvstore :as kvstore :refer [put! get! list! remove! KeyStore]]
            [clojure.core.async :as async :refer [<! >! chan]]
            [clojure.walk :as walk]
            [cljs.nodejs :as nodejs]
            [clojure.string :as string]
            [cognitect.transit :as t])
  (:require-macros [cljs.core.async :refer [go]]
                   [asgnx.aws.core :refer [aws]]))

(def S3 (nodejs/require "aws-sdk/clients/s3"))
(def s3-client (new S3))

(defn- path [k]
  (string/join "/" (map name k)))

(defn- key [path]
  (keyword (first (reverse (string/split path #"/")))))

(defn- object-keys [ks]
  (map #(key (:Key %)) ks))

(defn- marshall [v]
  (let [w (t/writer :json)]
    (t/write w v)))

(defn- unmarshall [v]
  (let [r (t/reader :json)]
    (t/read r v)))

(defn body [r]
  (.toString (:Body r)))

(defn data [r]
  (unmarshall (body r)))

(deftype S3Store [bucket]
  KeyStore
  (put!
    [_ key value]
    (aws .putObject
          s3-client
         {:Bucket bucket
          :Key  (path key)
          :Body (marshall value)}))
  (remove! [_ key]
    (go
      (some->
        (<! (aws .deleteObject
                 s3-client
                 {:Bucket bucket
                  :Key    (path key)}))
        second
        :DeleteMarker)))
  (get! [_ key]
    (go
      (some->
          (<! (aws .getObject
                   s3-client
                   {:Bucket bucket
                    :Key    (path key)}))
          second
          data)))
  (get! [s key not-found]
    (go
       (or (<! (get! s key)) not-found)))
  (list! [_ key-prefix]
    (println "list!" (path key-prefix))
    (go
      (object-keys
       (some->
          (<! (aws .listObjects
                   s3-client
                   {:Bucket bucket
                    :Prefix (path key-prefix)}))
          second
          :Contents)))))

(defn s3-keystore [bucket]
  (S3Store. bucket))
