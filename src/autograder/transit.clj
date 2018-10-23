(ns autograder.transit
  (:require [cognitect.transit :as t])
  (:import  [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn read-transit 
  "Read a file with Transit that is encoded as json."
  [s]
  (t/read 
    (t/reader 
      (ByteArrayInputStream. 
        (.getBytes s)) 
      :json)))


(defn write-transit [d]
 (let [out (ByteArrayOutputStream. 4096)]
  (t/write
    (t/writer out :json)
    d)
  (.toString out)))


(defn slurp-transit 
  "Read a file with Transit that is encoded as json."
  [f] (read-transit (slurp f)))
  

(defn spit-transit
  "Marshall data with transit and spit it to a file"
  [f d] (spit f (write-transit d)))
