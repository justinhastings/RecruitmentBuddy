(ns asgnx.lambda
  "
    Provides the entry-point from AWS Lambda into the SMS application.

    This code is written in Clojurescript (hence the .cljs extension) and
    is designed to run on NodeJS. The asgnx.aws and asgnx.twilio namespaces
    also are written in Clojurescript and use NodeJS libraries.

    The SMS Api:

    expert <topic> ;; the person that sent the message becomes an expert on
                      the specified topic

    ask <topic> <question> ;; broadcast a text to everyone registered as an
                              expert on that topic and send any replies back
                              to the asker

    answer <your answer> ;; sends a reply back to the last question that the
                            expert received

    welcome <name> ;; replies with a welcome message

    homepage ;; returns the CS x278 homepage

    office <day> ;; returns the office hours for the given day of the week
                    (e.g., office tuesday)
  "
  (:require [asgnx.core :as core]
            [asgnx.aws.s3 :as s3]
            [asgnx.aws.ssm :as ssm]
            [asgnx.kvstore :as kvstore]
            [asgnx.twilio.core :as twilio]
            [clojure.string :as string]
            [clojure.core.async :refer [go <! chan]]
            [cljs-lambda.context :as ctx]
            [cljs-lambda.macros :refer-macros [defgateway deflambda]]))

(enable-console-print!)

(defn url-encoded-params [qstr]
  (into {}
        (filter #(= (count %) 2)
                 (map (fn [p] (string/split p #"="))
                      (string/split qstr #"&")))))

(defn params [event]
  (cond
    (:query event) (:query event)
    (:body event) (url-encoded-params (:body event))
    :default {}))

(defn decode [s]
  (when s
    (js/decodeURIComponent s)))

(defn decode-with-spaces [s]
  (when s
    (decode (string/replace s "+" " "))))

(defn handle-message
  "This method parses Lambda events from API Gateway and sends the
   processed messages on to core/handle-message.

   This function creates an AWS Lambda-compatible version of the
   system configuration that saves state to S3 and sends text messages
   via Twilio. The system configuration also provides access to secret
   credentials stored in the SSM parameter store."
  [event ctx]
  (go
    (let [args   (params event)
          from   (decode (get args "From"))
          to     (decode (get args "To"))
          msg    (decode-with-spaces (get args "Body"))
          system {:system-phone-number to
                  :credentials-mgr     ssm/get-secrets!
                  :effect-handlers     (merge kvstore/action-handlers
                                              twilio/action-handlers)
                  :state-mgr           (s3/s3-keystore "cs4278-asgnx-state")}
          resp   (<! (core/handle-message system from msg))]

        (println "Response: " resp)

        {:status 200
         :headers {:content-type "text/plain"}
         :body resp})))

(defgateway receive-message [event ctx]
  (handle-message event ctx))

(defn node-repl []
  (println "Node repl up!"))
