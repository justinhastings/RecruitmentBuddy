(ns asgnx.twilio.core
  (:require [clojure.core.async :as async :refer [<! >! chan go]]
            [clojure.walk :as walk]
            [cljs.nodejs :as nodejs]
            [clojure.string :as string]
            [cognitect.transit :as t]))

(def Twilio (nodejs/require "twilio"))

(defn client [account-sid token]
  (new Twilio account-sid token))

(defn send-sms! [client params]
  (let [r (chan 1)]
    (go
      (try
        (.then
          (.create (.-messages client)
                   (clj->js params))
          (fn [resp]
            (go
              (try
                (>! r (js->clj (.toJSON resp) :keywordize-keys true))
                (catch :default e
                  (println "Twilio handler error: " e))))))
        (catch :default e
          (println "Twilio Error:" e)
          (>! r e))))
    r))

(defn action-send [{:keys [credentials-mgr system-phone-number]} {:keys [to msg]}]
  (go
    (let [creds  (<! (credentials-mgr ["twilio-prod-account-sid" "twilio-prod-token"]))
          sid    (:twilio-prod-account-sid creds)
          tkn    (:twilio-prod-token creds)
          client (client sid tkn)]
      (send-sms! client {:to to :from system-phone-number :body msg}))))

(def action-handlers
  {:send action-send})
