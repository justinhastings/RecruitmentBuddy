(ns autograder.auth
  (:require [me.raynes.conch :refer [programs with-programs let-programs] :as sh]
            [autograder.transit :as transit])
  (:import  [java.io ByteArrayInputStream ByteArrayOutputStream]))

(programs aws aws2)


(try (aws "--version")
     (catch Exception e
            (println "
                     ============================================================

                     ERROR!!!!

                     You must install the Amazon AWS Command Line tools
                     (https://aws.amazon.com/cli/) and ensure that the 'aws'
                     command is on your path.

                     ============================================================
            ")
            (System/exit 1)))

(defn aws-cli [params]
 (try
   (transit/read-transit
     (apply aws (concat params ["--region" "us-east-1"
                                "--no-sign-request"])))
   (catch Exception e
          (-> e ex-data :proc :err println)
          nil)))

(defn id-token [response]
  (get-in response ["AuthenticationResult" "IdToken"]))

(defn access-token [response]
  (get-in response ["AuthenticationResult" "AccessToken"]))

(defn refresh-token [response]
  (get-in response ["AuthenticationResult" "RefreshToken"]))

(defn expires-in [response]
  (get-in response ["AuthenticationResult" "ExpiresIn"]))

(defn get-access-token [client-id refresh-token]
  (if-let [resp (aws-cli
                 ["cognito-idp" "initiate-auth"
                  "--auth-flow" "REFRESH_TOKEN_AUTH"
                  "--client-id" client-id
                  "--auth-parameters" (str "REFRESH_TOKEN=" refresh-token)])]
    resp))


(defn user-attributes
  "Extracts the [{Name xyz, Value qrs} {Name xyz2, Value qrs2}...] format
   of user attributes from Cognito into a map in the format {xyz qrs xyz2 qrs2}."
  [m]
  (->> (get m "UserAttributes")
       (map #(vector (keyword (get % "Name")) (get % "Value")))
       (into {})))

(defn get-user [tokens]
  (if-let [resp (aws-cli
                 ["cognito-idp" "get-user"
                  "--access-token" (:access-token tokens)])]
    (-> resp
        user-attributes
        (assoc :username (get resp "Username")))))



(defn prompt [msg]
  (println msg)
  (read-line))

(defn tokens [resp]
  {:expiration    (+ (System/currentTimeMillis) (* 1000 (expires-in resp)))
   :access-token  (access-token resp)
   :id-token      (id-token resp)
   :refresh-token (refresh-token resp)})

(defn authenticate
  ([client-id user pass]
   (if-let [resp (aws-cli
                  ["cognito-idp" "initiate-auth"
                   "--auth-flow" "USER_PASSWORD_AUTH"
                   "--client-id" client-id
                   "--auth-parameters" (str "USERNAME=" user ",PASSWORD=" pass)])]
     (tokens resp)))
  ([client-id refresh-token]
   (if refresh-token
     (tokens (get-access-token client-id refresh-token))
     (let [user (prompt "Username:")
           pass (prompt "Password:")]
       (authenticate client-id user pass)))))
