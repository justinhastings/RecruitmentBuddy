(ns asgnx.cli
  (:require [asgnx.core :as core]
            [asgnx.kvstore :as kvstore]
            [clojure.core.async :refer [<!! go]]))

(defn run [handler quit? recv send]
  (let [msg (recv)]
     (if (quit? msg) ;; Check if this is a quit message
         nil ;; Exit the loop
         (let [result (<!! (handler msg))] ;; Otherwise process the msg
            ;; Send the output
            (send result)
            ;; Recursively call run but pass in the new-state that will be used
            ;; when processing the next message
            (recur handler quit? recv send)))))


(defn should-quit? [m]
  (= "quit" m))

;; This is an impure function that reads from standard input, so we try
;; to keep it as simple as possible.
(defn prompt-for-input []
  (print "CSx278: ")
  (flush)
  (read-line))

;; This is an impure function that prints to standard out, so we try
;; to keep it as simple as possible.
(defn print-output [o]
  (println "out => " o))

;; A function to launch a command line input loop
(defn cli-loop []
  (let [ehdlrs (merge
                 {:send (fn [s a] (go (println "    >>>> Sending SMS:" a)))}
                 kvstore/action-handlers)
        state  (atom {})
        smgr   (kvstore/create state)
        system {:state-mgr smgr
                :effect-handlers ehdlrs}]
    (run
      #(core/handle-message system "console_user" %)
      should-quit? ;; Check for the phrase "quit"
      prompt-for-input ;; Print the prompt and read a message from stdin
      print-output))) ;; Send output to stdout


(defn -main
  "This is the entry point to this code from the command line
   that will create a cli prompt for the app. You can type commands
   into the prompt once you see `CSx278:`.
   CSx278: welcome <name>
   => Welcome jules
   CSx278: homepage
   => http://....

   CSx278: office tuesday
   => 8am-10am ...
  "
  [& args]
  (cli-loop))
