(ns asgnx.core
  (:require [clojure.string :as string]
            [clojure.core.async :as async :refer [go chan <! >!]]
            [asgnx.kvstore :as kvstore
             :refer [put! get! list! remove!]]))

;; Definitions of maps that contain the companies that are currently
;; represented in our database

(def employee-companies {})

(def ex-employee-companies {})

(def prior-candidate-companies {})


;; This is a helper function that you might want to use to implement
;; `cmd` and `args`.
(defn words [msg]
  (if msg
      (string/split msg #" ")
      []))

;; Returns the first word in a text
;; message.
;;
;; Example: (cmd "foo bar") => "foo"
;;
(defn cmd [msg]
  (get (words msg) 0))

;; Returns the list of words following
;; the command in a text message.
;;
;; Example: (args "foo bar baz") => ("bar" "baz")
;;
(defn args [msg]
  (rest (words msg)))

;; Returns a map with keys for the
;; :cmd and :args parsed from the msg.
;;
;; Example: (parsed-msg "foo bar baz") => {:cmd "foo" :args ["bar" "baz"]}
;;
(defn parsed-msg [msg]
  {:cmd (cmd msg) :args (args msg)})

;; Takes a destination for the msg in a parameter called `to`
;; and the message in a parameter called `msg` and returns
;; a map with the keys :to and :msg bound to each parameter.
;; The map should also have the key :action bound to the value
;; :send.
;;
(defn action-send-msg [to msg]
  {:to to
    :msg msg
    :action :send})


;; Takes a list of people to receive a message in a `people`
;; parameter and a message to send them in a `msg` parmaeter
;; and returns a list produced by invoking the above `action-send-msg`
;; function on each person in the people list.
;;
(defn action-send-msgs [people msg]
  (map action-send-msg people (repeat msg)))


;; Takes a list of keys in a `ks` parameter, a value to bind to that
;; key path to in a `v` parameter, and returns a map with
;; the key :ks bound to the `ks` parameter value and the key :v
;; vound to the `v` parameter value.)
;; The map should also have the key :action bound to the value
;; :assoc-in.
;;
(defn action-insert [ks v]
  {:action :assoc-in :ks ks :v v})

;; Parameters:
;; 1. a key prefix (e.g., [:a :b])
;; 2. a list of suffixes for the key (e.g., [:c :d])
;; 3. a value to bind
;;
;; and calls (action-insert combined-key value) for each possible
;; combined-key that can be produced by appending one of the suffixes
;; to the prefix.
;;
(defn action-inserts [prefix ks v]
  (map action-insert (map conj (repeat prefix) ks) (repeat v)))

;; Takes a list of keys in a `ks` parameter and returns a map with
;; the key :ks bound to the `ks` parameter value.
;; The map should also have the key :action bound to the value
;; :dissoc-in.
;;
(defn action-remove [ks]
  {:action :dissoc-in :ks ks})

;; Welcome message with details on commands
(defn welcome [_]
  (str "Greetings from the Vanderbilt University RecruitmentBuddy Chatbot! You
can text this bot to connect with alumni or current students who are on the job hunt, current employees,
 previous employees, or have interviewed at companies you're interested in. Here is a
list of commands to get you started:
\n\n \"ask-employees <company-name> ...\" - Ask a question to all current employees of a company
\n\n \"ask-ex-employees <company-name> ...\" - Ask a question to all prior employees of a company
\n\n \"ask-prior-candidates <company-name> ...\" - Ask a question to all people who have interviewed at a company
\n\n \"employee <company-name>\" - Register yourself as a current employee of a company
\n\n \"ex-employee <company-name>\" - Register yourself as a prior employee of a company
\n\n \"prior-candidate <company-name>\" - Register yourself as someone who has interviewed at a company
\n\n \"answer ...\" - Answer most recent question of a user
\n\n Note: You can choose to opt-out at any time if you choose to register yourself as a resource."))

;; Help message detailing the commands available to users
(defn helper[_]
  (str "Here are the available commands: \n\n
  \"ask-employees <company-name> ...\" - Ask a question to all current employees of a company
  \n\n \"ask-ex-employees <company-name> ...\" - Ask a question to all prior employees of a company
  \n\n \"ask-prior-candidates <company-name> ...\" - Ask a question to all people who have interviewed at a company
  \n\n \"employee <company-name>\" - Register yourself as a current employee of a company
  \n\n \"ex-employee <company-name>\" - Register yourself as a prior employee of a company
  \n\n \"prior-candidate <company-name>\" - Register yourself as someone who has interviewed at a company
  \n\n \"answer ...\" - Answer most recent question of a user
  \n\n \"optout-<employee/ex-employee/prior-candidate>\" - Opt-out of being a resource for a company"))


;; Incomplete code for displaying a list of current companies in database
(defn current-employees [_]
  (str "Here is a list of companies with current employees in our database: " (keys employee-companies)))

(defn current-ex-employees [_]
  (str "Here is a list of companies with prior employees in our database: " (keys ex-employee-companies)))

(defn current-prior-candidates [_]
  (str "Here is a list of companies with prior candidate in our database: " (keys prior-candidate-companies)))


;; Takes the current application `state`, a `topic`
;; the expert's `id` (e.g., unique name), and information
;; about the expert (`info`) and registers them as an expert on
;; the specified topic. Look at the associated test to see the
;; expected function signature.
;;
(defn employees-register [employees topic id info]
;;  (do (if (contains? employee-companies topic)
;;        (update employee-companies :topic inc)
;;        (assoc employee-companies :topic 1)
    (action-insert [employees topic id] info))

(defn ex-employees-register [ex-employees topic id info]
  (action-insert [ex-employees topic id] info))

(defn prior-candidates-register [prior-candidates topic id info]
  (action-insert [prior-candidates topic id] info))

;; Takes the current application `state`, a `topic`
;; and the expert's `id` (e.g., unique name) and then
;; removes the expert from the list of experts on that topic.
;; Look at the associated test to see the expected function signature.
;;
(defn employees-unregister [employees topic id]
  (action-remove [employees topic id]))

(defn ex-employees-unregister [ex-employees topic id]
  (action-remove [ex-employees topic id]))

(defn prior-candidates-unregister [prior-candidates topic id]
  (action-remove [prior-candidates topic id]))


;; Message shown when a user asks a question of employees or candidates
(defn employees-question-msg [employees question-words]
  (str "Asking " (count employees) " current employee(s) for an answer to: \""
       (string/join " " question-words) "\""))

(defn ex-employees-question-msg [ex-employees question-words]
  (str "Asking " (count ex-employees) " ex-employee(s) for an answer to: \""
       (string/join " " question-words) "\""))

(defn prior-candidates-question-msg [prior-candidates question-words]
  (str "Asking " (count prior-candidates) " prior candidate(s) for an answer to: \""
       (string/join " " question-words) "\""))


;; Function used to ask employees or candidates of companies questions
;;
(defn ask-employees [employees {:keys [args user-id]}]
  (if (empty? employees)
    [[] "There are no current employees from that company."]
    (if (empty? (rest args))
      [[] "You must ask a valid question. For help use the \"helper\" command."]
      [(into [] (concat (action-inserts [:conversations] employees user-id)
                        (action-send-msgs employees (string/join " " (rest args)))))
       (employees-question-msg employees (rest args))])))

(defn ask-ex-employees [ex-employees {:keys [args user-id]}]
  (if (empty? ex-employees)
    [[] "There are no prior employees from that company."]
    (if (empty? (rest args))
      [[] "You must ask a valid question. For help use the \"helper\" command."]
      [(into [] (concat (action-inserts [:conversations] ex-employees user-id)
                        (action-send-msgs ex-employees (string/join " " (rest args)))))
       (ex-employees-question-msg ex-employees (rest args))])))

(defn ask-prior-candidates [prior-candidates {:keys [args user-id]}]
  (if (empty? prior-candidates)
    [[] "There are no prior candidates from that company."]
    (if (empty? (rest args))
      [[] "You must ask a valid question. For help use the \"helper\" command."]
      [(into [] (concat (action-inserts [:conversations] prior-candidates user-id)
                        (action-send-msgs prior-candidates (string/join " " (rest args)))))
       (prior-candidates-question-msg prior-candidates (rest args))])))


;; Fucntion used to answer the question posed by a user
;;
(defn answer-question [conversation {:keys [args]}]
  (if (empty? args)
    [[] "You did not provide an answer."]
    (if (empty? conversation)
      [[] "You haven't been asked a question. For help use the \"helper\" command."]
      [(into [] (action-send-msgs [conversation] (string/join " " args)))
       (str "Your message was sent.")])))

;; Funtions used to add and remove employees and candidates from database
;;
(defn add-employee [employees {:keys [args user-id]}]
  (let [msg (str "You have been added as an employee at " (first args) ". To opt-out
in the future use the command \"optout-employee <company-name>\".")]
    [[(employees-register :employee (first args) user-id {})] msg]))

(defn remove-employee [employees {:keys [args user-id]}]
  (let [msg (str "You have been removed as an employee at " (first args) ".")]
    [[(employees-unregister :employee (first args) user-id)] msg]))


(defn add-ex-employee [ex-employees {:keys [args user-id]}]
  (let [msg (str "You have been added as an ex-employee at " (first args) ". To opt-out
in the future use the command \"optout-ex-employee <company-name>\".")]
    [[(ex-employees-register :ex-employee (first args) user-id {})] msg]))

(defn remove-ex-employee [ex-employees {:keys [args user-id]}]
  (let [msg (str "You have been removed as an ex employee at " (first args) ".")]
    [[(ex-employees-unregister :ex-employee (first args) user-id)] msg]))


(defn add-prior-candidate [prior-candidates {:keys [args user-id]}]
  (let [msg (str "You have been added as a prior-candidate at " (first args) ". To opt-out
in the future use the command \"optout-prior-candidate <company-name>\".")]
    [[(prior-candidates-register :prior-candidate (first args) user-id {})] msg]))

(defn remove-prior-candidate [prior-candidate {:keys [args user-id]}]
  (let [msg (str "You have been removed as a prior candidate at " (first args) ".")]
    [[(prior-candidates-unregister :prior-candidate (first args) user-id)] msg]))

;; Don't edit!
(defn stateless [f]
  (fn [_ & args]
    [[] (apply f args)]))


(def routes {"default"  (stateless (fn [& args] "Unknown command. For help use the \"helper\" command."))
             "current-employees" (stateless current-employees)
             "Hi" (stateless welcome)
             "hi" (stateless welcome)
             "yo" (stateless welcome)
             "Yo" (stateless welcome)
             "Hello" (stateless welcome)
             "hello" (stateless welcome)
             "helper" (stateless helper)
             "Helper" (stateless helper)
             "employee" add-employee
             "Employee" add-employee
             "optout-employee" remove-employee
             "Optout-employee" remove-employee
             "ask-employees" ask-employees
             "Ask-employees" ask-employees
             "ex-employee" add-ex-employee
             "Ex-employee" add-ex-employee
             "optout-ex-employee" remove-ex-employee
             "Optout-ex-employee" remove-ex-employee
             "ask-ex-employees" ask-ex-employees
             "Ask-ex-employees" ask-ex-employees
             "prior-candidate" add-prior-candidate
             "Prior-candidate" add-prior-candidate
             "optout-prior-candidate" remove-prior-candidate
             "Optout-prior-candidate" remove-prior-candidate
             "ask-prior-candidates" ask-prior-candidates
             "Ask-prior-candidates" ask-prior-candidates
             "answer" answer-question
             "Answer" answer-question})
;; Asgn 3.
;;
;; @Todo: Add mappings of the cmds "expert", "ask", and "answer" to
;; to the `routes` map so that the functions that you
;; created will be invoked when the corresponding text message
;; commands are received.
;;})


;; Don't edit!
(defn employees-on-topic-query [state-mgr pmsg]
  (let [[topic]  (:args pmsg)]
    (list! state-mgr [:employee topic])))

(defn ex-employees-on-topic-query [state-mgr pmsg]
  (let [[topic]  (:args pmsg)]
    (list! state-mgr [:ex-employee topic])))

(defn prior-candidates-on-topic-query [state-mgr pmsg]
  (let [[topic]  (:args pmsg)]
    (list! state-mgr [:prior-candidate topic])))

;; Don't edit!
(defn conversations-for-user-query [state-mgr pmsg]
  (let [user-id (:user-id pmsg)]
    (get! state-mgr [:conversations user-id])))


;; Don't edit!
(def queries
  {"employee" employees-on-topic-query
   "Employee" employees-on-topic-query
   "optout-employee" employees-on-topic-query
   "Optout-employee" employees-on-topic-query
   "ask-employees"    employees-on-topic-query
   "Ask-employees"    employees-on-topic-query
   "ex-employee" ex-employees-on-topic-query
   "Ex-employee" ex-employees-on-topic-query
   "optout-ex-employee" ex-employees-on-topic-query
   "Optout-ex-employee" ex-employees-on-topic-query
   "ask-ex-employees" ex-employees-on-topic-query
   "Ask-ex-employees" ex-employees-on-topic-query
   "prior-candidate" prior-candidates-on-topic-query
   "Prior-candidate" prior-candidates-on-topic-query
   "optout-prior-candidate" prior-candidates-on-topic-query
   "Optout-prior-candidate" prior-candidates-on-topic-query
   "ask-prior-candidates" prior-candidates-on-topic-query
   "Ask-prior-candidates" prior-candidates-on-topic-query
   "answer" conversations-for-user-query
   "Answer" conversations-for-user-query})


;; Don't edit!
(defn read-state [state-mgr pmsg]
  (go
    (if-let [qfn (get queries (:cmd pmsg))]
      (<! (qfn state-mgr pmsg))
      {})))


;; Asgn 1.
;;
;; @Todo: This function should return a function (<== pay attention to the
;; return type) that takes a parsed message as input and returns the
;; function in the `routes` map that is associated with a key matching
;; the `:cmd` in the parsed message. The returned function would return
;; `welcome` if invoked with `{:cmd "welcome"}`.
;;
;; Example:
;;
;; (let [msg {:cmd "welcome" :args ["bob"]}]
;;   (((create-router {"welcome" welcome}) msg) msg) => "Welcome bob"
;;
;; If there isn't a function in the routes map that is mapped to a
;; corresponding key for the command, you should return the function
;; mapped to the key "default".
;;
;; See the create-router-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn create-router [routes]
  (fn [pmsg]
    (if (contains? routes (get pmsg :cmd))
      (get routes (get pmsg :cmd))
      (get routes "default"))))

;; Don't edit!
(defn output [o]
  (second o))


;; Don't edit!
(defn actions [o]
  (first o))


;; Don't edit!
(defn invoke [{:keys [effect-handlers] :as system} e]
  (go
    (println "    Invoke:" e)
    (if-let [action (get effect-handlers (:action e))]
      (do
        (println "    Invoking:" action "with" e)
        (<! (action system e))))))


;; Don't edit!
(defn process-actions [system actions]
  (go
    (println "  Processing actions:" actions)
    (let [results (atom [])]
      (doseq [action actions]
        (let [result (<! (invoke system action))]
          (swap! results conj result)))
      @results)))


;; Don't edit!
(defn handle-message
  "
    This function orchestrates the processing of incoming messages
    and glues all of the pieces of the processing pipeline together.

    The basic flow to handle a message is as follows:

    1. Create the router that will be used later to find the
       function to handle the message
    2. Parse the message
    3. Load any saved state that is going to be needed to process
       the message (e.g., querying the list of experts, etc.)
    4. Find the function that can handle the message
    5. Call the handler function with the state from #3 and
       the message
    6. Run the different actions that the handler returned...these actions
       will be bound to different implementations depending on the environemnt
       (e.g., in test, the actions aren't going to send real text messages)
    7. Return the string response to the message

  "
  [{:keys [state-mgr] :as system} src msg]
  (go
    (println "=========================================")
    (println "  Processing:\"" msg "\" from" src)
    (let [rtr    (create-router routes)
          _      (println "  Router:" rtr)
          pmsg   (assoc (parsed-msg msg) :user-id src)
          _      (println "  Parsed msg:" pmsg)
          state  (<! (read-state state-mgr pmsg))
          _      (println "  Read state:" state)
          hdlr   (rtr pmsg)
          _      (println "  Hdlr:" hdlr)
          [as o] (hdlr state pmsg)
          _      (println "  Hdlr result:" [as o])
          arslt  (<! (process-actions system as))
          _      (println "  Action results:" arslt)]
      (println "=========================================")
      o)))
