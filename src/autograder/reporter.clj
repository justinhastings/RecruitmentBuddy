;; ==================================================================================
;;
;;
;;          DO NOT EDIT - If you edit this file, it may prevent your assignment
;;          from being handed in or receiving proper credit. Extensions, score
;;          correction, etc. will not be considered due to problems resulting from
;;          edits to this file.
;;
;;
;; ===================================================================================



(ns autograder.reporter
  "A custom test reporter for clojure.test that grades assignments and submits results,
   along with source code to an autograding server."
  (:require [clojure.edn :as edn]
            [clojure.data :as data]
            [clojure.test :as test]
            [clj-http.client :as client]
            [clj-jgit.porcelain :as git]
            [clj-jgit.querying :as gitq]
            [clojure.pprint :as pp]
            [io.aviso.repl :as pretty-repl]
            [puget.color.ansi :as ansi]
            [ultra.test.diff :as diff]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [autograder.auth :as auth]))


;; ===============================
;;
;; Configuration
;;
;; ===============================



(def ^:const home (System/getProperty "user.home"))
(def ^:const autograder-home (str home "/.autograder"))

(def ^:const assignment-config-location (str "./assignment.edn"))
(def ^:const token-location (str autograder-home "/.tkn"))

;; User-specific data is stored in a config.edn file in the user's
;; ~/.autograder directory. This file needs to be created through
;; some other process before running any autograding tests.
;; (def user-config
;;   (try
;;     (edn/read-string (slurp user-config-location))
;;     (catch Exception e
;;            (println "The CS 4278 configuration utility has not been run!
;;                      Please run it or you will not be able to grade /
;;                      submit assignments.")
;;            (throw e))))

;; Each assignment has an assignment.edn file in the root directory that stores
;; important information for autograding, such as the assignment ID.
(def assignment-config
  (try
    (edn/read-string (slurp assignment-config-location))
    (catch Exception e
           (println "
                     ============================================================

                     ERROR!!!!

                     The repository is missing a valid assignment.edn file. Please
                     ensure that you cloned the repository correctly. You will be
                     unable to autograde your assignments correctly until this is
                     fixed.

                     ============================================================
                     ")
           (System/exit 1)
           (throw e))))

(def assignment-id (:id assignment-config))
(def assignment-repo (:repo assignment-config))
(def scoring (:scoring assignment-config))
(def course-id (:course-id assignment-config))
(def client-id (:client-id assignment-config))
(def target (:target assignment-config))
(def handin-patch (atom nil))

(def refresh-token
  (try
    (slurp token-location)
    (catch Exception e
       (if-let [token (:refresh-token (auth/authenticate client-id nil))]
          (do (io/make-parents token-location)
              (spit token-location (pr-str token))
              token)
          (do (println "Authentication failed.")
              (System/exit 1))))))

(def tokens (auth/authenticate client-id refresh-token))
(def id-token (:id-token tokens))
(def user (auth/get-user tokens))
(def id (:email user))
(def name (:name user))



 ;; (println "Authenticated as:"
 ;;       (-> (auth/get-user (:access-token (auth/authenticate client-id refresh-token))))))



;; ===============================
;;
;; Git Hand-in
;;
;; ===============================
;;
;; The autograder keeps a shadow copy of the assignment in a
;; git repo in the ~/.autograder directory. This shadow copy
;; allows it to commit the code each time the test suite is
;; run and create a patch file with any changes. The patch
;; files are sent to the server to hand-in the code. To
;; get the code back to the state of the local repository,
;; the server replays the patch file history against the
;; original assignment repo state.
;;

(defn date-time-now-str []
  (.format
    (java.time.format.DateTimeFormatter/ofPattern
     "MM-dd-yyyy-HH-mm-ss"
     java.util.Locale/ENGLISH)
    (java.time.LocalDateTime/now)))

(defn asgn-git-repo
  "Loads the shadow git repo in the user's ~/.autograder directory."
  []
  (let [path (str autograder-home "/" assignment-id "/")]
     (git/load-repo path)))

(defn asgn-handin-patch
  "Copies the latest version of the src / test directories and the project.clj to the git repo."
  [msg]
  (let [path (str autograder-home "/" assignment-id)
        prj  (str path "/project.clj")]
    (let [repo (asgn-git-repo)
          brch (date-time-now-str)]
      (git/git-checkout repo "master")
      (git/git-branch-create repo brch)
      (git/git-checkout repo brch)
      (fs/copy-dir "./src" path)
      (fs/copy-dir "./test" path)
      (fs/copy "./project.clj" prj)
      (git/git-add repo ".")
      (let [commit (git/git-commit repo msg)
            patch  (gitq/changed-files-with-patch repo commit)]
        patch))))

(defn ensure-git-repo-exists
  "Checks if the shadow git repo exists and creates it if it does not."
  []
  (let [path     (str autograder-home "/" assignment-id)
        histpath (str path "/.git")
        repo     (io/file path)]
    (when (not (.exists repo))
      (println "Initializing hand-in directory...")
      (io/make-parents path)
      ;(fs/copy-dir "./" path)
      (git/git-clone-full assignment-repo repo)
      ;(when (fs/exists? histpath) (fs/delete-dir histpath))
      ;(git/git-init path)
      (let [repo (asgn-git-repo)]
        (println repo)
        (git/git-add repo ".")
        (git/git-commit repo "Hand-in Init"))

      (println "Hand-in directory created."))))

;; ===============================
;;
;; Utility HTTP Request Fns
;;
;; ===============================

(defn get-json [uri]
  (:body (client/get uri {:as :json})))

(defn post-json [uri data]
  (client/post uri {:form-params  data
                    :throw-entire-message? true
                    :content-type :json}))

;; ===============================
;;
;; Console Printing Fns
;;
;; ===============================

(defn print-grading-msg []
  (println "
==================================================
             Grading Your Solution
=================================================")
  (println "Name: "  name))



;; Pulled out of https://github.com/venantius/ultra/blob/master/src/ultra/test.clj
(defn ultra-report-error
    [{:keys [type expected actual diffs message] :as event}]
    (test/with-test-out
      (test/inc-report-counter :fail)
      (println (str (ansi/sgr "\nFAIL" :red) " in " (test/testing-vars-str event)))
      (when (seq test/*testing-contexts*) (println (test/testing-contexts-str)))
      (when message (println message))
      (if (seq diffs)
        (doseq [[actual [a b]] diffs
                :when (or a b)]
          (diff/prn-diffs a b actual expected))
        (diff/print-expected actual expected))))


;; Pulled out of https://github.com/venantius/ultra/blob/master/src/ultra/test.clj
(defn ultra-report-fail
    [{:keys [message expected actual] :as event}]
    (test/with-test-out
      (test/inc-report-counter :error)
      (println (str (ansi/sgr "\nERROR" :red) " in " (test/testing-vars-str event)))
      (when (seq test/*testing-contexts*) (println (test/testing-contexts-str)))
      (when message (println message))
      (println "expected:" (pr-str expected))
      (print "  actual: ")
      (if (instance? Throwable actual)
        (pretty-repl/pretty-print-stack-trace actual)
        (prn actual))))



;; ===============================
;;
;; Processing of Test Msgs
;;
;; ===============================
;;
;; The autograder is built on top of clojure.test's custom reporting mechanism.
;; As tests run, the report function is passed messages describing the state/results
;; of the test run. These messages are used to calculate the score and create a
;; test report to send to the autograding server.


;; All of the test messages are buffered and then later sent to the
;; autograding server at the end of a test run.
(def buffer (atom []))

(defn submit-results
  "Sends the results and source patch for an autograding run to the autograding server."
  [student-id asgn-id results patch]
  (try
    ; (println [student-id id-token course-id asgn-id])
    ; (println results)
    ; (println patch)

    (post-json target {:student-id student-id
                       :id-token id-token
                       :course-id course-id
                       :assignment-id asgn-id
                       :results results
                       :patch patch})
    (catch Exception e
       (println e)
       (println "")
       (println "")
       (println "===========================================")
       (println "               WARNING                     ")
       (println "Unable to submit the assignment for grading.")
       (println "                                           ")
       (println "Please save the ENTIRE expcetion stack ")
       (println "trace above and include it in any emails")
       (println "to the instructor.")
       (println "===========================================")
       (println "")
       (println ""))))



(defn start-autograding
 "Called for each test report to see if a new autograding run has started.

  The function ensures that the shadow git repo exists, generates a hand-in
  patch for any source code changes, and prints the initial grading message
  that the user sees."
 [m]
 (when (and (empty? @buffer)
            (= (:type m) :begin-test-ns))
    ;(println "Refresh: " refresh-token)

    (println "Authenticated as:" user)

    (ensure-git-repo-exists)
    (reset! handin-patch (asgn-handin-patch "Hand-in Commit"))
    (print-grading-msg)))


(defn append-test-message
  "Processes an individual test report to make it suitable for sending to the server
   and buffers it.

   Individual test reports have values that cannot be marshalled into JSON. This
   function converts these values into strings so that they can be sent across
   the wire."
  [m]
  (let [diff   (data/diff (:expected m) (:actual m))
        diff-a (with-out-str (pp/pprint (first diff)))
        diff-b (with-out-str (pp/pprint (second diff)))]

    (cond
      (= :fail (:type m))           (ultra-report-fail m)
      (= :error (:type m))          (do (ultra-report-error m)
                                        (pretty-repl/pretty-print-stack-trace (:actual m)))
      (= :begin-test-var (:type m)) (println "Testing " (:var m))
      :default                      nil)

    (swap! buffer conj (-> m
                           (update :with-values #(with-out-str (pp/pprint %)))
                           (assoc  :diff-a diff-a)
                           (assoc  :diff-b diff-b)
                           (dissoc :diffs)
                           (update :ns str)
                           (update :expected #(with-out-str (pp/pprint %)))
                           (update :actual   #(with-out-str (pp/pprint %)))
                           (update :var str)))))



(defn reports-by-ns [reports]
  (second
   (reduce
    (fn [[currns byns] r]
      (let [crns (:ns r)
            rns (if (empty? crns) currns crns)]
        [rns (update byns rns conj r)]))
    [nil {}]
    reports)))

(defn percent-pass [reports]
  (let [passes (count (filter #(= (keyword (:type %)) :pass) reports))
        fails  (count (filter #(= (keyword (:type %)) :fail) reports))
        errors (count (filter #(= (keyword (:type %)) :error) reports))
        total  (+ passes fails)]
    (if (> total 0)
        (/ (- passes errors) total)
        0)))

(defn score [scoring reports]
  (let [byns (reports-by-ns reports)]
    (map (fn [[rns rs]] {:test   rns
                         :score  (Math/rint
                                  (* (percent-pass rs)
                                     (or (get scoring (keyword rns))
                                         (get scoring rns))))
                         :out-of (or (get scoring (keyword rns))
                                     (get scoring rns))})
         byns)))

(defn total-score [score]
  (reduce (fn [[sp tp] {:keys [score out-of]}] [(+ sp score) (+ tp out-of)])
          [0 0]
          score))


(defn end-autograding
  "At the end of an autograding run, this function submits the results and resets any state."
  [m]
  (when (= (:type m) :summary)
    (let [student-score (score scoring @buffer)
          [total possible] (total-score student-score)]
      ;(println @buffer)
      (println "================================================")
      (println "               Estimated Score:")
      (println "================================================")
      (pp/print-table student-score)
      (println)
      (println "Total: " (format "%.2f" (float total)) "/" possible)
      (println "================================================")
      (submit-results id assignment-id @buffer @handin-patch)
      (println "Score submitted.")
      (println)
      (println "Your actual score is calculated on the server and
may be different than this score in some circumstances.
The server score is considered the definitive score.")
      (reset! buffer []))))


;; ===============================
;;
;; Entry Point
;;
;; ===============================

(defn report
  "This function is a custom test reporter for clojure.test that receives test reports."
  [m]
  (start-autograding m)
  (append-test-message m)
  (end-autograding m))
