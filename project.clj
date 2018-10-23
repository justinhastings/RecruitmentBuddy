(defproject asgn1 "0.1.0-SNAPSHOT"
            :description "CS4278"
            :url "http://magnum.io"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.9.0"]
                           [org.clojure/clojurescript "1.10.339"
                            :scope "provided"]
                           [thheller/shadow-cljs "2.4.33"]
                           [cljsjs/aws-sdk-js "2.94.0-0"]
                           [clj-http "3.9.1"]
                           [io.nervous/cljs-lambda "0.3.5"
                            :exclusions [org.clojure/tools.reader
                                         org.clojure/clojurescript
                                         org.clojure/clojure
                                         org.clojure/core.async]]]

            ;; Serverless
            :cljs-lambda {:cljs-build-id "lambda"
                          :resource-dirs ["resources"]}
            :npm {:dependencies [[serverless-cljs-plugin "0.1.2"]
                                 [ws "6.0.0"]
                                 [twilio "3.19.1"]
                                 [serverless-secrets "3.0.0-beta.12"]
                                 [source-map-support "0.5.7"]
                                 [aws-sdk "2.289.0"]
                                 [bignumber.js "7.2.1"]]}

            :source-paths ["src"]
            :test-paths ["test"]

            :plugins [[venantius/ultra "0.5.2"]
                      [lein-npm       "0.6.2"]
                      [lein-cljsbuild "1.1.7"]
                      [io.nervous/lein-cljs-lambda "0.6.6"]
                      [com.jakemccrary/lein-test-refresh "0.22.0"]]

            :test-refresh {:refresh-dirs ["src" "test"]
                           :report       autograder.reporter/report}

            :target-path "target/%s"

            :aliases {"node-repl" ["run" "-m" "shadow.cljs.devtools.cli" "watch" ":app"]}

            :profiles {:uberjar {:aot :all}
                       :dev {:dependencies [[binaryage/devtools "0.9.10"]
                                            [org.clojure/tools.nrepl "0.2.13"]
                                            [cheshire "5.8.0"]
                                            [clj-http "3.9.0"]
                                            [clj-jgit "0.8.10"]
                                            [com.cognitect/transit-clj "0.8.309"]
                                            [me.raynes/fs "1.4.6"]
                                            [proto-repl "0.3.1"]
                                            [org.clojure/test.check "0.9.0"]
                                            [com.cemerick/piggieback "0.2.2"]
                                            [me.raynes/conch "0.8.0"]
                                            [pjstadig/humane-test-output "0.8.3"]]

                             :injections [(require 'pjstadig.humane-test-output)
                                          (pjstadig.humane-test-output/activate!)]

                             :env {:dev true}}}

            :main asgnx.cli 

            :cljsbuild
            {:builds {:lambda
                      {:source-paths ["src"]
                       :compiler {:output-to     "target/cljsbuild/lambda.js"
                                  :output-dir    "target/cljsbuild"
                                  :main          "asgnx.lambda"
                                  :source-map    true
                                  :target        :nodejs
                                  :language-in   :ecmascript5
                                  :optimizations :none}}}})
