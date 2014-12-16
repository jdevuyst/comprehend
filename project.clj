(defproject comprehend "0.6-SNAPSHOT"
  :description "In-memory database modeled on sets, not tables. Comprehend supports pattern matching, forward matching, rewriting, and transactional storage."
  :url "https://github.com/jdevuyst/comprehend"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.cache "0.6.4"]]
  :global-vars {*assert* false}
  :profiles {:dev {:global-vars {*assert* true}
                   :dependencies [[org.clojure/math.combinatorics "0.0.7"]
                                  [org.clojure/core.logic "0.8.8"]
                                  [org.clojure/tools.trace "0.7.8"]]
                   :injections [(require '[clojure.set :as set])
                                (require '[clojure.walk :as w])
                                (require '[comprehend-test :as ct])
                                ; (require '[comprehend.mutable-test :as cmt])
                                (require '[comprehend.engine-test :as cet])
                                (require '[comprehend.benchmark :as benchmark])
                                (require '[clojure.tools.trace :refer [deftrace trace trace-ns]])]}})