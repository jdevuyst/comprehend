(defproject comprehend "0.6.5-SNAPSHOT"
  :description "In-memory database modeled on sets, not tables. Comprehend supports pattern matching, forward matching, rewriting, and transactional storage."
  :url "https://github.com/jdevuyst/comprehend"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-ancient "0.5.5"]]
  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [org.clojure/core.cache "0.6.4"]
                 [fletching-macros "0.9"]]
  :global-vars {*assert* false}
  :profiles {:dev {:global-vars {*assert* true}
                   :dependencies [[org.clojure/core.logic "0.8.9"]
                                  [org.clojure/math.combinatorics "0.0.8"]
                                  [print-foo "1.0.1"]]
                   :injections [(require '[clojure.set :as set])
                                (require '[clojure.walk :as w])
                                (require '[clojure.core.reducers :as r])
                                (require '[comprehend.tools :as ct])
                                (require '[comprehend.engine :as ce])
                                (require '[comprehend :as c])
                                (require '[comprehend.tools-test :as ct-test])
                                (require '[comprehend.engine-test :as ce-test])
                                (require '[comprehend-test :as c-test])
                                (require '[comprehend.mutable-test :as cm-test])
                                (require '[comprehend.benchmark :as benchmark])
                                (require '[clojure.core.cache :as cache])
                                (require '[clojure.math.combinatorics :as combo])
                                (require '[print.foo :refer :all])]}})
