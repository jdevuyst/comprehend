(defproject comprehend "0.5.1-SNAPSHOT"
  :description "In-memory database modeled on sets, not tables. Comprehend supports pattern matching, forward matching, rewriting, and transactional storage."
  :url "https://github.com/jdevuyst/comprehend"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.logic "0.8.8"]]
  :profiles {:dev {:dependencies [[org.clojure/math.combinatorics "0.0.7"]]}})
