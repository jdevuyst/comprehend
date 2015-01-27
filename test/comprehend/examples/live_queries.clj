; The tests in this file are based on the following blog post:
; https://jdevuyst.blogspot.com/2015/01/comprehend-live-queries.html

(ns comprehend.examples.live-queries
  (:require [clojure.test :refer :all]
            [comprehend :as c]
            [comprehend.mutable :as cm]))

(def !acc-results (atom nil))

(defn report-new-matches [m-s]
  "Prints new paths of length 3 since report-new-matches was last
  called on m-s. Prints zero matches on the first invocation."
  (let [!results (volatile! nil)]
    (dosync
      (vreset! !results (c/comprehend :mark :report-new-matches
                                      @m-s
                                      [a b]
                                      [b c]
                                      [a b c]))
      (cm/mark m-s :report-new-matches))
    (swap! !acc-results into @!results)))

(declare m-s)
(defn live-reporter
  ([] nil)
  ([s diff] (report-new-matches m-s)))

(deftest live-query-test
  (def m-s (cm/mutable-indexed-set live-reporter))
  (report-new-matches m-s) ; init

  (are [x y] (= (do
                  (reset! !acc-results #{})
                  x
                  (cm/flush m-s)
                  @!acc-results)
                y)
       (reduce cm/conj m-s [[1 2] [2 3] [1 3] [3 4]])
       #{[1 3 4] [1 2 3] [2 3 4]}

       (cm/conj m-s [0 1])
       #{[0 1 3] [0 1 2]}))