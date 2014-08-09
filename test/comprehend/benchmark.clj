(ns comprehend.benchmark
  (:require [comprehend :as c]
            [clojure.core.logic :as l]
            [clojure.test :refer :all]))

; This file benchmarks several solutions to a single problem.
;
; THE PROBLEM
; Given a graph of N vertices and N random edges, find all paths of length 4.
;
; APPROACHES
; - Comprehend
; - Core.logic with membero
; - Naive algorithm
;
; RESULTS
; Comprehend was found to be faster for N > 50.
;
; SEE ALSO
; http://jdevuyst.blogspot.com/2014/05/comprehend-clojure-pattern-matching.html

(defn edge [source dest]
  [source dest]
  ; [[[[[[[source dest]]]]]]]
  )

(defn make-graph
  "Creates a collection of tuples of two arities. The singles
  represent the nodes. The pairs represent eges."
  [node-count edge-count]
  (let [nodes (->> (repeatedly gensym)
                   (take node-count)
                   vec)
        random-node #(nodes (int (rand node-count)))
        !edges (atom (transient #{}))]
    (doseq [i (range edge-count)]
      (loop []
        (let [e (edge (random-node) (random-node))]
          (if (@!edges e)
            (recur)
            (swap! !edges conj! e)))))
    (concat (map vector nodes)
            (persistent! @!edges))))


(defn run-benchmark [n]
  (let [!G (atom nil)
        !S (atom nil)
        !v1 (atom nil)
        !v2 (atom nil)
        !v3 (atom nil)]
    (print (str "Constructing a graph with N=" n "... "))

    (reset! !G (time (vec (make-graph n n))))

    (print "Constructing an equivalent indexed set... ")

    (reset! !S (time (reduce conj (c/indexed-set) @!G)))

    (print "Finding paths using comprehend... ")

    (reset! !v1 (time (vec (c/comprehend
                             @!S
                             (edge a b) (edge b c) (edge c d) (edge d e)
                             [a b c d e]))))

    (print "Finding paths using core.logic's membero and unification without indexes... ")

    (reset! !v2 (time (vec (l/run* [a b c d e]
                                   (l/membero (edge a b) @!G)
                                   (l/membero (edge b c) @!G)
                                   (l/membero (edge c d) @!G)
                                   (l/membero (edge d e) @!G)))))

    (print "Finding paths using for, filter, and argument destructuring... ")

    (reset! !v3 (time (let [E (vec (filter #(= 2 (count %)) @!G))]
                        (vec (->> (for [e1 E e2 E e3 E e4 E]
                                    [e1 e2 e3 e4])
                                  (filter (fn [[[v1 v2a] [v2b v3a] [v3b v4a] [v4b v5]]]
                                            (and (= v2a v2b)
                                                 (= v3a v3b)
                                                 (= v4a v4b))))
                                  (map (fn [[[v1 v2a] [v2b v3a] [v3b v4a] [v4b v5]]]
                                         [v1 v2a v3a v4a v5])))))))

    (is (= (* 2 n) (count @!G) (count @!S)))
    (is (= (count @!v1) (count @!v2) (count @!v3)))
    (is (= (set @!v1) (set @!v2) (set @!v3)))
    (is (> (count @!v1) 0) "Try generating more edges")))

(deftest benchmark
  (testing "Benchmark"
    (run-benchmark 50)))