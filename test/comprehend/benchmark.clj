(ns comprehend.benchmark
  (:require [comprehend :as c]
            [clojure.core.logic :as l]
            [clojure.set :as set]
            [clojure.test :refer :all]))

; This file benchmarks several solutions to a single problem.
;
; THE PROBLEM
; Given a graph of N vertices and N random edges, find all paths of length 4.
;
; APPROACHES
; (1) Comprehend
; (2) Core.logic with membero
; (3) Naive algorithm
;
; RESULTS
; Comprehend was found to be always faster than (2).
; Comprehend was found to be faster than (3) for N > 30.
; (2) was found to be faster than (3) for N > 40.
;
; The old PLDB-based implementation was faster than (2) and (3) for N > 50.
;
; SEE ALSO
; http://jdevuyst.blogspot.com/2014/05/comprehend-clojure-pattern-matching.html
;
; TO DO
; - Add a hand-optimized approach
; - Benchmark different problems

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
    (let [s1 (set @!v1)
          s2 (set @!v2)]
      (is (= s2 (set @!v3)))
      (is (= s1 s2))
      (when-not (= s1 s2)
        (println "s1 - s2: " (set/difference s1 s2))
        (println "s2 - s1: " (set/difference s2 s1))))
    (is (> (count @!v1) 0) "Try generating more edges")))

(let [this-ns-name (ns-name *ns*)]
  (defn reload []
    (require [this-ns-name] :reload-all)))

(def N 50)

(deftest benchmark
  (testing "Benchmark"
    (let [prev-assert-val *assert*]
      (println "Disabling assertions...")
      (set! *assert* false)
      (reload)

      (run-benchmark N)

      (when prev-assert-val
        (set! *assert* prev-assert-val)
        (println "Re-enabling assertions...")
        (reload)))))