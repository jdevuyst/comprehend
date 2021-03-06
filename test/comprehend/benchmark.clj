(ns comprehend.benchmark
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [comprehend :as c]
            [clojure.core.cache :as cache]
            [clojure.core.logic :as l]))

; This file benchmarks several solutions to a single problem.
;
; THE PROBLEM
; Given a graph of N vertices and N random edges, find all paths of length 4.
;
; APPROACHES
; (1) Comprehend, using a set of pairs
; (2) Comprehend, using a map (in an indexed set)
; (3) Core.logic with membero and unification without indexes
; (4) Naive algorithm
;
; RESULTS
; Comprehend was found to be always faster than (3).
; Comprehend was found to be faster than (4) for N > 17 on a dual core laptop.
; (3) was found to be faster than (4) for N > 35.
;
; In old PLDB-based implementation (1) was faster than (3) and (4) for N > 50.
;
; SEE ALSO
; - http://jdevuyst.blogspot.com/2015/01/comprehend-new-engine.html
; - http://jdevuyst.blogspot.com/2014/05/comprehend-clojure-pattern-matching.html
;
; TO DO
; - Add a hand-optimized approach
; - Benchmark different problems

(defn edge [source dest]
  [source dest])

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
    (persistent! @!edges)))

(defn run-benchmark [n]
  (testing (str "Benchmark with N=" n)
    (print (str "Setting up benchmark with N=" n "... "))
    (flush)

    (let [!G (atom nil)
          !S (atom nil)
          !M (atom nil)
          !v1 (atom nil)
          !v2 (atom nil)
          !v3 (atom nil)
          !v4 (atom nil)]
      (time (do
              (reset! !G (vec (make-graph n n)))

              (reset! !S (reduce conj (c/indexed-set) @!G))

              (reset! !M (c/indexed-set (reduce (fn [m [a b :as v]]
                                                  (when (= 2 (count v))
                                                    (assoc m a (conj (get m a #{})
                                                                     b))))
                                                {}
                                                @!G)))

              (swap! !S c/index (cache/basic-cache-factory {}))
              (swap! !M c/index (cache/basic-cache-factory {}))))

      (print "Finding paths using comprehend and the set of pairs... ")
      (flush)

      (reset! !v1 (time (vec (c/comprehend
                               @!S
                               (edge a b) (edge b c) (edge c d) (edge d e)
                               [a b c d e]))))

      (print "Finding paths using comprehend and the map in the set... ")
      (flush)

      (reset! !v2 (time (vec (c/comprehend
                               @!M
                               {a #{b} b #{c} c #{d} d #{e}}
                               [a b c d e]))))

      (print "Finding paths using core.logic's membero and unification without indexes... ")
      (flush)

      (reset! !v3 (time (vec (l/run* [a b c d e]
                                     (l/membero (edge a b) @!G)
                                     (l/membero (edge b c) @!G)
                                     (l/membero (edge c d) @!G)
                                     (l/membero (edge d e) @!G)))))

      (print "Finding paths using for, filter, and argument destructuring... ")
      (flush)

      (reset! !v4 (time (let [E (vec (filter #(= 2 (count %)) @!G))]
                          (vec (->> (for [e1 E e2 E e3 E e4 E]
                                      [e1 e2 e3 e4])
                                    (filter (fn [[[v1 v2a] [v2b v3a] [v3b v4a] [v4b v5]]]
                                              (and (= v2a v2b)
                                                   (= v3a v3b)
                                                   (= v4a v4b))))
                                    (map (fn [[[v1 v2a] [v2b v3a] [v3b v4a] [v4b v5]]]
                                           [v1 v2a v3a v4a v5])))))))

      (is (= 1 (->> @!M (filter map?) count)))
      (is (= n
             (->> @!G
                  (map count)
                  (filter (partial = 2))
                  count)
             (->> @!S
                  (map count)
                  (filter (partial = 2))
                  count)
             (->> @!M
                  (filter map?)
                  first
                  vals
                  (map count)
                  (reduce + 0))))
      (is (= (count @!v1) (count @!v2) (count @!v3) (count @!v4)))
      (let [s1 (set @!v1)
            s2 (set @!v2)
            s3 (set @!v3)]
        (is (= s3 (set @!v4)))
        (is (= s1 s3))
        (is (= s2 s3))
        (when-not (= s1 s3)
          (println "s1 - s3: " (set/difference s1 s3))
          (println "s3 - s1: " (set/difference s3 s1)))
        (when-not (= s2 s3)
          (println "s2 - s3: " (set/difference s2 s3))
          (println "s3 - s2: " (set/difference s3 s2))))
      (is (pos? (count @!v1)) "Try generating more edges"))))

(def ^:dynamic *N* (+ 13 (int (rand 10))))

(deftest benchmark-test (run-benchmark *N*))

(let [this-ns-name (ns-name *ns*)]
  (defn reload []
    (require [this-ns-name] :reload-all))

  (defn benchmark
    ([] (benchmark 5 10 15 20 25))
    ([& ns]
     (doseq [n ns]
       (binding [*N* n]
         (run-tests this-ns-name))))))