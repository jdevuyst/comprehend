(ns comprehend.benchmark
  (:require [comprehend.core :as c]
            [clojure.core.logic :as l]))

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

(println "Constructing a graph")

(def n 150)
(def G (time (vec (make-graph n n))))

(println "Constructing an equivalent indexed set")

(def S (time (reduce conj (c/indexed-set) G)))

(println "Finding paths using comprehend")

(def v1 (time (vec (c/comprehend
                     S
                     (edge a b) (edge b c) (edge c d) (edge d e)
                     [a b c d e]))))

(println "Finding paths using unification without indexes")

(def v2 (time (vec (l/run* [a b c d e]
                           (l/membero (edge a b) G)
                           (l/membero (edge b c) G)
                           (l/membero (edge c d) G)
                           (l/membero (edge d e) G)))))

(print "Sanity checks... ")

(assert (= (* 2 n) (count G) (count S)))
(assert (= (count v1) (count v2)))
(assert (> (count v1) 0) "Try generating more edges")

(println "OK")