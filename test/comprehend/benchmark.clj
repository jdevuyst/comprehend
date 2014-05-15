(ns comprehend.benchmark
  (:require [comprehend.core :as c]
            [clojure.core.logic :as l]))

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
        (let [e [(random-node) (random-node)]]
          (if (@!edges e)
            (recur)
            (swap! !edges conj! e)))))
    (concat (map vector nodes)
            (persistent! @!edges))))

(println "Constructing graph")

(def n 150)
(def G (time (vec (make-graph n n))))

(println "Constructing indexed set")

(def S (reduce conj (c/indexed-set) G))

(println "Finding paths using comprehend")

(def v1 (time (vec (c/comprehend S
                                 [a b] [b c] [c d] [d e]
                                 [a b c d e]))))

(println "Finding paths using unification without indexes")

(def v2 (time (vec (l/run* [a b c d e]
                           (l/membero [a b] G)
                           (l/membero [b c] G)
                           (l/membero [c d] G)
                           (l/membero [d e] G)))))

(println "Sanity check")

(assert (= (count v1) (count v2)))