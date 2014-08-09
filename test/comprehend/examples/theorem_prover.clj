(ns comprehend.examples.theorem-prover
  (require [comprehend :as c]
           [clojure.math.combinatorics :as combo]
           [clojure.test :refer :all]
           [clojure.pprint]))

; This file contains a theorem prover for propositional logic.
;
; To prove that a formula A is valid, the theorem prover uses  semantic/analytic
; tableaux to try to construct a model for ~A (not-A). If the model-construction
; effort is successful then A is not valid (and we have a counterexample). If
; unsuccessful then A is valid.
;
; It would be very doable to extend this system to modal propositional logics
; and even dynamic modal propositional logics. For more information, see my PhD
; thesis: http://jdevuyst.appspot.com/publications/2013/jdevuyst-phd-thesis.pdf

(defn apply-rules
  "Given a collection of tableaux `tabs`, apply-rules takes the first tableau
  `%` and applies the following algorithm:
  - If `%` is inconsistent (`%` contains A and ~A) then apply-rules returns
  `(rest coll)`
  - If `%` is consistent then apply-rules applies the tableau rules for
  conjunction and for double negation. That is, for every ~~A it adds A. For
  every conjunction A&B it adds A and it adds B. Call the new tableau `%`.
  - For every disjunction AvB it takes two copies of `%` and adds A to one copy
  and B to the other copy. If there are multiple disjunctions then it
  repeats this rule for all remaining disjunctions and for all copies made so
  far.
  Finally, apply-rules prepends all resulting tableaux to `(rest tabs)`.
  Note also that AvB is equivalent to ~(~A&~B)."
  [tabs]
  (concat (as-> (first tabs) %
                (if %
                  (c/rcomprehend :mark :consistent %
                                 x
                                 [:not x]
                                 (reduced nil)))
                (if %
                  (c/rcomprehend :mark :notnot [tab %]
                                 [:not [:not x]]
                                 (conj tab x)))
                (if %
                  (c/rcomprehend :mark :and [tab %]
                                 [:and x y]
                                 (into tab [x y])))
                (if %
                  (->> (c/comprehend :mark :or %
                                     [:not [:and x y]]
                                     [[:not x] [:not y]])
                       (apply combo/cartesian-product)
                       (map (partial into (c/mark % :or))))))
          (rest tabs)))

(defn tableau [& forms]
  (into (c/mark (c/indexed-set) :notnot :and :or)
        forms))

(defn find-model [form]
  (first ((c/fix apply-rules) [(tableau form)])))

(defn theorem? [form]
  (not (find-model [:not form])))

; HILBERT AXIOMS
; http://en.wikipedia.org/wiki/Hilbert_system

(defn implies [a b]
  [:not [:and a [:not b]]])

(defn p1 [a]
  (implies a a))

(defn p2 [a b]
  (implies a
           (implies b a)))

(defn p3 [a b c]
  (implies (implies a
                    (implies b c))
           (implies (implies a b)
                    (implies a c))))

(defn p4 [a b]
  (implies (implies [:not a] [:not b])
           (implies b a)))

(defn is-valid [name form]
  (testing (str name " is valid")
    (is (theorem? form)))
  (testing (str name " has a model")
    (is (find-model form))))

(deftest axioms
  (let [a :p
        b :q
        c :r]
    (testing "Hilbert axioms"
      (is-valid "P1" (p1 a))
      (is-valid "P2" (p2 a b))
      (is-valid "P3" (p3 a b c))
      (is-valid "P4" (p4 a b)))))