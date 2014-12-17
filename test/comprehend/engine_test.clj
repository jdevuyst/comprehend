(ns comprehend.engine-test
  (:require [clojure.test :refer :all]
            [comprehend.engine :refer :all]
            [comprehend.tools :as ctools]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [deftrace trace trace-ns]]))

(def VarType comprehend.engine.Var)

(deftest all-tests
  (ctools/with-cache-atom
    (atom (ctools/soft-cache))

    (testing "grounded?"
      (is (grounded? 1))
      (is (not (grounded? (variable 'x))))
      (is (grounded? [1 2 [3]]))
      (is (not (grounded? [1 2 [(variable 'x)]]))))

    (let [x (variable 'x)
          y (variable 'y)
          z (variable 'z)]
      (testing "generalize"
        (is (= VarType (-> [1 2 3 4]
                           generalize
                           :query
                           type)))
        (is (-> [1 2 3 x]
                generalize
                :query
                vector?))
        (is (= (->> [1 2 3 x]
                    generalize
                    :query
                    (map type))
               (repeat 4 VarType)))
        (is (= (->> [1 2 [] [4 {:a :b}] #{{[x] [y]}}]
                    generalize
                    :query
                    (w/postwalk #(if (coll? %)
                                   %
                                   (type %))))
               [VarType VarType VarType VarType #{{[VarType] [VarType]}}]))
        (is (= (-> [1 x] generalize :query)
               (-> [2 x] generalize :query)))
        (is (= (-> [1 2 3 x] generalize :query)
               (-> [1 2 3 x] generalize :query generalize :query)))
        (is (= (-> [1 2 3 x] generalize :query)
               (-> [4 5 6 x] generalize :query generalize :query)))
        (is (= (-> [1 2 [] [#{4} {:a :b}] #{[x] [y]}]
                   generalize
                   :query)
               (-> [1 2 [] [:a-keyword] #{[x] [y]}]
                   generalize
                   :query
                   generalize
                   :query))))

      (testing "equivalance of variables"
        (is (= x x))
        (is (not= x y))
        (is (= (variable 'test) (variable 'test))))

      (testing "unify-colls"
        (is (= (set (unify #{x y :lit} #{1 2 3}))
               #{[:dom x #{1 2 3}]
                 [:dom y #{1 2 3}]
                 [:dom :lit #{1 2 3}]})))

      (testing "unify-sequentials"
        (is (= (set (unify [x y] [1 2]))
               #{[:dom x #{1}]
                 [:dom y #{2}]}))
        (is (= (set (unify [1 2] [1 2]))
               #{[:dom 1 #{1}] [:dom 2 #{2}]}))
        (is (= (set (unify [1 2] [2 1]))
               #{[:dom 2 #{1}] [:dom 1 #{2}]})))

      (testing "unify-maps"
        (is (= (->> (unify-maps {1 x 3 y} {1 2 3 4})
                    (map (fn [[t x dom]] [t x (set dom)]))
                    set)
               #{[:dom [1 x] #{[1 2] [3 4]}]
                 [:dom [3 y] #{[1 2] [3 4]}]})))

      (testing "mixed"
        (is (= (-> (unify [x y] [2 1])
                   set
                   (conj [:dom 0 #{0}]))
               (->> (unify-maps {0 [[x y]]} {0 [[2 1]]})
                    (t-transform decompose-dom-terms)
                    set))))

      (testing "simplify-domains"
        (is (= (->> (unify-colls #{[:a x]} #{[:a 1] [:b 2] [:a 4]})
                    (t-transform simplify-domains)
                    (map (fn [[x y z]] [x y (set z)])))
               [[:dom [:a x] #{[:a 1] [:a 4]}]])))

      (testing "sequences of triples to maps"
        (is (= (->> (list [:dom x [1 2 3]] [:dom x [2 3 4 5]]
                          [:dom y #{6 7 8 9}]
                          [:dom z #{1}])
                    triples-to-map)
               {:dom {x #{3 2}
                      y #{6 7 8 9}
                      z #{1}}})))

      (testing "maps to sequences of triples"
        (is (= (->> (list [:dom x [1 2 3]] [:dom x [2 3 4 5]]
                          [:dom y #{6 7 8 9}]
                          [:dom z #{1}])
                    triples-to-map
                    map-to-triples
                    set)
               #{[:dom x #{2 3}]
                 [:dom y #{6 7 8 9}]
                 [:dom z #{1}]})))

      (testing "constraint-map?"
        (is (-> (list [:dom x [1 2 3]] [:dom x [2 3 4 5]]
                      [:dom y #{6 7 8 9}]
                      [:dom z #{1}])
                triples-to-map
                constraint-map?))
        (is (-> (list [:dom x [1 2 3]] [:dom x [2 3 4 5]]
                      [:dom y #{6 7 8 9}]
                      [:dom z #{1}])
                constraint-map?
                not)))

      (testing "develop"
        (is (= (-> (list [:dom [x {y [x z]}] #{[1 {2 [1 3] :a :b}]}]
                         [:dom {100 y} #{{100 2}}])
                   triples-to-map
                   develop
                   :val)
               {y 2, x 1, z 3}))
        (is (-> (list [:dom [x {y [x z]}] #{[1 {2 [1 3] :a :b}]}]
                      [:dom {100 y} #{{100 2}}]
                      [:dom {3 x} #{{3 4}}])
                triples-to-map
                develop
                :val
                empty?)))

      (testing "quantify1"
        (let [x-dom #{1 2 3}
              m {:dom {x x-dom
                       y #{4 5 6 7}
                       z #{8}}}]
          (is (= (count x-dom) (count (quantify1 m))))
          (is (= x-dom (->> (quantify1 m)
                            (map develop1)
                            (map :val)
                            (map #(get % x))
                            set))))
        (is (= (-> {:dom {x #{2}
                          y #{4}}}
                   quantify1)
               nil)))

      (testing "develop-all"
        (is (= (->> (list [:dom [x {y [x z]}] #{[1 {2 [1 3] :a :b}]}]
                          [:dom {100 y} #{{100 2}}])
                    list
                    develop-all
                    (map triples-to-map)
                    (map :val))
               [{x 1 y 2 z 3}])))

      (testing "find-models"
        (is (= (find-models {x 2 y z} {1 2 3 4 5 6})
               #{{x 1, y 3, z 4}
                 {x 1, y 5, z 6}
                 {x 1, y 1, z 2}}))
        (is (= (find-models #{[x y] [y x]}
                            #{[1 2] [2 1] [3 4] [4 5] [5 3] [6 6]})
               #{{x 1 y 2}
                 {x 2 y 1}
                 {x 6 y 6}}))
        (is (= (find-models #{{x #{y} y #{z}}}
                            #{{1 #{2 4}
                               2 #{5 6}
                               3 #{7 8}
                               9 #{0}}})
               #{{x 1 y 2 z 5}
                 {x 1 y 2 z 6}})))

      (testing "match patterns in a set"
        (is (= (set (match-in [:a x y] #{[:a 1 2] [:a 2 3] [:b 3 4]}))
               #{[:a 1 2] [:a 2 3]}))))))

(let [this-ns-name (ns-name *ns*)]
  (defn reload-and-test []
    (require [this-ns-name] :reload-all)
    (run-tests this-ns-name)))