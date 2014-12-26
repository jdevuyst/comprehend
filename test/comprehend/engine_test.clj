(ns comprehend.engine-test
  (:require [clojure.test :refer :all]
            [comprehend.tools :as ct]
            [comprehend.engine :refer :all]
            [clojure.walk :as w]
            [clojure.core.cache :as cache]))

(def VarType comprehend.engine.Var)

(def !cache (atom (cache/soft-cache-factory {})))

(deftest all-tests
  (testing "grounded?"
    (is (grounded? 1))
    (is (not (grounded? (variable 'x))))
    (is (grounded? [1 2 [3]]))
    (is (not (grounded? [1 2 [(variable 'x)]]))))

  (let [x (variable 'x)
        y (variable 'y)
        z (variable 'z)]
    (testing "equivalance of variables"
      (is (= x x))
      (is (not= x y))
      (is (= (variable 'test) (variable 'test))))

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

    (testing "unification-type"
      (is (= (unification-type {1 2 3 4}) :map))
      (is (= (unification-type [1 2 3 4])
             (unification-type '(1 2 3 4))
             (unification-type (map identity {1 2 3 4 5 6 7 8}))
             [:list-like 4]))
      (is (= (unification-type #{1 2 3 4}) :set-like))
      (is (= (unification-type nil)
             (unification-type 1)
             :not-a-coll)))

    (testing "unify colls"
      (is (= (set (unify {} #{x y :lit} #{1 2 3}))
             #{[x #{1 2 3}]
               [y #{1 2 3}]
               [:lit #{1 2 3}]})))

    (testing "unify sequentials"
      (is (= (set (unify {} [x y] [1 2]))
             #{[x #{1}]
               [y #{2}]}))
      (is (= (set (unify {} [1 2] [1 2]))
             #{[1 #{1}] [2 #{2}]}))
      (is (= (set (unify {} [1 2] [2 1]))
             #{[2 #{1}] [1 #{2}]})))

    (testing "unify maps"
      (is (= (->> (unify {} {1 x 3 y} {1 2 3 4})
                  (map (fn [[x dom]] [x (set dom)]))
                  set)
             #{[[1 x] #{[1 2] [3 4]}]
               [[3 y] #{[1 2] [3 4]}]})))

    (testing "mixed"
      (is (= (-> (unify {} [x y] [2 1])
                 set
                 (conj [0 #{0}]))
             (->> (unify {} {0 [[x y]]} {0 [[2 1]]})
                  (mapcat decompose-dom-terms)
                  set))))

    (testing "simplify-domains"
      (is (= (->> (unify {} #{[:a x]} #{[:a 1] [:b 2] [:a 4]})
                  (mapcat (partial simplify-domains !cache {}))
                  (map (fn [[x dom]] [x (set dom)])))
             [[[:a x] #{[:a 1] [:a 4]}]])))

    (testing "sequences of triples to maps"
      (is (= (->> (list [x [1 2 3]] [x [2 3 4 5]]
                        [y #{6 7 8 9}]
                        [z #{1}])
                  constraints-as-mmap)
             {x #{3 2}
              y #{6 7 8 9}
              z #{1}})))

    (testing "develop"
      (is (= (->> (list [[x {y [x z]}] #{[1 {2 [1 3] :a :b}]}]
                        [{100 y} #{{100 2}}])
                  (develop1 !cache) ; without substitution of known values in keys, this test would require quantification
                  (develop1 !cache)
                  (develop1 !cache)
                  constraints-as-mmap)
             {y #{2}, x #{1}, z #{3}}))
      (is (->> (list [[x {y [x z]}] #{[1 {2 [1 3] :a :b}]}]
                     [{100 y} #{{100 2}}]
                     [{3 x} #{{3 4}}])
               (develop1 !cache)
               (develop1 !cache)
               empty?)))

    (testing "quantify1"
      (let [x-dom #{1 2 3}
            m {x x-dom
               y #{4 5 6 7}
               z #{8}}]
        (is (= (count x-dom) (count (quantify1 m))))
        (is (= x-dom (->> (quantify1 m)
                          (map (partial develop1 !cache))
                          (map #(get % x))
                          (map first)
                          set))))
      (is (= (-> {x #{2} y #{4}}
                 quantify1
                 set)
             #{{x #{2} y #{4}}})))

    (testing "develop-all"
      (is (= (->> (list [[x {y [x z]}] #{[1 {2 [1 3] :a :b}]}]
                        [{100 y} #{{100 2}}])
                  list
                  (develop-all !cache)
                  (map constraints-as-mmap))
             [{x #{1} y #{2} z #{3}}])))

    (testing "find models"
      (is (= (->> (match-in !cache {x 2 y z} #{{1 2 3 4 5 6}})
                  (map constraints-as-model)
                  set)
             #{{x 1, y 3, z 4}
               {x 1, y 5, z 6}
               {x 1, y 1, z 2}}))
      (is (= (->> (match-with !cache
                              [[x y] [y x]]
                              #{[1 2] [2 1] [3 4] [4 5] [5 3] [6 6]})
                  (map constraints-as-model)
                  set)
             #{{x 1 y 2}
               {x 2 y 1}
               {x 6 y 6}}))
      (is (= (->> (match-with !cache
                              [{x #{y} y #{z}}]
                              #{{1 #{2 4}
                                 2 #{5 6}
                                 3 #{7 8}
                                 9 #{0}}})
                  (map constraints-as-model)
                  set)
             #{{x 1 y 2 z 5}
               {x 1 y 2 z 6}})))))

(let [this-ns-name (ns-name *ns*)]
  (defn reload-and-test []
    (require [this-ns-name] :reload-all)
    (run-tests this-ns-name)))