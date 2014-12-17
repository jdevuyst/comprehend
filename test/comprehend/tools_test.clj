(ns comprehend.tools-test
  (:require [clojure.test :refer :all]
            [comprehend.tools :as ct]
            [clojure.tools.trace :refer [deftrace trace trace-ns]]))

(def VarType comprehend.engine.Var)

(deftest all-tests
  (testing "fixpoints"
    (is (= (ct/fixpoint [x 1]
                        (if (>= x 1000)
                          x
                          (* 2 x)))
           1024))
    (is (= ((ct/fix #(min 10 (inc %))) 0)
           10)))
  (testing "subst"
    (is (= (ct/subst {:x 49 :y 50}
                     [1 2 {:x 3 4 [:y]}])
           [1 2 {49 3, 4 [50]}]))))

(let [this-ns-name (ns-name *ns*)]
  (defn reload-and-test []
    (require [this-ns-name] :reload-all)
    (run-tests this-ns-name)))