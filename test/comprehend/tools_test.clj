(ns comprehend.engine-test
  (:require [comprehend.tools :refer :all]
            [clojure.tools.trace :refer [deftrace trace trace-ns]]))

(def VarType comprehend.engine.Var)

(deftest all-tests
  (ctools/with-cache-atom
    (atom (ctools/soft-cache))

    (testing "reverse-map"
      (is (= (->> (reverse-map {1 2 3 4 5 0 6 0})
                  (map (fn [[k v]] [k (set v)]))
                  (into {}))
             {2 #{1}
              4 #{3}
              0 #{5 6}})))
    (testing "fixpoints"
      (is (= (fixpoint [x 1]
                              (if (>= x 1000)
                                x
                                (* 2 x)))
             1024))
      (is (= ((fix #(min 10 (inc %))) 0)
             10)))
    (testing "subst"
        (is (= (subst {x 49 y 50}
                      [1 2 {x 3 4 [y]}])
               [1 2 {49 3, 4 [50]}])))))

(let [this-ns-name (ns-name *ns*)]
  (defn reload-and-test []
    (require [this-ns-name] :reload-all)
    (run-tests this-ns-name)))