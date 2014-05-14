(ns comprehend.core-test
  (:require [clojure.test :refer :all]
            [comprehend.core :refer :all]))

(ugly-fix-for-sets)

(deftest match-test
  (testing "Sets != lists != maps"
    (is (not= (comprehend (db-set #{1})
                          {x y}
                          [x y])
              [[nil 1]]))
    (is (not= (comprehend (db-set {nil 1})
                          #{x}
                          x)
              [1]))
    (is (not= (comprehend (db-set [1])
                          {x y}
                          [x y])
              [[0 1]]))
    (is (not= (comprehend (db-set {0 1})
                          [x]
                          x)
              [1])))
  (testing "Sets"
    (is (= (comprehend (db-set #{3})
                       #{x y}
                       x)
           [3]))
    (is (= (set (comprehend (db-set #{} #{1} #{2 3})
                            #{x}
                            x))
           #{1 2 3}))
    (is (= (comprehend (db-set #{1})
                       #{x 4}
                       x)
           [])))
  (testing "Vectors and lists"
    (is (= (comprehend (db-set [1 2]
                               [3]
                               [4 5 6 7])
                       [x y z]
                       :something)
           []))
    (is (= (set (comprehend (db-set [1 2 3]
                                    [4 5 6]
                                    [7 8])
                            [x y z]
                            x))
           #{1 4}))
    (is (= (comprehend (db-set [1 2 3]
                               [4 5 6]
                               [7 8])
                       [x 5 z]
                       x)
           [4]))
    (is (= (set (comprehend (db-set [1 2]
                                    '(3 4))
                            (x y)
                            [x y]))
           #{[1 2] [3 4]})))
  (testing "Maps"
    (is (= (set (comprehend (db-set {1 2}
                                    {3 4 5 6})
                            {x y a b c d}
                            x))
           #{1 3 5}))
    (is (= (set (comprehend (db-set {:a 1 :b 2 :c 1}
                                    {:d 1 :e 2})
                            {x 1}
                            x))
           #{:a :c :d}))
    (is (= (set (comprehend (db-set {:a 1 :b 2 :c 1}
                                    {:a 4 :d 6})
                            {:a x}
                            x))
           #{1 4})))
  (testing "Literals and symbols"
    (is (= (comprehend (db-set [true false])
                       [x false]
                       x)
           [true]))
    (is (= (comprehend (db-set [true false])
                       [x true]
                       x)
           []))
    (is (= (comprehend (db-set ['a 'b])
                       [x 'b]
                       x)
           ['a]))
    (is (= (comprehend (db-set ['a 'b])
                       [x 'a]
                       x)
           [])))
  (testing "Subpatterns"
    (is (= (set (comprehend (db-set [[1] 2] 3)
                            x
                            x))
           #{[[1] 2] 3}))
    (is (= (comprehend (db-set [[1] 2] 3)
                       [[x] y]
                       x)
           [1]))))

(run-tests)