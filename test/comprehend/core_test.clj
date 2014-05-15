(ns comprehend.core-test
  (:require [clojure.test :refer :all]
            [comprehend.core :refer :all]))

(let [A (hash-set 1 2 3)
      B (apply indexed-set A)]
  (defmacro invariance-test [varname expr]
    `(is (= ~@(map (fn [X]
                     `(let [~varname ~X]
                        ~expr))
                   [A B])))))

(deftest match-test
  (testing "IPersistentSet"
    (invariance-test S (set (seq S)))
    (invariance-test S (set S))
    (invariance-test S (count S))
    (invariance-test S (set (cons 0 S)))
    (invariance-test S (set (conj S 0)))
    (is (= (empty (indexed-set 1 2 3)) (indexed-set)))
    ; (invariance-test S S) ; TO DO: need to extend java.lang.Set (?)
    ; (invariance-test S (disj S (first (seq S))))
    (invariance-test S (contains? S (first (seq S))))
    (invariance-test S (contains? S (gensym)))
    (invariance-test S (get S (apply min (seq S))))
    (invariance-test S (get S (gensym))))
  (testing "Sets != lists != maps"
    (is (not= (comprehend (indexed-set #{1})
                          {x y}
                          [x y])
              [[nil 1]]))
    (is (not= (comprehend (indexed-set {nil 1})
                          #{x}
                          x)
              [1]))
    (is (not= (comprehend (indexed-set [1])
                          {x y}
                          [x y])
              [[0 1]]))
    (is (not= (comprehend (indexed-set {0 1})
                          [x]
                          x)
              [1])))
  (testing "Sets"
    (is (= (comprehend (indexed-set #{3})
                       #{x y}
                       x)
           [3]))
    (is (= (set (comprehend (indexed-set #{} #{1} #{2 3})
                            #{x}
                            x))
           #{1 2 3}))
    (is (= (comprehend (indexed-set #{1})
                       #{x 4}
                       x)
           [])))
  (testing "Vectors and lists"
    (is (= (comprehend (indexed-set [1 2]
                                    [3]
                                    [4 5 6 7])
                       [x y z]
                       :something)
           []))
    (is (= (set (comprehend (indexed-set [1 2 3]
                                         [4 5 6]
                                         [7 8])
                            [x y z]
                            x))
           #{1 4}))
    (is (= (comprehend (indexed-set [1 2 3]
                                    [4 5 6]
                                    [7 8])
                       [x 5 z]
                       x)
           [4]))
    (is (= (set (comprehend (indexed-set [1 2]
                                         '(3 4))
                            [x y]
                            [x y]))
           #{[1 2] [3 4]}))
    (is (= (set (comprehend (indexed-set [1 2]
                                         '(3 4))
                            (list x y)
                            [x y]))
           #{[1 2] [3 4]}))
    (is (= (comprehend (indexed-set [1 2]
                                    '(3 4))
                       '(x y)
                       [x y])
           [])))
  (testing "Maps"
    (is (= (set (comprehend (indexed-set {1 2}
                                         {3 4 5 6})
                            {x y a b c d}
                            x))
           #{1 3 5}))
    (is (= (set (comprehend (indexed-set {:a 1 :b 2 :c 1}
                                         {:d 1 :e 2})
                            {x 1}
                            x))
           #{:a :c :d}))
    (is (= (set (comprehend (indexed-set {:a 1 :b 2 :c 1}
                                         {:a 4 :d 6})
                            {:a x}
                            x))
           #{1 4})))
  (testing "Literals and symbols"
    (is (= (comprehend (indexed-set [true false])
                       [x false]
                       x)
           [true]))
    (is (= (comprehend (indexed-set [true false])
                       [x true]
                       x)
           []))
    (is (= (comprehend (indexed-set [true false])
                       [x (identity false)]
                       x)
           [true]))
    (is (= (comprehend (indexed-set ['a 'b])
                       [x 'b]
                       x)
           ['a]))
    (is (= (comprehend (indexed-set ['a 'b])
                       [x 'a]
                       x)
           []))
    (is (= (comprehend (indexed-set ['a 1] ['b 2])
                       ['a x]
                       x)
           [1])))
  (testing "Subpatterns"
    (is (= (comprehend (indexed-set [1 2] [ 2 3])
                         [a b]
                         [b c]
                         [a c])
           [[1 3]]))
    (is (= (set (comprehend (indexed-set [[1] 2] 3)
                            x
                            x))
           #{[[1] 2] 3}))
    (is (= (comprehend (indexed-set [[1] 2] 3)
                       [[x] y]
                       x)
           [1]))))

(run-tests)