(ns comprehend.core-test
  (:require [clojure.test :refer :all]
            [comprehend.core :refer :all]
            [comprehend.core :as c]))

(def this-ns-symb 'comprehend.core-test)

(let [A (hash-set 1 2 3)
      B (apply indexed-set A)]
  (defmacro invariance-test [varname expr]
    `(is (= ~@(map (fn [X]
                     `(let [~varname ~X]
                        ~expr))
                   [A B])))))

(deftest all-tests
  (testing "IPersistentSet"
    (invariance-test S (set (seq S)))
    (invariance-test S (set S))
    (invariance-test S (count S))
    (let [a (gensym)] (invariance-test S (set (cons a S))))
    (let [a (gensym)] (invariance-test S (set (conj S a))))
    (invariance-test S (set (conj S (first S))))
    (is (= (empty (indexed-set 1 2 3)) (indexed-set)))
    ; (invariance-test S S) ; TO DO: this seems to require subclassing java.lang.Set
    (let [a (gensym)] (invariance-test S (set (disj (conj S a) a))))
    (invariance-test S (set (disj S (gensym))))
    (invariance-test S (contains? S (first S)))
    (invariance-test S (contains? S (gensym)))
    (let [a (gensym)] (invariance-test S (get (conj S a) a)))
    (invariance-test S (get S (gensym))))
  (testing "Strong equality"
    (let [S (indexed-set 1 2 3)
          a (gensym)
          strong= #(and (= (.-m %1) (.-m %2))
                        (= (.-idx %1) (.-idx %2)))]
      (is (strong= (conj S a) (-> S (conj a) (disj a) (conj a))))
      ; (is (strong= S (conj S (first S) (disj S a))))
      ; (is (strong= S (disj (conj S a))))
      ))
  (testing "Comprehension"
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
             [1])))
    (testing "Opaque collections"
      (is (= (set (comprehend (indexed-set #{[1] ^::c/opaque [2]})
                              #{x}
                              x))
             #{[1] [2]}))
      (is (= (comprehend (indexed-set #{[1] ^::c/opaque [2]})
                         #{[x]}
                         x)
             [1]))
      (testing "Automatic hinting of opaqueness in patterns"
        (is (= (comprehend (indexed-set ^::c/opaque [1 [2]])
                           [1 [2]]
                           true)
               [true])))))
  (testing "Other"
    (is (indexed-set? (indexed-set 1 2)))
    (is (not (indexed-set? (hash-set 1 2))))))

(defn test-comprehend []
  (require [this-ns-symb] :reload-all)
  (run-tests this-ns-symb))