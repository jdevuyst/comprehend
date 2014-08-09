(ns comprehend-test
  (:require [clojure.test :refer :all]
            [comprehend :refer :all]
            [comprehend :as c]
            [clojure.walk :as w]))

(let [A (hash-set 1 2 3)
      B (apply indexed-set A)]
  (defmacro invariance-test [varname expr]
    `(is (= ~@(map (fn [X]
                     `(let [~varname ~X]
                        ~expr))
                   [A B])))))

(deftest all-tests
  (testing "Interfaces"
    (invariance-test S (set (seq S)))
    (invariance-test S (set S))
    (invariance-test S (count S))
    (let [a (gensym)] (invariance-test S (set (conj S a))))
    (invariance-test S (set (conj S (first S))))
    (is (= (.hashCode (indexed-set 1 2)) (hash (indexed-set 1 2))))
    (is (not= (hash (indexed-set 1 2)) (hash (indexed-set 1 2 3))))
    (is (= (empty (indexed-set 1 2 3)) (indexed-set)))
    (is (= (indexed-set 1 2) (indexed-set 1 2)))
    (is (not= (indexed-set 1 2) (indexed-set 1 2 3)))
    (is (.equals (indexed-set 1 2) (indexed-set 1 2)))
    (is (not (.equals (indexed-set 1 2) (indexed-set 1 2 3))))
    (is (not= (c/indexed-set [1]) (c/indexed-set ^::c/opaque [1])))
    (is (not (.equals (c/indexed-set [1]) (c/indexed-set ^::c/opaque [1]))))
    (let [a (gensym)] (invariance-test S (set (disj (conj S a) a))))
    (invariance-test S (set (disj S (gensym))))
    (invariance-test S (contains? S (first S)))
    (invariance-test S (contains? S (gensym)))
    (is (not (contains? (c/indexed-set) (gensym))))
    (let [a (gensym)] (invariance-test S (get (conj S a) a)))
    (invariance-test S (get S (gensym)))
    (let [a (gensym)] (invariance-test S ((conj S a) a)))
    (invariance-test S (S (gensym)))
    (let [a (gensym)] (invariance-test S (a (conj S a))))
    (invariance-test S ((gensym) S))
    (invariance-test S (.toString S)))
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
             nil)))
    (testing "Vectors and lists"
      (is (= (comprehend (indexed-set [1 2]
                                      [3]
                                      [4 5 6 7])
                         [x y z]
                         :something)
             nil))
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
             nil)))
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
             nil))
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
             nil))
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
               [true]))
        (is (= (c/comprehend (c/indexed-set [1] [2])
                             [2]
                             true)
               [true])))))
  (testing "Forward comprehension"
    (is (= (set (comprehend :mark :b
                            (-> (indexed-set [1 2] [2 3] [3 4] [5 6])
                                (mark :b)
                                (conj [4 5]))
                            [x y]
                            [y z]
                            [x y z]))
           #{[3 4 5] [4 5 6]}))
    (is (= (-> (indexed-set)
               (mark :a)
               (into [1 2 3])
               (mark :a)
               (into [4 5 6])
               (as-> s (comprehend :mark :a s x x))
               set)
           #{4 5 6})))
  (testing "Strong equality and index integrity"
    (let [S (-> (indexed-set)
                (mark :a :b)
                (into '([[:test] [[[#{:a}]]]]
                        [1 [2 "test" {[[#{:a}]] [:b]}] 3]
                        [:test]
                        4)))
          a [[[[[2 [[[#{:a}]]]]]] 3] (gensym)]
          b [[:test] [[[#{:a}]]]]
          norm (partial w/postwalk #(if (map? %)
                                      (->> %
                                           (filter (comp not empty? second))
                                           (into {}))
                                      %))
          strong= #(and (= (indexed-set? %1)
                           (indexed-set? %2))
                        (= %1 %2)
                        (= (.-m %1) (.-m %2))
                        (= (.-markers %1) (.-markers %2))
                        (= (norm (.-idx %1)) (norm (.-idx %2))))]
      (is (not (strong= S (conj S a))))
      (is (strong= S (disj S a)))
      (is (strong= (conj S a) (conj (conj S a) a)))
      (is (strong= (conj S a) (-> S (conj a) (disj a) (conj a))))
      (is (strong= S (conj (disj S a) (first S))))
      (is (strong= S (disj (conj S a) a)))
      (is (strong= S (conj S b)))
      (is (strong= (conj S b) (-> S (conj b) (disj b) (conj b))))
      (is (strong= S (conj (disj S b) b)))
      (is (strong= (indexed-set 1 2 [3 4])
                   (unmark (into (mark (indexed-set) :a) '(1 2 [3 4])) :a)))
      (is (not= (indexed-set 1)
                (-> (indexed-set 1)
                    (mark :a))))
      (is (strong= (-> (indexed-set 1)
                       (mark :a))
                   (-> (indexed-set 1)
                       (mark :a)
                       (mark :a))))
      (is (strong= (-> (indexed-set 1)
                       (conj 2)
                       (mark :a))
                   (-> (indexed-set 1)
                       (mark :a)
                       (conj 2)
                       (mark :a))))
      (is (= (comprehend (disj S b)
                         [1 [2 "test" {[[#{:a}]] [x]}] 3]
                         x)
             [:b]))
      (is (= (comprehend (disj S b)
                         [:test]
                         true)
             [true]))))
  (testing "Metadata"
    (let [m {:a :b :c :d}
          s (with-meta (indexed-set 1 2) m)]
      (is (= (meta s) m))
      (is (= m (meta (conj s (gensym)))))
      (is (= m (meta (disj s (first s)))))
      (is (= m (meta (mark s :a :b :c))))))
  (testing "up/top"
    (is (= (c/comprehend (c/indexed-set [1 [2 [3]]])
                         [x [y [z]]]
                         (c/up z 2))
           '(([2 [3]]))))
    (is (= (c/comprehend (c/indexed-set [1 [2 [3]]])
                         [x [y [z]]]
                         (c/top z))
           (c/comprehend (c/indexed-set [1 [2 [3]]])
                         [x [y [z]]]
                         (c/top y))
           (c/comprehend (c/indexed-set [1 [2 [3]]])
                         [x [y [z]]]
                         (c/top x))
           (c/comprehend (c/indexed-set [1 [2 [3]]])
                         [x [y [z]]]
                         (c/up x))
           '(([1 [2 [3]]]))))
    (is (= (c/comprehend (c/indexed-set [1] [[1]] [[[1]]])
                         [[x]]
                         [[[x]]]
                         (c/top x))
           '(([[1]] [[[1]]]))))
    (is (= (set (c/comprehend (c/indexed-set #{:a 1} #{[:a] 2} #{[[:a]] 3}
                                             #{[[[[[:a]]]]] 4} #{[[[[[[:a]]]]]] 5}
                                             #{[[[[[[[[:a]]]]]]]] 6})
                              #{[x]}
                              #{[[x]]}
                              (set (c/top x))))
           #{#{#{2 [:a]} #{3 [[:a]]}}
             #{#{4 [[[[[:a]]]]]} #{[[[[[[:a]]]]]] 5}}}))
    (is (= (c/comprehend (c/indexed-set [1 2] [1 3])
                         [x 2]
                         [y 3]
                         [(c/up x) (c/up y)])
           '([([1 2]) ([1 3])]))))
  (testing "Other"
    (is (= (-> (indexed-set)
               (mark :a :b :c)
               (into [1 2])
               (mark :a :b :c)
               (into [3 4])
               (mark :a :b)
               (conj 5)
               (mark :c)
               (unmark :b :c)
               .-markers)
           #{:a}))
    (is (= (set (c/auto-comprehend (c/indexed-set [1 2 [3 [4]]] [10 20 [30 [40]]])
                                   [a b [c [d]]]))
           #{{:a 1 :b 2 :c 3 :d 4} {:a 10 :b 20 :c 30 :d 40}}))
    (is (= (set (c/auto-comprehend :mark :a
                                   (into (mark (c/indexed-set 0) :a) #{[1 2 [3 [4]]] [10 20 [30 [40]]]})
                                   [a b [c [d]]]))
           #{{:a 1 :b 2 :c 3 :d 4} {:a 10 :b 20 :c 30 :d 40}}))
    (is (indexed-set? (indexed-set 1 2)))
    (is (not (indexed-set? (hash-set 1 2))))
    (is (= (fixpoint [x 1]
                     (if (>= x 1000)
                       x
                       (* 2 x)))
           1024))
    (is (= ((c/fix #(min 10 (inc %))) 0)
           10))))

(deftest README-examples
  (is (= (set (c/indexed-set 1 2 3))
         (hash-set 1 3 2 3)))
  (let [s (c/indexed-set [:person 1] [:person 2] [:person 3]
                         [:parent-of 1 2] [:parent-of 2 3] [:parent-of 3 4])]
    (is (= (set (c/comprehend s
                              [:parent-of a b]
                              [:parent-of b c]
                              [:grandparent-of a c]))
           #{[:grandparent-of 1 3] [:grandparent-of 2 4]})))
  (is (= (c/comprehend (c/indexed-set [[1 2]] [[2 3]] [[1 2 3]])
                       [[1 x]]
                       x)
         '(2)))
  (is (= (set (c/comprehend (c/indexed-set {1 2 3 4})
                            {x y}
                            [x y]))
         #{[1 2] [3 4]}))
  (let [bound-symb 1]
    (is (= (c/comprehend (c/indexed-set [1 2] [3 4])
                         [bound-symb unbound-symb]
                         [bound-symb unbound-symb])
           '([1 2]))))
  (is (= (c/comprehend (c/indexed-set [0 1] [1 2])
                       [(dec 1) x]
                       x)
         '(1)))
  (is (= (set (c/rcomprehend [s (c/indexed-set [1] [3] [5] [7] [9])]
                             [x]
                             (conj s [(inc x)])))
         #{[1] [2] [3] [4] [5] [6] [7] [8] [9] [10]}))
  (is (= (c/rcomprehend [s (c/indexed-set [1] [2] [3])]
                        [x]
                        (conj s [(- x)]))
         (c/indexed-set [1] [-1] [2] [-2] [3] [-3])))
  (is (= (c/comprehend [s (c/indexed-set [1] [2])]
                       [x]
                       s)
         (list (c/indexed-set [1] [2])
               (c/indexed-set [1] [2]))))
  (is (= (set (c/comprehend (c/indexed-set {:a 1} {:b 1} {:a 2 :b 2})
                            {:a x}
                            (c/up x)))
         '#{({:a 1}) ({:b 2, :a 2})}))
  (is (= (c/comprehend (c/indexed-set #{:a 1} #{[:a] 2} #{[[:a]] 3})
                       #{[x]}
                       #{[[x]]}
                       (set (c/top x)))
         '(#{#{2 [:a]} #{3 [[:a]]}})))
  (is (= (-> (indexed-set 1)
             (mark :a :b)
             (conj 2)
             (mark :a)
             (conj 3)
             (as-> s (comprehend :mark :a s x x))
             set)
         #{3}))
  (is (= (-> (indexed-set 1)
             (mark :a :b)
             (conj 2)
             (mark :a)
             (conj 3)
             (as-> s (comprehend :mark :b s x x))
             set)
         #{2 3}))
  (is (= (c/comprehend :mark "marker"
                       (-> (c/indexed-set [1 2] [2 3])
                           (mark "marker")
                           (conj [3 4]))
                       [a b]
                       [b c]
                       [a b c])
         '([2 3 4])))
  (let [s (-> (c/indexed-set 1)
              (mark :a)
              (conj 2))]
    (is (= (set (c/comprehend :mark :a
                              [s' s]
                              x
                              s'))
           #{(mark s :a)})))
  (is (= (c/comprehend :mark :a
                       [s (-> (c/indexed-set 1)
                              (mark :a)
                              (conj 2))]
                       x
                       {x (c/comprehend :mark :a
                                        (conj s 3)
                                        y
                                        y)})
         [{2 [3]}]))
  (is (= (set (comprehend (indexed-set 1 2 3 4)
                          x
                          (if (even? x)
                            x
                            ::c/skip)))
         #{2 4}))
  (is (= (set (c/auto-comprehend (c/indexed-set [1 [2 [3]]] [10 [20 [30 [40]]]])
                                 [a [b c]]))
         #{{:a 1 :b 2 :c [3]} {:a 10 :b 20 :c [30 [40]]}}))

  (is (= (set (c/comprehend (c/indexed-set [1] ^::c/opaque [2])
                            [x]
                            y
                            [x y]))
         #{[1 [1]] [1 [2]]}))
  (is (= (c/comprehend (c/indexed-set [^::c/opaque [1]])
                       [[x]]
                       x)
         nil))
  (is (= (c/comprehend (c/indexed-set [1] [^::c/opaque [1]])
                       [[x]]
                       x)
         '(1)))
  (is (= (c/indexed-set [1])
         (c/indexed-set [1])))
  (is (not= (c/indexed-set 1)
            (mark (c/indexed-set 1) :a)))
  (is (not= (c/indexed-set [1])
            (c/indexed-set ^::c/opaque [1])))
  (is (= (-> (indexed-set 1)
             (conj 2)
             (mark :a))
         (-> (indexed-set 1)
             (mark :a)
             (conj 2)
             (mark :a))))
  (is (not= (c/indexed-set 1) #{1}))
  (is (= (set (c/rcomprehend [s (c/indexed-set [1 2] [2 3] [3 4])]
                             [a b]
                             [b c]
                             (conj s [a c])))
         #{[1 2] [2 3] [3 4] [1 3] [2 4]}))
  (is (= (set (c/fixpoint [s (c/indexed-set [1 2] [2 3] [3 4])]
                          (c/rcomprehend [s' s]
                                         [a b]
                                         [b c]
                                         (conj s' [a c]))))
         #{[1 2] [2 3] [3 4] [1 3] [2 4] [1 4]})))

(let [this-ns-name (ns-name *ns*)]
  (defn test-comprehend []
    (require [this-ns-name] :reload-all)
    (run-tests this-ns-name)))