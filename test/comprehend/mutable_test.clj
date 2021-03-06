(ns comprehend.mutable-test
  (:require [clojure.test :refer :all]
            [comprehend :as c]
            [comprehend.mutable :as cm]))

(deftest flat-file-database-tests
  (let [file (java.io.File/createTempFile "example" "edn")
        filename (str file)
        db (cm/stored-indexed-set filename)]
    (cm/conj db 1)
    (cm/conj db 2)
    (cm/disj db 2)
    (cm/conj db 3)
    (cm/conj db 4)
    (cm/conj db 5)
    (cm/disj db 4)
    (cm/flush db)
    (is (= #{1 3 5}
           (-> @db seq set)
           (-> filename
               cm/stored-indexed-set
               deref
               seq
               set)))
    (.delete file)))

(deftest README-mutable-examples
  (let [db (cm/mutable-indexed-set)]
    (is (cm/mutable-indexed-set? db))
    (cm/conj db 1)
    (cm/conj db 2)
    (cm/disj db 2)
    (cm/conj db 3)
    (is (= (set (seq @db)) #{1 3})))
  (let [!log (atom [])
        f (fn
            ([] [1 2 3])
            ([s diff] (swap! !log conj [diff (set (seq s))])))
        db (cm/mutable-indexed-set f)]
    (cm/conj db 4)
    (cm/flush db)
    (cm/disj db 4)
    (cm/flush db)
    (cm/conj db 5)
    (cm/flush db)
    (is (= @!log [[{4 :added} #{1 2 3 4}]
                  [{4 :removed} #{1 2 3}]
                  [{5 :added} #{1 2 3 5}]]))
    (reset! !log [])
    (dosync
      (cm/conj db 6)
      (cm/disj db 6)
      (cm/conj db 7))
    (cm/flush db)
    (is (= @!log [[{6 :removed, 7 :added}
                   #{1 2 3 5 7}]]))))

(deftest other-tests
  (is (cm/mutable-indexed-set? (cm/mutable-indexed-set)))
  (is (not (cm/mutable-indexed-set? (c/indexed-set))))
  (is (not (c/indexed-set? (cm/mutable-indexed-set))))
  (is (= (-> (cm/mutable-indexed-set)
             (cm/conj 1)
             (cm/conj 2)
             (cm/mark :a :b)
             (cm/conj 3)
             (cm/unmark :b :c)
             (cm/disj 2)
             (cm/conj 4)
             deref)
         (-> (c/indexed-set)
             (conj 1)
             (conj 2)
             (c/mark :a :b)
             (conj 3)
             (c/unmark :b :c)
             (disj 2)
             (conj 4)))))

(let [this-ns-name (ns-name *ns*)]
  (defn reload-and-test []
    (require [this-ns-name] :reload-all)
    (run-tests this-ns-name)))