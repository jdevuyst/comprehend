(ns comprehend.mutable-test
  (:require [clojure.test :refer :all]
            [comprehend :as c]
            [comprehend.mutable :as cm]))

(def ^:private !!sema (atom (promise)))

(defn- unwait []
  (deliver @!!sema nil))

(defn wait []
  (deref (deref !!sema))
  (reset! !!sema (promise)))

(deftest flat-file-database-tests
  (let [file (java.io.File/createTempFile "example" "edn")
        filename (str file)
        db (cm/stored-indexed-set filename)]
    (cm/conj! db 1)
    (cm/conj! db 2)
    (cm/disj! db 2)
    (cm/conj! db 3)
    (Thread/sleep 500)
    (is (= #{1 3}
           (-> @db seq set)
           (-> filename
               cm/stored-indexed-set
               deref
               seq
               set)))
    (.delete file)))

(deftest README-mutable-examples
  (let [db (cm/mutable-indexed-set)]
    (cm/conj! db 1)
    (cm/conj! db 2)
    (cm/disj! db 2)
    (cm/conj! db 3)
    (is (= (set (seq @db)) #{1 3})))
  (let [!log (atom [])
        f (fn
            ([] [1 2 3])
            ([s diff]
             (swap! !log conj [diff (set (seq s))])
             (unwait)))
        db (cm/mutable-indexed-set f)]
    (cm/conj! db 4)
    (wait)
    (cm/disj! db 4)
    (wait)
    (cm/conj! db 5)
    (wait)
    (is (= @!log [[{4 :added} #{1 2 3 4}]
                  [{4 :removed} #{1 2 3}]
                  [{5 :added} #{1 2 3 5}]]))
    (reset! !log [])
    (dosync
      (cm/conj! db 6)
      (cm/disj! db 6)
      (cm/conj! db 7))
    (wait)
    (is (= @!log [[{6 :removed, 7 :added}
                   #{1 2 3 5 7}]]))))


(let [this-ns-name (ns-name *ns*)]
  (defn reload-and-test []
    (require [this-ns-name] :reload-all)
    (run-tests this-ns-name)))