(ns comprehend.db
  (:require [comprehend :as c]
            [clojure.edn :as edn]))

(deftype DBSet [!s !log]
  clojure.lang.IDeref
  (deref [this] @!s))

(defn conj! [db v]
  (dosync
    (when-not (contains? (deref (.-!s db)) v)
      (alter (.-!log db) assoc v :added)
      (alter (.-!s db) conj v)))
  db)

(defn disj! [db v]
  (dosync
    (when (contains? (deref (.-!s db)) v)
      (alter (.-!log db) assoc v :removed)
      (alter (.-!s db) disj v)))
  db)

(defn mark! [db & markers]
  (dosync (apply alter (.-!s db) c/mark markers)))

(defn unmark! [db & markers]
  (dosync (apply alter (.-!s db) c/unmark markers)))

(defn- ref-steal [!ref]
  (as-> @!ref $
        (when-not (empty? $)
          (ref-set !ref (empty $))
          $)))

(defn db [iof]
  (let [!s (ref (into (c/indexed-set) (iof)))
        !log (ref {})
        !a (agent nil)]
    (add-watch !log
               ::writer
               (fn [k !log old-state new-state]
                 (send !a (fn [a-val]
                            (let [[s diff] (dosync [@!s (ref-steal !log)])]
                              (if-not (empty? diff)
                                (iof s diff))
                              nil)))))
    (DBSet. !s !log)))

(defn- rate-limit [f]
  (fn [& args]
    (Thread/sleep 300)
    (apply f args)))

(defn- edn-file-io [filename]
  (fn
    ([] (try
          (-> filename slurp edn/read-string)
          (catch java.io.FileNotFoundException x nil)))
    ([s diff] (->> s seq pr-str (spit filename)))))

(defn edn-file-db [filename]
  (-> filename edn-file-io rate-limit db-set))