(ns comprehend.mutable
  (:refer-clojure :exclude [conj! disj!])
  (:require [comprehend :as c]
            [clojure.edn :as edn]))

(deftype MutableSet [!s !log]
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
  (dosync
    (apply alter (.-!s db) c/mark markers)
    db))

(defn unmark! [db & markers]
  (dosync
    (apply alter (.-!s db) c/unmark markers)
    db))

(defn- ref-steal [!ref]
  (as-> @!ref $
        (when-not (empty? $)
          (ref-set !ref (empty $))
          $)))

(defn mutable-indexed-set
  ([] (mutable-indexed-set (constantly nil)))
  ([io] (let [!s (ref (into (c/indexed-set) (io)))
              !log (ref {})
              !a (agent nil)]
          (add-watch !log
                     ::writer
                     (fn [k !log old-state new-state]
                       (send-off !a (fn [a-val]
                                      (let [[s diff] (dosync [@!s (ref-steal !log)])]
                                        (if-not (empty? diff)
                                          (io s diff))
                                        nil)))))
          (MutableSet. !s !log))))

(defn- rate-limit-io
  ([io] (rate-limit-io io 250))
  ([io sec] (fn
              ([] (io))
              ([s diff]
               (io s diff)
               (Thread/sleep sec)))))

(defn- edn-file-io [filename]
  (fn
    ([] (try
          (-> filename slurp edn/read-string)
          (catch java.io.FileNotFoundException x nil)))
    ([s diff] (let [tempname (str filename " - " (java.util.Date.))]
                (->> s seq pr-str (spit tempname))
                (.renameTo (java.io.File. tempname)
                           (java.io.File. filename))))))

(defn stored-indexed-set [filename]
  (-> filename edn-file-io rate-limit-io mutable-indexed-set))