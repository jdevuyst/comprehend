(ns comprehend.mutable
  (:refer-clojure :exclude [flush conj! disj!])
  (:require [comprehend :as c]
            [clojure.edn :as edn]))

(deftype MutableSet [!s !log !!sema]
  clojure.lang.IDeref
  (deref [this] @!s))

(defn flush [db]
  (when-let [sema @(.-!!sema db)]
    @sema)
  db)

(defn conj! [db v]
  (dosync
    (when-not (contains? @(.-!s db) v)
      (when-not @(.-!!sema db)
        (ref-set (.-!!sema db) (promise)))
      (alter (.-!log db) assoc v :added)
      (alter (.-!s db) conj v)))
  db)

(defn disj! [db v]
  (dosync
    (when (contains? @(.-!s db) v)
      (when-not @(.-!!sema db)
        (ref-set (.-!!sema db) (promise)))
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
  (let [v @!ref]
    (ref-set !ref (empty v))
    v))

(defn mutable-indexed-set
  ([] (mutable-indexed-set (constantly nil)))
  ([io] (let [!s (ref (into (c/indexed-set) (io)))
              !log (ref {})
              !!sema (ref nil)
              !a (agent nil)]
          (add-watch !log
                     ::writer
                     (fn [k !log old-state new-state]
                       (send-off !a (fn [a-val]
                                      (let [[s diff sema]
                                            (dosync [@!s (ref-steal !log) (ref-steal !!sema)])]
                                        (when-not (empty? diff)
                                          (io s diff))
                                        (when sema
                                          (deliver sema nil)))))))
          (MutableSet. !s
                       !log
                       !!sema))))

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