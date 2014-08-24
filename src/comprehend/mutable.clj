(ns comprehend.mutable
  (:refer-clojure :exclude [flush conj! disj!])
  (:require [comprehend :as c]
            [clojure.edn :as edn]))

(deftype MutableSet [!s !log !semas a]
  clojure.lang.IDeref
  (deref [this] @!s))

(defn- alter-contents! [db op keyw v]
  (dosync
    (alter (.-!s db) op v)
    (alter (.-!log db) assoc v keyw))
  db)

(defn conj! [db v]
  (alter-contents! db conj :added v))

(defn disj! [db v]
  (alter-contents! db disj :removed v))

(defn- alter-markers! [db op markers]
  (dosync
    (apply alter (.-!s db) op markers)
    db))

(defn mark! [db & markers]
  (alter-markers! db c/mark markers))

(defn unmark! [db & markers]
  (alter-markers! db c/unmark markers))

(defn flush [db]
  (let [sema (promise)]
    (send-off (.-a db) (fn [io]
                         (deliver sema nil)
                         io))
    @sema)
  db)

(defn- ref-steal [!ref]
  (let [v @!ref]
    (when (not= v (empty v))
      (ref-set !ref (empty v)))
    v))

(defn- process-logs [db]
  (send-off (.-a db)
            (fn [io]
              (let [[s log old-semas] (dosync [@(.-!s db)
                                               (ref-steal (.-!log db))
                                               @(.-!semas db)])]
                (when (seq log)
                  (io s log))
                io))))

(defn mutable-indexed-set
  ([] (mutable-indexed-set (constantly nil)))
  ([io] (let [db (MutableSet. (ref (into (c/indexed-set) (io)))
                              (ref {})
                              (ref #{})
                              (agent io))]
          (add-watch (.-!log db)
                     ::io
                     (fn [& args] (process-logs db)))
          db)))

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