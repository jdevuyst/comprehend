(ns comprehend.mutable
  (:refer-clojure :exclude [flush conj! disj!])
  (:require [comprehend :as c]
            [clojure.edn :as edn]))

(deftype MutableSet [!s !log a]
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

(defn- db-agent-send [db f]
  (send-off (.-a db) #(do (f db %) %)))

(defn flush [db]
  (let [sema (promise)]
    (db-agent-send db (fn [db io] (deliver sema nil)))
    @sema)
  db)

(defn- ref-steal [!ref]
  (let [v @!ref]
    (when (not= v (empty v))
      (ref-set !ref (empty v)))
    v))

(defn- process-logs [db io]
  (let [[s log] (dosync [@(.-!s db)
                         (ref-steal (.-!log db))])]
    (when (seq log)
      (io s log))
    io))

(defn mutable-indexed-set
  ([] (mutable-indexed-set (constantly nil)))
  ([io] (let [db (MutableSet. (ref (into (c/indexed-set) (io)))
                              (ref {})
                              (agent io))]
          (add-watch (.-!log db)
                     ::log-agent
                     (fn [& args] (db-agent-send db process-logs)))
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