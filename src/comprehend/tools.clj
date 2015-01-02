(ns comprehend.tools
  (:refer-clojure :exclude [println memoize])
  (:require [clojure.core.reducers :as r]
            [clojure.set :as set]
            [clojure.walk :as w]
            [clojure.string :as string]
            [clojure.core.cache :as cache]))

(defn println [& args]
  (.write *out* (str (string/join \space args)
                     \newline))
  (flush))

(defmacro assert-notice []
  (when *assert*
    `(println (str "NOTICE: Assertions are enabled for "
                   (ns-name *ns*)
                   ". This will affect performance."))))

;;
;; CORE.CACHE BASED MEMOIZATION
;;

(defn memoized [!cache f & args]
  {:pre [(some? !cache)
         (fn? f)]}
  @(let [k [f args]
         c @!cache
         !r (cache/lookup c k)]
     (if !r
       (do
         (compare-and-set! !cache c (cache/hit c k))
         !r)
       (cache/lookup (swap! !cache
                            (fn [c]
                              (if (cache/has? c k)
                                c
                                (cache/miss c k (delay (apply f args))))))
                     k))))

;;
;; FIXPOINTS
;;

(defn fix [f]
  #(let [v (f %)]
     (if (= % v)
       v
       (recur v))))

(defmacro fixpoint [[name val] expr]
  `((fix (fn [~name] ~expr)) ~val))

(def ^:dynamic *!changed*)

(defn cmapcat [f coll]
  (let [!changed *!changed*]
    (r/mapcat (fn [x]
                (if-let [r (f x)]
                  (do
                    (reset! !changed true)
                    r)
                  [x]))
              coll)))

(defn cfix [f]
  (fn [x]
    (binding [*!changed* (atom false)]
      (loop [x x]
        (let [v (f x)]
          (if @*!changed*
            (do
              (reset! *!changed* false)
              (recur v))
            v))))))

;;
;; OPERATIONS ON COLLECTIONS
;;

(defn as-set [coll]
  {:pre [(coll? coll)]
   :post [(set? %)]}
  (if (set? coll)
    coll
    (set coll)))

(defn partial-intersection
  "Set-intersection, with nil treated as the universal set."
  [s1 s2]
  {:pre [(or (nil? s1) (coll? s1))
         (or (nil? s2) (coll? s2))]
   :post [(or (nil? %) (coll? %))]}
  (cond (and s1 s2) (set/intersection (as-set s1) (as-set s2))
        s1 s1
        :else s2))

(defn subst [m structure]
  {:pre [(map? m)]}
  (w/postwalk #(if-let [v (find m %)]
                 (val v)
                 %)
              structure))