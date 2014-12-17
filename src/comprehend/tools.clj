(ns comprehend.tools
  (:refer-clojure :exclude [memoize])
  (:require [clojure.set :as set]
            [clojure.walk :as w]
            [clojure.core.cache :as cache]))

(defmacro assert-notice []
  (when *assert*
    `(println (str "NOTICE: Assertions are enabled for "
                   (ns-name *ns*)
                   ". This will affect performance."))))

(assert-notice)

;
; CACHE
;

(defn soft-cache []
  (cache/soft-cache-factory {}))

(def ^:dynamic *!cache* nil)

(defmacro with-cache-atom [!cache & body]
  `(binding [*!cache* ~!cache]
            ~@body))

(defn memoize [f]
  (fn [& args]
    {:pre [(some? *!cache*)]}
    (let [!c *!cache*
          k [f args]
          r (cache/lookup @!c k ::not-found)]
      (if (identical? ::not-found r)
        (let [r (apply f args)]
          (swap! !c cache/miss k r)
          r)
        (do
          (swap! !c cache/hit k)
          r)))))

;
; MISC
;

(defn fix [f] ; XXX moved
  #(let [v (f %)]
     (if (= % v)
       v
       (recur v))))

(defmacro fixpoint [[name val] expr] ; XXX moved
  `((fix (fn [~name] ~expr)) ~val))

(defn subst [m structure]
  {:pre [(map? m)]}
  (w/postwalk #(if-let [v (find m %)]
                 (val v)
                 %)
              structure))

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