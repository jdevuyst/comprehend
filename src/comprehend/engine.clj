(ns comprehend.engine
  (:require [comprehend.tools :as ctools]
            [clojure.set :as set]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [deftrace trace trace-ns]]))

(ctools/assert-notice)

;
; FIRST CLASS LOGICAL TERMS
;

(defprotocol ILogicalTerm
  (varname [_]))

(deftype Var [symb]
  ILogicalTerm
  (varname [this]
           (assert symb)
           symb)
  clojure.lang.IHashEq
  (hasheq [this] (hash symb))
  java.lang.Object
  (equals [this o]
          (= symb (varname o)))
  (toString [this]
            (.toString symb))
  (hashCode [this] (.hasheq this)))

(extend-type java.lang.Object
  ILogicalTerm
  (varname [this] false))

(extend-type nil
  ILogicalTerm
  (varname [this] false))

(defn variable [symb]
  {:pre [symb]
   :post [(varname %)]}
  (Var. symb))

(defn grounded? [x]
  (if (coll? x)
    (every? grounded? x)
    (not (varname x))))

;
; CONSTRAINT STRUCTURES
;

(defn- constraint-pair? [x]
  (and (vector? x)
       (= 2 (count x))
       (-> x second coll?)))

(defn- constraint-coll? [x]
  (and (or (nil? x) (coll? x))
       (every? constraint-pair? x)))

(defn- constraint-map? [x]
  (and (map? x) (constraint-coll? x)))

(defn model? [x]
  (and (map? x)
       (->> x keys (every? varname))))

(defn constraints-as-mmap [constraints]
  {:pre [(constraint-coll? constraints)]
   :post [(constraint-map? %)]}
  (reduce (fn [m [k v]]
            (let [r (ctools/partial-intersection (m k) v)]
              (if (empty? r)
                (reduced {})
                (assoc m k r))))
          {}
          constraints))

(defn constraints-as-model [constraints]
  {:pre [(constraint-coll? constraints)
         (->> constraints
              (map first)
              (every? varname))
         (->> constraints
              (map second)
              (map count)
              (every? (partial = 1)))
         (->> constraints
              (map first)
              frequencies
              vals
              (every? (partial = 1)))]
   :post [(model? %)]}
  (reduce (fn [m [k v]]

            (assoc m k (first v)))
          {}
          constraints))

;
; GENERALIZATIONS
;

; XXX {:a :b x y} and {:b :a x y}
; do not currenty have identical queries
; neither do [x y] and [y x]
(let [const (variable ::const)]
  (defn generalize [x*]
    (let [!m (atom {})
          f (fn [y]
              (let [y* (-> @!m count variable)]
                (swap! !m assoc y* y)
                y*))]
      {:query (w/prewalk #(if (grounded? %)
                            (f %)
                            %)
                         x*)
       :const-map @!m})))

;
; (PARTIAL) UNIFICATION WITH STRUCTURES
;

(def ^:private falsum [(variable :falsum) #{}])

(defn unification-type [x]
  (cond (map? x) :map
        (sequential? x) [:list-like (count x)]
        (coll? x) :set-like
        :else :not-a-coll))

(defn unify [x* x]
  {:pre [(grounded? x)]
   :post [(constraint-coll? %)]}
  (let [t (unification-type x*)]
    (cond (= :not-a-coll t)
          [[x* #{x}]]

          (not= t (unification-type x))
          [falsum]

          (= :set-like t)
          (map (fn [el*] [el* x])
               x*)

          (= :map t)
          (map (fn [kv] [kv (seq x)])
               x*)

          :else
          (mapcat unify x* x))))

;
; OPERATIONS ON SEQUENCES OF CONSTRAINT PAIRS
;

(defn decompose-dom-terms [[x* dom :as constraint]]
  {:post [(constraint-coll? %)]}
  (if (and (= 1 (count dom))
           (not (varname x*)))
    (unify x* (first dom))
    [constraint]))

(defn extract-contradictory-literals [[x* dom :as constraint]]
  (if (and (-> x* coll? not)
           (-> x* varname not))
    (if (contains? (ctools/as-set dom) x*)
      []
      [falsum])
    [constraint]))

(declare find-models)

(def find-and-index-models
  (ctools/memoize (fn [x* dom ks]
                    (set/index (find-models x* dom)
                               ks))))

(defn simplify-domains [[x* dom :as constraint]]
  (let [{:keys [query const-map]} (generalize x*)]
    (assert query)
    (assert (map? const-map))
    (cond (varname query)
          [constraint]

          (not= x* query)
          [[x* (as-> (find-and-index-models query
                                            dom
                                            (keys const-map)) $
                     ($ const-map)
                     (map #(ctools/subst % query) $)
                     (set $))]]

          :else
          [constraint])))

;
; CONSTRAINT MAPS
;

(defn subst-known-values [m]
  {:pre [(constraint-map? m)]
   :post [(constraint-map? %)]}
  (let [values (->> m
                    (filter (fn [[k v]] (varname k)))
                    (filter (fn [[k v]] (= 1 (count v))))
                    (map (fn [[k v]] [k (first v)]))
                    (into {}))]
    (->> m
         (map (fn [[k v]]
                [(if (varname k)
                   k
                   (ctools/subst values k))
                 (ctools/subst values v)]))
         (into {}))))

(defn develop1 [constraints]
  {:pre [(constraint-coll? constraints)]
   :post [(constraint-map? %)]}
  (->> constraints
       (mapcat decompose-dom-terms)
       (mapcat extract-contradictory-literals)
       (mapcat simplify-domains) ; XXX call this less often
       constraints-as-mmap
       subst-known-values))

(def develop (ctools/fix develop1))

;
; QUANTIFY OVER DOMAINS
;

(defn quantify1 [m]
  {:pre [(constraint-map? m)]
   :post [(every? (partial constraint-coll?) %)]}
  (let [[k dom :as kv]
        (->> m
             (filter (fn [[k dom]] (> (count dom) 1)))
             (reduce (fn [[k1 dom1 :as kv1] [k2 dom2 :as kv2]]
                       (if (or (> (count dom1) (count dom2))
                               (nil? kv1))
                         (cond-> kv2
                                 (= (count dom2) 2) reduced)
                         kv1))
                     nil))]
    (when kv
      (map (fn [v]
             (concat m
                     [[k #{v}]]))
           dom))))

(defn develop-all1 [metaverse]
  {:pre [(every? constraint-map? metaverse)]
   :post [(every? (partial constraint-coll?) %)]}
  (->> metaverse
       (map develop)
       (mapcat #(or (quantify1 %)
                    [%]))
       (filter (complement empty?))
       (map set)
       set))

; the develop* functions are currently not very efficient
(def develop-all (ctools/fix (comp develop-all1 (partial map constraints-as-mmap))))

(defn find-models [x* dom]
  {:pre [(coll? dom)
         (grounded? dom)]
   :post [(every? model? %)]}
  (->> [[[x* dom]]]
       develop-all
       (map constraints-as-model)
       set))