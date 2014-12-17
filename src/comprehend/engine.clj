(ns comprehend.engine
  (:require [comprehend.tools :as ct]
            [clojure.set :as set]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [deftrace trace trace-ns]]))

(ct/assert-notice)

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
; CONSTRAINT STRUCTURES
;

(defn constraint-pair? [x]
  (and (vector? x)
       (= 2 (count x))
       (-> x second coll?)))

(defn constraint-coll? [x]
  (and (or (nil? x) (coll? x))
       (every? constraint-pair? x)))

(defn constraint-map? [x]
  (and (map? x) (constraint-coll? x)))

(defn constraints-as-mmap [constraints]
  {:pre [(constraint-coll? constraints)]
   :post [(constraint-map? %)]}
  (reduce (fn [m [k v]]
            (let [r (ct/partial-intersection (m k) v)]
              (if (empty? r)
                (reduced {})
                (assoc m k r))))
          {}
          constraints))

;
; MODELS
;

(defn model? [x]
  (and (map? x)
       (->> x keys (every? varname))))

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
; OPERATIONS ON CONSTRAINT COLLECTIONS
;

(defn decompose-dom-terms [[x* dom :as constraint]]
  {:pre [(coll? dom)]
   :post [(constraint-coll? %)]}
  (if (and (= 1 (count dom))
           (not (varname x*)))
    (unify x* (first dom))
    [constraint]))

(defn extract-contradictory-literals [[x* dom :as constraint]]
  {:pre [(coll? dom)]
   :post [(constraint-coll? %)]}
  (if (and (-> x* coll? not)
           (-> x* varname not))
    (if (contains? (ct/as-set dom) x*)
      []
      [falsum])
    [constraint]))

(declare indexed-match-in)

(defn simplify-domains [[x* dom :as constraint]]
  {:pre [(coll? dom)]
   :post [(constraint-coll? %)]}
  (let [{:keys [query const-map]} (generalize x*)]
    (assert query)
    (assert (map? const-map))
    (cond (varname query)
          [constraint]

          (not= x* query)
          [[x* (as-> (indexed-match-in query
                                       dom
                                       (keys const-map)) $
                     ($ const-map)
                     (map #(ct/subst % query) $)
                     (set $))]]

          :else
          [constraint])))

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
                   (ct/subst values k))
                 (ct/subst values v)]))
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

(def develop (ct/fix develop1))

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
    (if kv
      (map (fn [v]
             (concat m
                     [[k #{v}]]))
           dom)
      [m])))

;
; OPERATIONS ON METAVERSES OF CONSTRAINT COLLECTIONS
;

(defn develop-all1 [metaverse]
  {:pre [(every? constraint-coll? metaverse)]
   :post [(set? %)
          (every? (partial constraint-map?) %)]}
  (->> metaverse
       (map develop)
       (mapcat quantify1)
       (filter (complement empty?))
       (map constraints-as-mmap)
       set))

; the develop* functions are currently not very efficient
(def develop-all (ct/fix develop-all1))

;
; FINDING MODELS
;

(defn match-with [xs* dom]
  {:pre [(coll? xs*)
         (coll? dom)
         (grounded? dom)]
   :post [(every? model? %)]}
  (->> [(map (fn [x*] [x* dom]) xs*)]
       develop-all
       (map constraints-as-model)))

(defn match-in [x* dom]
  {:pre [(coll? dom)
         (grounded? dom)]
   :post [(every? model? %)]}
  (match-with [x*] dom))

(def indexed-match-in
  (ct/memoize (fn [x* dom ks]
                    (set/index (match-in x* dom)
                               ks))))