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

(defmacro ^:private falsum [explanation]
  `[[(variable ~explanation) #{}]])

(declare unify)

(defn unify-colls [coll* coll]
  {:pre [(coll? coll*)
         (coll? coll)
         (-> coll* map? not)
         (-> coll map? not)
         (grounded? coll)]
   :post [(constraint-coll? %)]}
  (map (fn [el*] [el* coll])
       coll*))

(defn unify-sequentials [l* l]
  {:pre [(sequential? l*)
         (sequential? l)
         (grounded? l)
         (= (count l*) (count l))]
   :post [(constraint-coll? %)]}
  (mapcat unify l* l))

(defn unify-maps [m* m]
  {:pre [(map? m*)
         (map? m)
         (grounded? m)]
   :post [(constraint-coll? %)]}
  (map (fn [kv] [kv (seq m)])
       (seq m*)))

(defn unify [x* x]
  {:pre [(grounded? x)]
   :post [(constraint-coll? %)]}
  (cond (sequential? x*) (if (and (sequential? x)
                                  (= (count x*)
                                     (count x)))
                           (unify-sequentials x* x)
                           (falsum :not-sequential))
        (map? x*) (if (map? x)
                    (unify-maps x* x)
                    (falsum :not-a-map))
        (coll? x*) (if (and (coll? x)
                            (-> x map? not))
                     (unify-colls x* x)
                     (falsum :not-a-coll))
        :else [[x* #{x}]]))

;
; OPERATIONS ON SEQUENCES OF CONSTRAINT PAIRS
;

(defn t-transform [f constraints]
  {:pre [(fn? f)
         (constraint-coll? constraints)]
   :post [(constraint-coll? %)]}
  (mapcat (fn [[x* x :as v]]
            (or (f x* x)
                [v]))
          constraints))

(defn decompose-dom-terms [x* dom]
  {:post [(constraint-coll? %)]}
  (when (and (= 1 (count dom))
             (not (varname x*)))
    (unify x* (first dom))))

(defn extract-contradictory-literals [x dom]
  (when (and (-> x coll? not)
             (-> x varname not))
    (let [s (ctools/as-set dom)]
      (if (contains? s x)
        []
        [[x #{}]]))))

(declare find-models)

(def find-and-index-models
  (ctools/memoize (fn [x* x ks]
                    (set/index (find-models x* x)
                               ks))))

(defn simplify-domains [x* dom]
  (let [{:keys [query const-map]} (generalize x*)]
    (assert query)
    (assert (map? const-map))
    (cond (varname query)
          nil ; merely need to test for consistency

          (not= x* query)
          [[x* (as-> (find-and-index-models #{query}
                                            dom
                                            (keys const-map)) $
                     ($ const-map)
                     (map #(ctools/subst % query) $)
                     (set $))]]

          :else
          nil)))

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
       (t-transform decompose-dom-terms)
       (t-transform extract-contradictory-literals)
       (t-transform simplify-domains) ; XXX call this less often
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

(defn find-models [x* x]
  {:pre [(grounded? x)]
   :post [(every? model? %)]}
  (->> [[[x* [x]]]]
       develop-all
       (map constraints-as-model)
       set))

(defn match-in [x* dom]
  {:pre [(coll? dom)
         (-> dom map? not)]
   :post [(coll? %)]}
  (->> (find-models #{x*} dom)
       (map #(ctools/subst % x*))))