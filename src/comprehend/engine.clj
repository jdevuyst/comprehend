(ns comprehend.engine
  (:require [comprehend.tools :as ctools]
            [clojure.set :as set]
            [clojure.walk :as w]
            [clojure.tools.trace :refer [deftrace trace trace-ns]]))

(ctools/assert-notice)

;
; FIRST CLASS LOGICAL TERMS
;

(defprotocol Moo
  (equiv [_ o]))

(defprotocol ILogicalTerm
  (varname [_]))

(deftype Var [symb m]
  clojure.lang.IObj
  (withMeta [this m] (Var. symb m))
  (meta [this] m)
  Moo
  (equiv [this o]
         (= symb (varname o)))
  clojure.lang.IHashEq
  (hasheq [this] (hash symb))
  ILogicalTerm
  (varname [this]
           (assert symb)
           symb)
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
  (Var. symb {}))

(defn grounded? [x]
  (if (coll? x)
    (every? grounded? x)
    (not (varname x))))

;
; CONSTRAINT STRUCTURES
;

(defn- constraint-triple? [x]
  (and (vector? x)
       (= 3 (count x))
       (let [[a b c] x]
         (and (= :dom a)
              (coll? c)))))

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
  `[[:dom ~explanation #{}]])

(declare unify)

(defn unify-colls [coll* coll]
  {:pre [(coll? coll*)
         (coll? coll)
         (-> coll* map? not)
         (-> coll map? not)
         (grounded? coll)]
   :post [(every? constraint-triple? %)]}
  (map (fn [el*] [:dom el* coll])
       coll*))

(defn unify-sequentials [l* l]
  {:pre [(sequential? l*)
         (sequential? l)
         (grounded? l)
         (= (count l*) (count l))]
   :post [(every? constraint-triple? %)]}
  (mapcat unify l* l))

(defn unify-maps [m* m]
  {:pre [(map? m*)
         (map? m)
         (grounded? m)]
   :post [(every? constraint-triple? %)]}
  (map (fn [kv] [:dom kv (seq m)])
       (seq m*)))

(defn unify [x* x]
  {:pre [(grounded? x)]
   :post [(every? constraint-triple? %)]}
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
        :else [[:dom x* #{x}]]))

;
; OPERATIONS ON SEQUENCES OF CONSTRAINT TRIPLES
;

(defn t-transform [f constraints]
  {:pre [(fn? f)
         (every? constraint-triple? constraints)]
   :post [(every? constraint-triple? %)]}
  (mapcat (fn [[t x* x :as v]]
            (or (f t x* x)
                [v]))
          constraints))

(defn decompose-dom-terms [t x* dom]
  {:pre [(keyword? t)]
   :post [(every? constraint-triple? %)]}
  (when (and (= :dom t)
             (= 1 (count dom))
             (not (varname x*)))
    (unify x* (first dom))))

(defn extract-contradictory-literals [t x dom]
  (when (and (= :dom t)
             (-> x coll? not)
             (-> x varname not))
    (let [s (ctools/as-set dom)]
      (if (contains? s x)
        []
        [[:dom x #{}]]))))

(declare find-models)

(def find-and-index-models
  (ctools/memoize (fn [x* x ks]
                    (set/index (find-models x* x)
                               ks))))

(defn simplify-domains [t x* dom]
  (when (= :dom t)
    (let [{:keys [query const-map]} (generalize x*)]
      (assert query)
      (assert (map? const-map))
      (cond (varname query)
            nil ; merely need to test for consistency

            (not= x* query)
            [[:dom x* (as-> (find-and-index-models #{query}
                                                   dom
                                                   (keys const-map)) $
                            ($ const-map)
                            (map #(ctools/subst % query) $)
                            (set $))]]

            :else
            nil))))

;
; CONSTRAINT MAPS
;

(defn map-to-triples [m]
  {:pre [(map? m)]
   :post [(every? constraint-triple? %)]}
  (mapcat (fn [[t m2]]
            (map (fn [[k v]] [t k v])
                 m2))
          m))

(defn constraint-map? [x]
  (and (map? x)
       (->> x
            map-to-triples
            (every? constraint-triple?))))

(defn triples-to-map [triples]
  {:pre [(every? constraint-triple? triples)]
   :post [(constraint-map? %)]}
  (reduce (fn [m [t k v]]
            (let [r (ctools/partial-intersection (get-in m [t k]) v)]
              (assert r)
              (if (empty? r)
                (reduced {})
                (assoc-in m [t k] r))))
          {}
          triples))

(defn subst-known-values [m]
  {:pre [(constraint-map? m)]
   :post [(constraint-map? %)]}
  (let [values (->> m
                    :dom
                    (filter (fn [[k v]] (varname k)))
                    (filter (fn [[k v]] (= 1 (count v))))
                    (map (fn [[k v]] [k (first v)]))
                    (into {}))]
    {:dom (->> (:dom m)
               (map (fn [[k v]]
                      [(if (varname k)
                         k
                         (ctools/subst values k))
                       (ctools/subst values v)]))
               (into {}))}))

(defn develop1 [constraints]
  {:pre [(every? constraint-triple? constraints)]
   :post [(constraint-map? %)]}
  (->> constraints
       (t-transform decompose-dom-terms)
       (t-transform extract-contradictory-literals)
       (t-transform simplify-domains) ; XXX call this less often
       triples-to-map
       subst-known-values))

(def develop (ctools/fix (comp develop1 map-to-triples)))

;
; QUANTIFY OVER DOMAINS
;

(defn quantify1 [m]
  {:pre [(constraint-map? m)]
   :post [(every? (partial every? constraint-triple?) %)]}
  (let [[k dom :as kv]
        (->> (get m :dom {})
             (filter (fn [[k dom]] (> (count dom) 1)))
             (reduce (fn [[k1 dom1 :as kv1] [k2 dom2 :as kv2]]
                       (if (or (> (count dom1) (count dom2))
                               (nil? kv1))
                         (cond-> kv2
                                 (= (count dom2) 2) reduced)
                         kv1))
                     nil))
        triples (map-to-triples m)]
    (when kv
      (map (fn [v]
             (concat triples
                     [[:dom k #{v}]]))
           dom))))

(defn develop-all1 [metaverse]
  {:pre [(every? constraint-map? metaverse)]
   :post [(every? (partial every? constraint-triple?) %)]}
  (->> metaverse
       (map develop)
       (mapcat #(or (quantify1 %)
                    [(map-to-triples %)]))
       (filter (complement empty?))
       (map set)
       set))

; the develop* functions are currently not very efficient
(def develop-all (ctools/fix (comp develop-all1 (partial map triples-to-map))))

(defn model? [x]
  (and (map? x)
       (->> x keys (every? varname))))

(defn find-models [x* x]
  {:pre [(grounded? x)]
   :post [(every? model? %)]}
  (->> [[[:dom x* [x]]]]
       develop-all
       (map triples-to-map)
       (map #(->> %
                  :dom
                  ; (filter (fn [[k v]] (varname k)))
                  (filter (fn [[k v]] (= 1 (count v))))
                  (map (fn [[k v]] [k (first v)]))
                  (into {})))
       set))

(defn match-in [x* dom]
  {:pre [(coll? dom)
         (-> dom map? not)]
   :post [(coll? %)]}
  (->> (find-models #{x*} dom)
       (map #(ctools/subst % x*))))