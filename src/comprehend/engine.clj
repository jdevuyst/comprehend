(ns comprehend.engine
  (:require [comprehend.tools :as ctools]
            [clojure.set :as set]
            [clojure.walk :as w]))

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
          ; (println :is-equal? this o (= symb (varname o)))
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
         (or (and (= :dom a)
                  (coll? c))
             (and (= :cont a)
                  (every? fn? c))
             (and (= :val a)
                  (varname b))))))

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

(defn unify-map-vals [m* m]
  {:pre [(map? m*)
         (map? m)
         (grounded? m)]
   :post [(every? constraint-triple? %)]}
  (mapcat (fn [[k* v*]]
            [[:cont k* [#(unify v* (m %))]]
             [:dom k* (keys m)]])
          m*))

(defn unify-map-keys [m* m]
  {:pre [(map? m*)
         (map? m)
         (grounded? m)]
   :post [(every? constraint-triple? %)]}
  (let [!rm (delay (ctools/memoized-reverse-map m))]
    (mapcat (fn [[k* v*]]
              [[:cont v* [#(unify #{k*} (@!rm %))]]
               [:dom v* (vals m)]])
            m*)))

(defn unify-maps [m* m]
  {:pre [(map? m*)
         (map? m)
         (grounded? m)]
   :post [(every? constraint-triple? %)]}
  (concat (unify-map-vals m* m)
          (unify-map-keys m* m)))

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

(defn materialize-grounded-continuations [t x* fs]
  {:pre [(keyword? t)]
   :post [(every? constraint-triple? %)]}
  (when (and (= :cont t)
             (grounded? x*))
    (mapcat #(% x*) fs)))

(defn extract-known-values [t x* dom]
  (when (and (= :dom t)
             (= 1 (count dom))
             (varname x*))
    [[:val x* (first dom)]]))

(let [bottom (variable 'bottom)]
  (defn extract-empty-domains [t x* dom]
    (when (and (= :dom t)
               (-> dom count zero?))
      [[:val bottom true] [:val bottom false]]))


  (defn extract-contradictory-literals [t x dom]
    (when (and (= :dom t)
               (grounded? x))
      (let [s (if (set? dom) dom (set dom))]
        (if (contains? s x)
          [[t x s]]
          [[:val bottom true] [:val bottom false]])))))

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
            (case t
              :val (condp = (get-in m [t k] ::not-found)
                     ::not-found (assoc-in m [t k] v)
                     v m
                     (reduced {}))
              (update-in m
                         [t k]
                         (case t
                           :cont set/union
                           :dom ctools/partial-intersection)
                         v)))
          {}
          triples))

(defn subst-known-values [m]
  {:pre [(constraint-map? m)]
   :post [(constraint-map? %)]}
  {:val (:val m)
   :cont (ctools/subst (:val m) (:cont m))
   :dom (ctools/subst (:val m) (:dom m))})

(defn develop1 [constraints]
  {:pre [(every? constraint-triple? constraints)]
   :post [(constraint-map? %)]}
  (->> constraints
       (t-transform decompose-dom-terms)
       (t-transform materialize-grounded-continuations)
       (t-transform extract-known-values)
       (t-transform extract-empty-domains)
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
                         (if (= (count dom2) 2)
                           (reduced kv2)
                           kv2)
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
(def develop-all
  (let [f (ctools/fix (comp develop-all1 (partial map triples-to-map)))]
    (fn [metaverse]
      (let [r (f metaverse)]
        (assert (every? #(-> % :cont empty?) r))
        r))))

(defn model? [x]
  (and (map? x)
       (->> x keys (every? varname))))

(defn find-models [x* x]
  {:pre [(grounded? x)]
   :post [(every? model? %)]}
  (->> [[[:dom x* [x]]]]
       develop-all
       (map triples-to-map)
       (map :val)
       set))

(defn match-in [x* dom]
  {:pre [(coll? dom)
         (-> dom map? not)]
   :post [(coll? %)]}
  (->> (find-models #{x*} dom)
       (map #(ctools/subst % x*))))