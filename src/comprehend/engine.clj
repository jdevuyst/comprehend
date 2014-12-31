(ns comprehend.engine
  (:require [comprehend.tools :as ct]
            [clojure.set :as set]
            [clojure.walk :as w]
            [clojure.core.reducers :as r]))

(ct/assert-notice)

;;
;; FIRST CLASS LOGICAL TERMS

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

; XXX currently not fully extensional:
; (generalize #{a b}) = (generalize #{c d})
; iff (compare (hash a) (hash b)) = (compare (hash c) (hash d))
; (de Bruijn indices do not appear to help with this)
(defn generalize [x*]
  (let [!consts (atom {})
        !vars (atom {})
        !counter (atom 0)
        f (fn [!m y]
            (or (@!m y)
                (let [y* (variable [::temp (swap! !counter inc)])]
                  (swap! !m assoc y y*)
                  y*)))]
    {:query (w/prewalk #(cond (varname %) (f !vars %)
                              (grounded? %) (f !consts %)
                              :else %)
                       x*)
     :const-map (->> @!consts
                     (r/map (fn [[k v]] [v #{k}]))
                     (into {}))}))

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

(defn model? [x]
  (and (constraint-map? x)
       (every? varname (keys x))
       (every? (partial = 1) (map count (vals x)))))

(defn merge-doms [coll1 coll2]
  {:pre [(or (nil? coll1) (coll? coll1))
         (or (nil? coll2) (coll? coll2))]
   :post [(coll? %)]}
  (with-meta (ct/partial-intersection coll1 coll2)
             {:top (set/union (-> coll1 meta :top)
                              (-> coll2 meta :top))
              :up (set/union (-> coll1 meta :up)
                             (-> coll2 meta :up))}))

(defn constraints-as-mmap [constraints]
  {:pre [(r/reduce (fn f
                     ([b x y] (f b [x y]))
                     ([b c] (and b (constraint-pair? c))))
                   true
                   constraints)]
   :post [(constraint-map? %)]}
  (->> constraints
       (r/reduce (fn f
                   ([m [k v]]
                    (f m k v))
                   ([m k v]
                    (let [r (merge-doms (m k) v)]
                      (if (empty? r)
                        (reduced (transient {}))
                        (assoc! m k r)))))
                 (transient {}))
       persistent!))

(defn model-as-subst-map [model]
  {:pre [(model? model)]
   :post [(map? %)]}
  (->> model
       (r/reduce (fn f
                   ([m [k v]] (f m k v))
                   ([m k v] (assoc! m k (first v))))
                 (transient {}))
       persistent!))

;;
;; (PARTIAL) UNIFICATION WITH STRUCTURES
;;

(def ^:private falsum [(variable :falsum) #{}])

(defn unification-type [x]
  (cond (map? x) :map
        (sequential? x) [:list-like (count x)]
        (coll? x) :set-like
        :else :not-a-coll))

(defn- upmd [old-meta new-parent]
  {:pre [(or (nil? old-meta) (map? old-meta))
         (coll? new-parent)
         (not= clojure.lang.MapEntry (type new-parent))]
   :post [(map? %)
          (-> % :top coll?)
          (-> % :up coll?)]}
  (let [new-parent (with-meta new-parent old-meta)]
    {:top (or (:top old-meta) [new-parent])
     :up [new-parent]}))

(defn unify [md x* x]
  {:pre [(grounded? x)]
   :post [(constraint-coll? %)]}
  (let [t (unification-type x*)]
    (cond (= :not-a-coll t)
          [[x* (with-meta #{x} md)]]

          (not= t (unification-type x))
          [falsum]

          (= :set-like t)
          (map (fn [el*] [el* (with-meta x (upmd md x))])
               x*)

          (= :map t)
          (map (fn [kv] [kv (with-meta (seq x) (upmd md x))])
               x*)

          :else ; list-like
          (mapcat unify
                  (repeat (if (= (type x) clojure.lang.MapEntry)
                            (upmd md (vec x))
                            (upmd md x)))
                  x*
                  x))))

;;
;; OPERATIONS ON CONSTRAINT COLLECTIONS
;;

(defn decompose-dom-terms [[x* dom :as constraint]]
  {:pre [(coll? dom)]
   :post [(constraint-coll? %)]}
  (if (and (= 1 (count dom))
           (not (varname x*)))
    (unify (meta dom)
           x*
           (first dom))
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

(defn simplify-domains [!cache
                        known-values
                        [x* dom :as constraint]]
  {:pre [(coll? dom)]
   :post [(constraint-coll? %)]}
  (let [{:keys [query const-map]} (generalize (ct/subst known-values x*))]
    (assert query)
    (assert (model? const-map))
    (if (and (-> query varname not)
             (-> const-map count pos?)
             (not= x* (-> dom meta ::simplified)))
      [[x* (as-> (ct/memoized !cache
                              indexed-match-in
                              !cache
                              query
                              dom
                              (keys const-map)) $
                 ($ const-map)
                 (r/mapcat #(r/mapcat (comp :top meta)
                                      (vals %))
                           $)
                 (into #{} $)
                 (with-meta $ (assoc (meta dom) ::simplified x*)))]]
      [constraint])))

(defn collect-known-values [constraints]
  (->> constraints
       (filter (fn [[k v]] (varname k)))
       (filter (fn [[k v]] (= 1 (count v))))
       (r/map (fn [[k v]] [k (first v)]))
       (into {})))

(defn develop1 [!cache constraints]
  {:pre [(constraint-coll? constraints)]
   :post [(constraint-map? %)]}
  (->> constraints
       (r/mapcat decompose-dom-terms)
       (r/mapcat extract-contradictory-literals)
       (r/mapcat (partial simplify-domains
                          !cache
                          (collect-known-values constraints)))
       constraints-as-mmap))

(defn develop [!cache constraints]
  ((ct/fix (partial develop1 !cache)) constraints))

(defn quantify1 [m]
  {:pre [(constraint-map? m)]
   :post [(every? (partial constraint-coll?) %)]}
  (let [[k dom :as kv]
        (->> m
             (r/filter (fn [[k dom]] (> (count dom) 1)))
             (r/reduce (fn [[k1 dom1 :as kv1] [k2 dom2 :as kv2]]
                         (if (or (> (count dom1) (count dom2))
                                 (nil? kv1))
                           (cond-> kv2
                                   (= (count dom2) 2) reduced)
                           kv1))
                       nil))]
    (if kv
      (map (fn [v]
             (concat m
                     [[k (with-meta #{v}
                                    (meta dom))]]))
           dom)
      [m])))

;;
;; OPERATIONS ON METAVERSES OF CONSTRAINT COLLECTIONS
;;

(defn develop-all1 [!cache metaverse]
  {:pre [(every? constraint-coll? metaverse)]
   :post [(set? %)
          (every? (partial constraint-map?) %)]}
  (->> metaverse

       (into [])

       (r/map (partial develop !cache))
       (r/mapcat quantify1)
       (r/filter not-empty)
       (r/map constraints-as-mmap)

       (r/fold 1
               (fn
                 ([] #{})
                 ([x y] (set/union x y)))
               (fn
                 ([] #{})
                 ([coll x] (conj coll x))))))

(defn develop-all [!cache metaverse]
  ((ct/fix (partial develop-all1 !cache)) metaverse))

;;
;; FINDING MODELS
;;

(defn match-with [!cache xs* dom]
  {:pre [(coll? xs*)
         (coll? dom)
         (grounded? dom)]
   :post [(every? model? %)]}
  (->> [(map (fn [x*] [x* dom]) xs*)]
       (develop-all !cache)))

(defn match-in [!cache x* dom]
  {:pre [(coll? dom)
         (grounded? dom)]
   :post [(every? model? %)]}
  (match-with !cache [x*] dom))

(defn indexed-match-in [!cache x* dom ks]
  (set/index (match-in !cache x* (with-meta dom nil))
             ks))