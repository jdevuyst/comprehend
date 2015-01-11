(ns comprehend.engine
  (:require [comprehend.tools :as ct]
            [clojure.set :as set]
            [clojure.walk :as w]
            [clojure.core.reducers :as r]
            [fletching.macros :refer :all]))

;;
;; DOMAIN METADATA
;;

(comment
  (defn- valid-md? [md]
    (or (nil? md)
        (and (map? md)
             (or (-> md :top nil?)
                 (-> md :top coll?))
             (or (-> md :up nil?)
                 (and (-> md :up coll?)
                      (every? coll? (:up md))))))))

(defn- merge-md [md1 md2]
  (comment {:pre [(valid-md? md1)
                  (valid-md? md2)]
            :post [(valid-md? %)]})
  {:top (set/union (:top md1)
                   (:top md2))
   :up (let [seq1 (:up md1)
             seq2 (:up md2)
             [seq1 seq2] (if (> (count seq1) (count seq2))
                           [seq1 seq2]
                           [seq2 seq1])
             seq2 (concat seq2 (repeat nil))]
         (map set/union seq1 seq2))
   :simplified (set/union (:simplified md1)
                          (:simplified md2))})

(defn- upmd [old-meta new-parent]
  (comment {:pre [(valid-md? old-meta)
                  (coll? new-parent)
                  (not= clojure.lang.MapEntry (type new-parent))]
            :post [(valid-md? %)]})
  (let [new-parent (with-meta new-parent old-meta)]
    {:top (or (:top old-meta) [new-parent])
     :up (cons [new-parent] (:up old-meta))
     :simplified (:simplified old-meta)}))

;;
;; FIRST CLASS LOGICAL TERMS
;;

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
  (comment {:pre [symb]
            :post [(varname %)]})
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
  (let [!consts (volatile! {})
        !vars (volatile! {})
        !counter (volatile! 0)
        f (fn [!m y]
            (or (@!m y)
                (let [y* (variable [::temp (vswap! !counter inc)])]
                  (vswap! !m assoc y y*)
                  y*)))]
    {:query (w/prewalk #(cond (varname %) (f !vars %)
                              (grounded? %) (f !consts %)
                              :else %)
                       x*)
     :const-map (->> @!consts
                     (r/map (fn [[k v]] [v #{k}]))
                     (into {}))}))

;;
;; CONSTRAINT STRUCTURES
;;

(comment
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
         (every? (partial = 1) (map count (vals x))))))

(defn merge-doms [coll1 coll2]
  (comment {:pre [(or (nil? coll1) (coll? coll1))
                  (or (nil? coll2) (coll? coll2))]
            :post [(coll? %)]})
  (with-meta (ct/partial-intersection coll1 coll2)
             (merge-md (meta coll1) (meta coll2))))

(defn constraints-as-mmap [constraints]
  (comment {:pre [(r/reduce (fn f
                              ([b x y] (f b [x y]))
                              ([b c] (and b (constraint-pair? c))))
                            true
                            constraints)]
            :post [(constraint-map? %)]})
  (->> constraints
       (r/reduce (fn f
                   ([m [k v]]
                    (f m k v))
                   ([m k v]
                    (let [r (merge-doms (m k) v)]
                      (if (empty? r)
                        (reduced (transient {:inconsistent #{}}))
                        (assoc! m k r)))))
                 (transient {}))
       persistent!))

(defn model-as-subst-map [model]
  (comment {:pre [(model? model)]
            :post [(map? %)]})
  (->> model
       (r/reduce (fn f
                   ([m [k v]] (f m k v))
                   ([m k v] (assoc! m k (first v))))
                 (transient {}))
       persistent!))

;;
;; (PARTIAL) UNIFICATION WITH STRUCTURES
;;

(defn unification-type [x]
  (cond (map? x) :map
        (sequential? x) [:list-like (count x)]
        (coll? x) :set-like
        :else :not-a-coll))

(def ^:private falsum [:inconsistent #{}])

(defn unify [md x* x]
  (comment {:pre [(grounded? x)]
            :post [(constraint-coll? %)]})
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
                            (upmd md [(key x) (val x)])
                            (upmd md x)))
                  x*
                  x))))

;;
;; OPERATIONS ON CONSTRAINT COLLECTIONS
;;

(defn decompose-dom-terms [[x* dom :as constraint]]
  (comment {:pre [(coll? dom)]
            :post [(constraint-coll? %)]})
  (when (and (= 1 (count dom))
             (not (varname x*)))
    (unify (meta dom)
           x*
           (first dom))))

(defn extract-contradictory-literals [[x* dom :as constraint]]
  (comment {:pre [(coll? dom)]
            :post [(constraint-coll? %)]})
  (when (and (not= falsum constraint)
             (-> x* coll? not)
             (-> x* varname not))
    (if (contains? (set dom) x*)
      []
      [falsum])))

(declare indexed-match-in)

(defn simplify-domains [!cache
                        known-values
                        [x* dom :as constraint]]
  (comment {:pre [(coll? dom)]
            :post [(constraint-coll? %)]})
  (let [{:keys [query const-map]} (generalize (ct/subst known-values x*))]
    (comment assert query)
    (comment assert (model? const-map))
    (when (and (-> query varname not)
               (-> const-map count pos?)
               (-> dom meta :simplified (get x*) not))
      [[x* (-> (ct/memoized !cache
                            indexed-match-in
                            !cache
                            query
                            dom
                            (keys const-map))
               (>< const-map)
               (>> r/mapcat #(r/mapcat (comp :top meta)
                                       (vals %)))
               (>> into #{})
               (with-meta (assoc (meta dom) :simplified #{x*})))]])))

(defn collect-known-values [constraints]
  (->> constraints
       (r/filter (fn [[k v]] (varname k)))
       (r/filter (fn [[k v]] (= 1 (count v))))
       (r/map (fn [[k v]] [k (first v)]))
       (into {})))

(defn develop1 [!cache constraints]
  (comment {:pre [(constraint-coll? constraints)]
            :post [(constraint-map? %)]})
  (->> constraints
       (ct/cmapcat decompose-dom-terms)
       (ct/cmapcat extract-contradictory-literals)
       (ct/cmapcat (partial simplify-domains
                            !cache
                            (collect-known-values constraints)))
       constraints-as-mmap))

(defn develop [!cache constraints]
  ((ct/cfix (partial develop1 !cache)) constraints))

(defn quantify1 [m]
  (comment {:pre [(constraint-map? m)]
            :post [(every? (partial constraint-coll?) %)]})
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
    (when kv
      (map (fn [v]
             (concat m
                     [[k (with-meta #{v}
                                    (meta dom))]]))
           dom))))

;;
;; OPERATIONS ON METAVERSES OF CONSTRAINT COLLECTIONS
;;

(defn develop-all1 [!cache metaverse]
  (comment {:pre [(every? constraint-coll? metaverse)]
            :post [(set? %)
                   (every? (partial constraint-map?) %)]})
  (->> metaverse
       vec

       (r/map (partial develop !cache))
       (ct/cmapcat quantify1)
       (r/filter (comp not :inconsistent))
       (r/map constraints-as-mmap)

       (r/fold (inc (/ (count metaverse)
                       (.. Runtime getRuntime availableProcessors)
                       4))
               set/union
               conj)))

(defn develop-all [!cache metaverse]
  ((ct/cfix (partial develop-all1 !cache)) metaverse))

;;
;; FINDING MODELS
;;

(defn match-in [!cache x* dom]
  (comment {:pre [(coll? dom)
                  (grounded? dom)]
            :post [(every? model? %)]})
  (develop-all !cache [[[x* dom]]]))

(defn indexed-match-in [!cache x* dom ks]
  (set/index (match-in !cache x* (with-meta dom nil))
             ks))

(defn match-with [!cache xs* dom]
  (comment {:pre [(coll? xs*)
                  (coll? dom)
                  (grounded? dom)]
            :post [(every? model? %)]})
  (develop-all !cache [(map (fn [x*] [x* dom]) xs*)]))

(defn forward-match-with [!cache xs* dom narrowed-dom]
  (comment {:pre [(coll? xs*)
                  (coll? dom)
                  (grounded? dom)
                  (coll? narrowed-dom)
                  (set/subset? narrowed-dom dom)]
            :post [(every? model? %)]})
  (let [step1 (->> xs*
                   (map (fn [x*] [x* (develop !cache [[x* dom]])]))
                   (into {}))]
    (develop-all !cache
                 (for [x* xs*]
                   (->> (dissoc step1 x*)
                        vals
                        (mapcat identity)
                        (cons [x* narrowed-dom]))))))