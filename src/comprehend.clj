(ns comprehend
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.protocols :as lp]
            [clojure.core.logic.pldb :as pldb]
            [clojure.walk :as w]))

(declare indexed-set? conj* indexed-set disj*)

; The following relations are subject to change in future versions
(pldb/db-rel roots-rel ^:index v)
(pldb/db-rel set-element-rel ^:index s ^:index v)
(pldb/db-rel list-element-rel ^:index l ^:index i ^:index v)
(pldb/db-rel map-element-rel ^:index m ^:index k ^:index v)
(pldb/db-rel list-count-rel ^:index l ^:index c)

(defn- roots [s]
  (-> roots-rel pldb/rel-key ((.-idx s)) ::pldb/unindexed))

(deftype Set [m idx]
  clojure.lang.IHashEq
  (hasheq [this] (-> this .-idx hash))
  clojure.lang.Counted
  (count [this] (-> this roots count))
  clojure.lang.IPersistentSet
  (seq [this] (map (comp (.-m this) first) (roots this)))
  (cons [this o] (conj* this o))
  (empty [this] (indexed-set))
  (equiv [this o] (or (identical? this o)
                      (and (indexed-set? o)
                           (.equiv (.-idx this) (.-idx o)))))
  (disjoin [this k] (disj* this k))
  (contains [this k] (-> this roots (.contains (-> k hash list))))
  (get [this k] (if (.contains this k)
                  ((.-m this) (hash k))))
  clojure.lang.IFn
  (invoke [this k] (.get this k))
  clojure.lang.ILookup
  (valAt [this k] (.get this k))
  Object
  (toString [this] (-> this set .toString))
  (equals [this o] (.equiv this o))
  (hashCode [this] (.hasheq this)))

(defn indexed-set? [x]
  (= Set (type x)))

(letfn [(bound? [env x]
                (or (contains? env x) (resolve x)))
        (qsymb? [x] (and (= 2 (count x))
                         (= (first x) 'quote)
                         (symbol? (second x))))
        (squach [x]
                (cond (and (coll? x) (not (qsymb? x))) (mapcat squach x)
                      (map? x) (->> x
                                    ((juxt keys vals))
                                    concat
                                    (mapcat squach))
                      :else [x]))]
  (defn- unbound-symbols [env x]
    (->> x
         squach
         (filter symbol?)
         (filter #(not (bound? env %))))))

(defn describe
  "Describe is a public function for technical reasons. Do not use directly."
  [ren make-rel x]
  (letfn [(f [x]
             (cond (-> x meta ::opaque)
                   nil

                   (sequential? x)
                   (cons (make-rel list-count-rel (ren x) (count x))
                         (mapcat (fn [i v]
                                   (cons (make-rel list-element-rel (ren x) i (ren v))
                                         (f v)))
                                 (iterate inc 0)
                                 x))

                   (map? x)
                   (mapcat (fn [[k v]]
                             (cons (make-rel map-element-rel (ren x) (ren k) (ren v))
                                   (concat (f k)
                                           (f v))))
                           x)

                   (coll? x)
                   (mapcat (fn [v]
                             (cons (make-rel set-element-rel (ren x) (ren v))
                                   (f v)))
                           x)

                   :else
                   nil))]
    (cons (make-rel roots-rel (ren x))
          (f x))))

(defn- conj* [s o]
  (let [!m (atom (transient (.-m s)))
        ren (fn [o]
              (swap! !m assoc! (hash o) o)
              (hash o))
        facts (doall (describe ren vector o))]
    (Set. (persistent! @!m)
          (apply pldb/db-facts (.-idx s) facts))))

(defn indexed-set
  ([] (Set. {} pldb/empty-db))
  ([o] (conj* (indexed-set) o))
  ([o & os] (reduce conj* (indexed-set o) os)))

(defn- disj* [s o]
  (let [!subterms (atom #{})
        o-facts (->> o
                     (describe (fn [o]
                                 (swap! !subterms conj (hash o))
                                 (hash o))
                               vector)
                     set)
        pinned-facts (->> (pldb/with-db (.-idx s)
                                        (l/run* [x y]
                                                (l/membero y (seq @!subterms))
                                                (l/conde [(roots-rel y) (l/== x y)]
                                                         [(l/fresh [tmp]
                                                                   (l/conde [(set-element-rel x y)]
                                                                            [(list-element-rel x tmp y)]
                                                                            [(map-element-rel x tmp y)]
                                                                            [(map-element-rel x y tmp)]))])))
                          set
                          (filter (fn [[l r]]
                                    (or (not (contains? @!subterms l))
                                        (and (= l r) (not= l (hash o))))))
                          (map second)
                          (map (partial (.-m s)))
                          (mapcat (partial describe
                                           (fn [o]
                                             (swap! !subterms disj (hash o))
                                             (hash o))
                                           vector))
                          doall)]
    (Set. (reduce dissoc (.-m s) @!subterms)
          (reduce (partial apply pldb/db-retraction)
                  (.-idx s)
                  (reduce disj o-facts pinned-facts)))))

(defn- mark-ungrounded-terms [var? p]
  (w/postwalk (fn [x]
                (if (coll? x)
                  (with-meta x
                             {::opaque (->> x
                                            (filter #(or (var? %)
                                                         (and (coll? %)
                                                              (-> % meta ::opaque not))))
                                            empty?)})
                  x))
              p))

(defmacro comprehend [s & rdecl]
  (assert (>= (count rdecl) 2) "syntax: (comprehend s pattern+ expr)")
  (let [patterns (butlast rdecl)
        expr (last rdecl)
        explicit-vars (->> patterns (unbound-symbols &env) set)
        patterns (map (partial mark-ungrounded-terms explicit-vars) patterns)]
    `(let [s# ~s
           lookup# #(map (partial (.-m s#)) %)]
       (for [[~@explicit-vars]
             (map lookup#
                  (pldb/with-db
                    (.-idx s#)
                    ; bogus var required when explicit-vars is empty
                    ; TO DO: should get rid of run* instead
                    (l/run* [~@explicit-vars bogus-var#]
                            (let [ren# (memoize #(cond (~explicit-vars %) %
                                                       (and (coll? %)
                                                            (-> % meta ::opaque not)) (l/lvar)
                                                       :else (hash %)))
                                  make-goal# (fn [f# & args#]
                                               (apply f# args#))]
                              (fn [a#]
                                (->> [~@patterns]
                                     (mapcat (partial describe ren# make-goal#))
                                     (reduce lp/bind a#))))
                            (l/== bogus-var# nil))))]
         ~expr))))
