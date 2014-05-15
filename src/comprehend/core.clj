(ns comprehend.core
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.protocols :as lp]
            [clojure.core.logic.pldb :as pldb]))

(declare indexed-set? conj* indexed-set disj* count* seq* contains?* get* equiv?*)

(deftype Set [m idx]
  clojure.lang.IHashEq
  (hasheq [this] (-> this .-idx hash))
  clojure.lang.IPersistentSet
  (seq [this] (seq* this))
  (count [this] (count* this))
  (cons [this o] (conj* this o))
  (empty [this] (indexed-set))
  (equiv [this o] (or (identical? this o)
                      (and (indexed-set? o)
                           (.equiv (.-idx this) (.-idx o)))))
  (disjoin [this k] (disj* this k))
  (contains [this k] (contains?* this k))
  (get [this k] (get* this k)))

(defn indexed-set? [x]
  (= Set (type x)))

(pldb/db-rel toplevel-pred ^:index v)
(pldb/db-rel set-element-rel ^:index s ^:index v)
(pldb/db-rel list-element-rel ^:index l ^:index i ^:index v)
(pldb/db-rel map-element-rel ^:index m ^:index k ^:index v)
(pldb/db-rel list-count-rel ^:index l ^:index c)

(letfn [(bound? [env x]
                (or (contains? env x)
                    (resolve x)))
        (runtime-sequential? [x]
                             (and (sequential? x)
                                  (not= [(first x) (symbol? (second x)) (count x)]
                                        ['quote true 2])))
        (squach [x]
                (cond (or (set? x) (runtime-sequential? x)) (mapcat squach x)
                      (map? x) (->> x
                                    ((juxt keys vals))
                                    concat
                                    (mapcat squach))
                      :else [x]))]
  (defn- extract-unbound-symbols [env x]
    (->> x
         squach
         (filter symbol?)
         (filter #(not (bound? env %))))))

(defn describe
  "Describe is a public function for technical reasons. Do not use directly."
  [ren make-rel x]
  (letfn [(f [x]
             (cond (sequential? x)
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
    (cons (make-rel toplevel-pred (ren x))
          (f x))))

(defn- conj* [s o]
  (let [!m (atom (transient (.-m s)))
        ren (fn [o]
              (swap! !m assoc! (hash o) o)
              (hash o))
        facts (describe ren vector o)]
    (Set. (persistent! @!m)
          (apply pldb/db-facts (.-idx s) facts))))

(defn indexed-set
  ([] (Set. {} pldb/empty-db))
  ([o] (conj* (indexed-set) o))
  ([o & os] (reduce conj* (indexed-set o) os)))

(defn- disj* [s o] ; TO DO: disj* is currently leaky!
  (Set. (.-m s)
        (pldb/db-retraction (.-idx s) toplevel-pred (hash o))))

(defmacro comprehend [s & rdecl]
  (assert (>= (count rdecl) 2) "syntax: (comprehend s pattern+ expr)")
  (let [patterns (butlast rdecl)
        expr (last rdecl)
        explicit-vars (->> patterns (extract-unbound-symbols &env) set)]
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
                                                       (coll? %) (l/lvar)
                                                       :else (hash %)))
                                  make-goal# (fn [f# & args#]
                                               (apply f# args#))]
                              (fn [a#]
                                (->> [~@patterns]
                                     (mapcat (partial describe ren# make-goal#))
                                     (reduce lp/bind a#))))
                            (l/== bogus-var# nil))))]
         ~expr))))

(defn- count* [s]
  (count (seq s))) ; TO DO: make more efficient!

(defn- seq* [s]
  (comprehend s x x))

(defn- contains?* [s k]
  (-> (comprehend s k k) empty? not))

(defn- get* [s k]
  (first (comprehend s k k)))