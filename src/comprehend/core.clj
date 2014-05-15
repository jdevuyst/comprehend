(ns comprehend.core
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.protocols :as lp]
            [clojure.core.logic.pldb :as pldb]))

(declare conj* count* seq* get*)

(deftype Set [m idx]
  clojure.lang.IPersistentSet
  (seq [this] (seq* this)) ;ISeq seq()
  (count [this] (count* this)) ;int count()
  (cons [this o] (conj* this o)) ;IPersistentCollection cons(Object o)
  (empty [this] (indexed-set)) ;IPersistentCollection empty()
  (equiv [this o] (.equiv (seq this) o)) ;boolean equiv(Object o)
  (disjoin [this k] (assert false "not implemented")) ;IPersistentSet disjoin(Object key)
  (contains [this k] (some? (.get this k))) ;boolean contains(Object key)
  (get [this k] (get* this k)) ;Object get(Object key)
  )

(pldb/db-rel toplevel-pred ^:index v)
(pldb/db-rel set-element-rel ^:index s ^:index v)
(pldb/db-rel list-element-rel ^:index l ^:index i ^:index v)
(pldb/db-rel map-element-rel ^:index m ^:index k ^:index v)
(pldb/db-rel list-count-rel ^:index l ^:index c)

(letfn [(runtime-sequential? [x]
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
  (defn- extract-unbound-symbols [x]
    (->> x
         squach
         (filter symbol?)
         (filter (comp nil? resolve)))))

(defn describe [ren make-rel x]
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

(defmacro comprehend [s & rdecl]
  (assert (>= (count rdecl) 2) "syntax: (comprehend s pattern+ expr)")
  (let [patterns (butlast rdecl)
        expr (last rdecl)
        explicit-vars (-> patterns extract-unbound-symbols set)
        ren-name (gensym "ren__")
        make-rel-name (gensym "make-rel__")]
    `(let [s# ~s]
       (for [[~@explicit-vars]
             (map #(map (partial (.-m s#)) %)
                  (pldb/with-db
                    (.-idx s#)
                    ; bogus var required when explicit-vars is empty
                    (l/run* [~@explicit-vars bogus-var#]
                            (let [~ren-name (memoize #(cond (~explicit-vars %) %
                                                            (coll? %) (l/lvar)
                                                            :else (hash %)))
                                  ~make-rel-name (fn [f# & args#]
                                                   (apply f# args#))]
                              ~@(for [p patterns]
                                  `(fn [a#]
                                     (reduce lp/bind
                                             a#
                                             (describe ~ren-name ~make-rel-name ~p)))))
                            (l/== bogus-var# nil))))]
         ~expr))))

(defn count* [s]
  (assert false "not implemented"))

(defn seq* [s]
  (first (comprehend s x x)))

(defn get* [s k]
  (first (comprehend s k k)))