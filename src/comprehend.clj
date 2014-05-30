(ns comprehend
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.protocols :as lp]
            [clojure.core.logic.pldb :as pldb]
            [clojure.walk :as w]))

;;
;; PUBLIC API
;;

(declare indexed-set indexed-set?
         roots conj* disj* unbound-symbols describe annotate-ungrounded-terms retract-marks comprehend*)

(deftype Set [m idx markers meta]
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
                           (.equiv (.-idx this) (.-idx o))
                           (.equiv (.-markers this) (.-markers o)))))
  (disjoin [this k] (disj* this k))
  (contains [this k] (-> this roots (contains? (-> k hash list))))
  (get [this k] (if (.contains this k)
                  ((.-m this) (hash k))))
  clojure.lang.IFn
  (invoke [this k] (.get this k))
  clojure.lang.ILookup
  (valAt [this k] (.get this k))
  clojure.lang.IObj
  (withMeta [this m] (Set. (.-m this) (.-idx this) (.-markers this) m))
  (meta [this] (.-meta this))
  Object
  (toString [this] (-> this set .toString))
  (equals [this o] (.equiv this o))
  (hashCode [this] (.hasheq this)))

(defn indexed-set
  ([] (Set. {} pldb/empty-db #{} {}))
  ([o] (conj* (indexed-set) o))
  ([o & os] (reduce conj* (indexed-set o) os)))

(defn indexed-set? [x]
  (= Set (type x)))

(defmacro comprehend [& args]
  `(comprehend* (comp seq
                      (partial filter
                               (comp not (partial = ::skip)))
                      #(map (partial %1 %2) %3))
                ~@args))

(defmacro rcomprehend [& args]
  `(comprehend* reduce
                ~@args))

(defmacro auto-comprehend [& args]
  `(comprehend ~@args
               ~(->> args
                     (unbound-symbols &env)
                     set
                     (map (juxt keyword symbol))
                     (into {}))))

(defn mark [s & markers]
  (Set. (.-m s)
        (retract-marks s markers)
        (into (.-markers s) markers)
        (.-meta s)))

(defn unmark [s & markers]
  (Set. (.-m s)
        (retract-marks s markers)
        (reduce disj (.-markers s) markers)
        (.-meta s)))

(defn fix [f]
  #(let [v (f %)]
     (if (= % v)
       v
       (recur v))))

(defmacro fixpoint [[name val] expr]
  `(loop [~name ~val
          new-val# ~expr]
     (if (= ~name new-val#)
       ~name
       (let [~name new-val#]
         (recur new-val# ~expr)))))

;;
;; PRIVATE FUNCTIONS
;;

(pldb/db-rel roots-rel ^:index v)
(pldb/db-rel marks-rel ^:index m ^:index v)
(pldb/db-rel set-element-rel ^:index s ^:index v)
(pldb/db-rel list-element-rel ^:index l ^:index i ^:index v)
(pldb/db-rel map-element-rel ^:index m ^:index k ^:index v)
(pldb/db-rel list-count-rel ^:index l ^:index c)

(defn- roots [s]
  (-> roots-rel pldb/rel-key ((.-idx s)) ::pldb/unindexed))

(letfn [(qsymb? [x] (and (= 2 (count x))
                         (= (first x) 'quote)
                         (symbol? (second x))))

        (squach [x]
                (cond (and (coll? x) (not (qsymb? x))) (mapcat squach x)
                      (map? x) (->> x
                                    ((juxt keys vals))
                                    concat
                                    (mapcat squach))
                      :else [x]))]
  (defn- symbols [env x]
    (filter symbol? (squach x))))

(letfn [(bound? [env x]
                (or (contains? env x) (resolve x)))]
  (defn- unbound-symbols [env x]
    (filter (complement (partial bound? env))
            (symbols env x))))

(defn describe
  "Describe is a public function for technical reasons. Do not use directly."
  [markers ren make-rel x]
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
    (concat [(make-rel roots-rel (ren x))]
            (map (fn [marker] (make-rel marks-rel marker (ren x))) markers)
            (f x))))

(defn- annotate-ungrounded-terms [var? p]
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

(defn- conj* [s o]
  (if (contains? s o)
    s
    (let [!m (atom (transient (.-m s)))
          ren (fn [o]
                (swap! !m assoc! (hash o) o)
                (hash o))
          facts (doall (describe (.-markers s) ren vector o))]
      (Set. (persistent! @!m)
            (apply pldb/db-facts (.-idx s) facts)
            (.-markers s)
            (.-meta s)))))

(defn- disj* [s o]
  (if-not (contains? s o)
    s
    (let [!subterms (atom #{})
          o-facts (->> o
                       (describe (.-markers s)
                                 (fn [o]
                                   (swap! !subterms conj (hash o))
                                   (hash o))
                                 vector)
                       set)
          pinned-facts (->> (pldb/with-db
                              (.-idx s)
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
                                      (or (not (@!subterms l))
                                          (and (= l r) (not= l (hash o))))))
                            (map second)
                            (map (partial (.-m s)))
                            (mapcat (partial describe
                                             (.-markers s)
                                             (fn [o]
                                               (swap! !subterms disj (hash o))
                                               (hash o))
                                             vector))
                            doall)]
      (Set. (reduce dissoc (.-m s) @!subterms)
            (reduce (partial apply pldb/db-retraction)
                    (.-idx s)
                    (reduce disj o-facts pinned-facts))
            (.-markers s)
            (.-meta s)))))

(defmacro comprehend* [f & args]
  (let [marker? (= :mark (first args))
        [marker rdecl] (if marker?
                         [(second args) (drop 2 args)]
                         [nil args])
        [s-name s] (if (-> rdecl first vector?)
                     [(-> rdecl first first)
                      (-> rdecl first second)]
                     [(gensym "s__") (first rdecl)])
        rdecl (rest rdecl)
        patterns (butlast rdecl)
        expr (last rdecl)
        explicit-vars (->> patterns (unbound-symbols &env) set)
        patterns (map (partial annotate-ungrounded-terms explicit-vars) patterns)
        marker-name (gensym "marker__")
        ren-name (gensym "ren__")]
    `(let [~s-name ~s
           ~marker-name ~marker]
       (->> (pldb/with-db
              (.-idx ~s-name)
              (l/run* [~@explicit-vars q#]
                      (let [~ren-name (memoize #(cond (~explicit-vars %) %
                                                      (and (coll? %)
                                                           (-> % meta ::opaque not)) (l/lvar)
                                                      :else (hash %)))
                            make-goal# (fn [f# & args#]
                                         (apply f# args#))]
                        (fn [a#]
                          (->> [~@patterns]
                               (mapcat (partial describe nil ~ren-name make-goal#))
                               ~@(if marker
                                   [`(cons (l/conde ~@(for [pattern patterns]
                                                        `[(marks-rel ~marker-name (~ren-name ~pattern))])))])
                               (reduce lp/bind a#))))))
            (map #(map (.-m ~s-name) %))
            (~f (fn [~s-name [~@explicit-vars]] ~expr)
                ~(if marker?
                   `(mark ~s-name ~marker-name)
                   s-name))))))

(defn- retract-marks [s markers]
  (->> markers
       (mapcat #(for [x (pldb/with-db (.-idx s)
                                      (l/run* [x]
                                              (marks-rel % x)))]
                  [% x]))
       (map (fn [[marker x]]
              [marks-rel marker x]))
       (reduce (partial apply pldb/db-retraction)
               (.-idx s))))