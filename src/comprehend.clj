(ns comprehend
  (:require [clojure.walk :as w]
            [comprehend.engine :as ce]
            [comprehend.tools :as ct]
            [clojure.core.cache :as cache]))

(declare indexed-set? index unbound-symbols comprehend* up* top*)

;;
;; PUBLIC API
;;

(defprotocol CacheProtocolExtension
  (conjd-root-el [_ v])
  (disjd-root-el [_ v]))

; XXX there's more potential in these hooks
(extend-protocol CacheProtocolExtension
  clojure.core.cache.TTLCache
  (conjd-root-el [c v] c)
  (disjd-root-el [c v] c)
  clojure.core.cache.LUCache
  (conjd-root-el [c v] c)
  (disjd-root-el [c v] c)
  clojure.core.cache.LRUCache
  (conjd-root-el [c v] c)
  (disjd-root-el [c v] c)
  clojure.core.cache.FIFOCache
  (conjd-root-el [c v] c)
  (disjd-root-el [c v] c)
  clojure.core.cache.BasicCache
  (conjd-root-el [c v] (empty c))
  (disjd-root-el [c v] (empty c))
  clojure.core.cache.SoftCache
  (conjd-root-el [c v] c)
  (disjd-root-el [c v] c)
  clojure.core.cache.LIRSCache
  (conjd-root-el [c v] c)
  (disjd-root-el [c v] c))

(deftype Set [hs !cache markers]
  clojure.lang.IHashEq
  (hasheq [this] (hash-combine (.hasheq hs)
                               (.hasheq markers)))
  clojure.lang.Counted
  (count [this] (.count hs))
  clojure.lang.IPersistentSet
  (seq [this] (.seq hs))
  (cons [this o]
        (if (.contains hs o)
          this
          (Set. (.cons hs o)
                (atom (conjd-root-el @!cache o))
                (reduce (fn [m [k v]]
                          (assoc m k (conj v o)))
                        {}
                        markers))))
  (empty [this] (-> hs empty index))
  (equiv [this o] (or (identical? this o)
                      (and (indexed-set? o)
                           (.equiv hs (.-hs o))
                           (.equiv markers (.-markers o)))))
  (disjoin [this o] (Set. (.disjoin hs o)
                          (atom (disjd-root-el @!cache o))
                          (reduce (fn [m [k v]]
                                    (assoc m k (disj v o)))
                                  {}
                                  markers)))
  (contains [this o] (.contains hs o))
  (get [this o] (.get hs o))
  clojure.lang.IFn
  (invoke [this o] (.invoke hs o))
  clojure.lang.IObj
  (withMeta [this m] (Set. (.withMeta hs m)
                           !cache
                           markers))
  (meta [this] (.meta hs))
  Object
  (toString [this] (.toString hs))
  (equals [this o] (.equiv this o))
  (hashCode [this] (.hasheq this)))

(defn indexed-set? [x]
  (= Set (type x)))

(defn unindex [s]
  {:pre [(set? s)]
   :post [(set? %)
          (-> % indexed-set? not)]}
  (if (indexed-set? s)
    (.-hs s)
    s))

(defn index
  ([hs] (index hs (cache/soft-cache-factory {})))
  ([hs cache]
   {:pre [(set? hs)]
    :post [(indexed-set? %)]}
   (Set. (unindex hs) (atom cache) {})))

(defn indexed-set
  ([] (index #{}))
  ([& xs] (index (set xs))))

(defn mark [s & markers]
  {:pre [(indexed-set? s)]}
  (Set. (.-hs s)
        (.-!cache s)
        (reduce #(assoc %1 %2 #{})
                (.-markers s)
                markers)))

(defn unmark [s & markers]
  {:pre [(indexed-set? s)]}
  (Set. (.-hs s)
        (.-!cache s)
        (reduce dissoc (.-markers s) markers)))

(defn additions [s marker]
  {:pre [(indexed-set? s)]}
  (get (.-markers s) marker #{}))

(defmacro comprehend [& args]
  (comprehend* &env
               `(comp seq
                      (partial filter
                               (partial not= ::skip))
                      #(map (partial %1 %2) %3))
               args))

(defmacro rcomprehend [& args]
  (comprehend* &env reduce args))

(defmacro auto-comprehend [& args]
  `(comprehend ~@args
               ~(->> args
                     (unbound-symbols &env)
                     set
                     (map (juxt keyword symbol))
                     (into {}))))

(defmacro top [x]
  `(-> '~x ce/variable top* distinct))

(defmacro up
  ([x] `(up ~x 1))
  ([x n] `(-> '~x ce/variable (up* ~n) distinct)))

;;
;; PRIVATE FUNCTIONS
;;

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

(def ^{:dynamic true :private true} *scope*)

(defn top* [x]
  {:pre [(ce/varname x)]
   :post [(coll? %)]}
  (-> (*scope* x)
      meta
      :top))

(defn up* [x n]
  {:pre [(ce/varname x)]}
  (-> (*scope* x)
      meta
      :up
      (nth (dec n) nil)))

(defn- comprehend* [env f args]
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
        explicit-vars (set (unbound-symbols env patterns))
        marker-name (gensym "marker__")]
    `(let [~s-name ~s
           ~marker-name ~marker]
       (->> (let ~(->> explicit-vars
                       (mapcat (fn [x]
                                 [x (ce/variable x)]))
                       vec)
              ~(if marker
                 `(ce/forward-match-with (.-!cache ~s-name)
                                         [~@patterns]
                                         (.-hs ~s-name)
                                         (additions ~s-name ~marker-name))
                 `(ce/match-with (.-!cache ~s-name)
                                 [~@patterns]
                                 (.-hs ~s-name))))
            (~f (fn [~s-name constraints#]
                  (let [~(->> explicit-vars
                              (map (fn [x] [x (ce/variable x)]))
                              (into {}))
                        (ce/model-as-subst-map constraints#)]
                    (binding [*scope* constraints#] ; XXX add nested scoping
                      ~expr)))
                ~(if marker? ; XXX ...and using rcomprehend or let-syntax
                   `(mark ~s-name ~marker-name)
                   s-name))))))