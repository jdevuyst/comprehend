(ns comprehend
  (:require [clojure.walk :as w]
            [comprehend.engine :as ce]
            [comprehend.tools :as ct]
            [clojure.core.cache :as cache]))

(ct/assert-notice)

(declare indexed-set indexed-set?
         unbound-symbols describe annotate-ungrounded-terms
         comprehend* cursor up* top* close-update-map)

;;
;; PUBLIC API
;;

(defprotocol CacheProtocolExtension
  (conjd-root-el [_ v])
  (disjd-root-el [_ v]))

(extend-type clojure.core.cache.SoftCache
  CacheProtocolExtension
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
  (cons [this o] (Set. (.cons hs o)
                       (atom (conjd-root-el @!cache o))
                       (reduce (fn [m [k v]]
                                 (assoc m k (conj v o)))
                               {}
                               markers)))
  (empty [this] (indexed-set))
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
  ; clojure.lang.ILookup
  ; (valAt [this k] (.get hs k))
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

(defn index ; XXX new
  ([hs] (index hs (cache/soft-cache-factory {})))
  ([hs cache]
   {:pre [(set? hs)]
    :post [(indexed-set? %)]}
   (Set. hs (atom cache) {})))

(defn unindex [s] ; XXX new
  {:pre [(indexed-set? s)]
   :post [(set? s)]}
  (.-hs s))

(defn indexed-set
  ([] (index #{}))
  ([& xs] (index (set xs))))

(defn mark [s & markers]
  (Set. (.-hs s)
        (.-!cache s)
        (reduce #(assoc %1 %2 #{})
                (.-markers s)
                markers)))

(defn unmark [s & markers]
  (Set. (.-hs s)
        (.-!cache s)
        (reduce dissoc (.-markers s) markers)))

(defmacro comprehend [& args]
  (comprehend* &env
               `(comp seq
                      (partial filter
                               (comp not (partial = ::skip)))
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

(defn- annotate-ungrounded-terms [var? p] ; XXX delete this?
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

(def ^{:dynamic true :private true} *scope*)

(defn up* [x]
  {:pre [(ce/varname x)]}
  (->> (*scope* x)
       meta
       :top))

(defn top* [x]
  {:pre [(ce/varname x)]}
  {:post [x]}
  (->> (*scope* x)
       meta
       :top))

(defmacro up
  ([x] `(up* (ce/variable '~x)))
  ([x n] '[:up-n-not-implemented]))

(defmacro top [x]
  `(top* (ce/variable '~x)))

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
        explicit-vars (->> patterns (unbound-symbols env) set)
        patterns (map (partial annotate-ungrounded-terms explicit-vars) patterns)
        marker-name (gensym "marker__")
        ren-name (gensym "ren__")]
    `(let [~s-name ~s
           ~marker-name ~marker]
       (->> (let ~(->> explicit-vars
                       (mapcat (fn [x]
                                 [x (ce/variable x)]))
                       vec)
              (ce/match-with (.-!cache ~s-name)
                             [~@patterns]
                             (.-hs ~s-name)))
            (~f (fn [~s-name constraints#]
                  (let [~(->> explicit-vars
                              (map (fn [x] [x (ce/variable x)]))
                              (into {}))
                        (ce/constraints-as-model constraints#)]
                    (binding [*scope* constraints#]
                      ~expr)))
                ~s-name)))))
; ~(if marker? ; and [either using rcomprehend or using let syntax] ; TODO optimize further
;    `(mark ~s-name ~marker-name)
;    s-name))))))