(ns comprehend.core
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

(defn ugly-fix-for-sets []
  (extend-protocol clojure.core.logic.protocols/IWalkTerm
    clojure.lang.IPersistentCollection
    (walk-term [v f]
               v)))

(declare empty-db-set into*)

(deftype Set [content]
  ; clojure.lang.IPersistentSet
  ; (seq [this] :x) ;ISeq seq()
  ; (count [this] :x) ;int count()
  ; (cons [this o] :x) ;IPersistentCollection cons(Object o)
  ; (empty [this] (empty-db-set)) ;IPersistentCollection empty()
  ; (equiv [this o] :x) ;boolean equiv(Object o)
  ; (disjoin [this k] :x) ;IPersistentSet disjoin(Object key)
  ; (contains [this k] (some? (get this k))) ;boolean contains(Object key)
  ; (get [this k] :x) ;Object get(Object key)
  )

(pldb/db-rel db-element-rel* ^:index v)
(pldb/db-rel set-element-rel* ^:index l ^:index i ^:index v)
(pldb/db-rel listlike-element-rel* ^:index l ^:index i ^:index v)
(pldb/db-rel map-element-rel* ^:index m ^:index k ^:index v)
(pldb/db-rel count-rel* ^:index l ^:index c)

(defprotocol IParseMode
  (listlike? [_ x])
  (db-element-rel [_])
  (set-element-rel [_])
  (listlike-element-rel [_])
  (map-element-rel [_])
  (count-rel [_]))

(def ReaderParseMode
  (reify IParseMode
    (listlike? [_ x] (and (sequential? x)
                          (not= (first x) 'quote)))
    (db-element-rel [_] `(var-get ~(resolve 'db-element-rel*)))
    (set-element-rel [_] `(var-get ~(resolve 'set-element-rel*)))
    (listlike-element-rel [_] `(var-get ~(resolve 'listlike-element-rel*)))
    (map-element-rel [_] `(var-get ~(resolve 'map-element-rel*)))
    (count-rel [_] `(var-get ~(resolve 'count-rel*)))))

(def ExtensionalParseMode
  (reify IParseMode
    (listlike? [_ x] (sequential? x))
    (db-element-rel [_] db-element-rel*)
    (set-element-rel [_] set-element-rel*)
    (listlike-element-rel [_] listlike-element-rel*)
    (map-element-rel [_] map-element-rel*)
    (count-rel [_] count-rel*)))

(def ^:dynamic *parse-mode*)

(defn- squach [x]
  (cond (or (set? x) (listlike? *parse-mode* x)) (mapcat squach x)
        (map? x) (mapcat squach (concat (keys x) (vals x)))
        :else [x]))

(defn- extract-unbound-symbols [p]
  (->> p
       squach
       (filter symbol?)
       (filter (comp nil? resolve))))

(defn- pattern-facts [p]
  (letfn [(pattern-facts [p]
                         (cond (set? p)
                               (concat (map (fn [v]
                                              [(set-element-rel *parse-mode*) p nil v])
                                            p)
                                       (mapcat pattern-facts p))

                               (listlike? *parse-mode* p)
                               (concat (map (fn [i v]
                                              [(listlike-element-rel *parse-mode*) p i v])
                                            (iterate inc 0)
                                            p)
                                       [[(count-rel *parse-mode*) p (count p)]]
                                       (mapcat pattern-facts p))

                               (map? p)
                               (concat (map (fn [[k v]]
                                              [(map-element-rel *parse-mode*) p k v])
                                            p)
                                       (mapcat pattern-facts
                                               (concat (keys p) (vals p))))

                               :else
                               nil))]
    (cons [(db-element-rel *parse-mode*) p]
          (pattern-facts p))))

(defmacro comprehend [s & rdecl]
  (assert (-> rdecl count (>= 2)) "comprehend arguments: db pattern(s) expr")
  (binding [*parse-mode* ReaderParseMode]
    (let [ps (butlast rdecl)
          expr (last rdecl)
          explicit-vars (doall (mapcat extract-unbound-symbols ps))
          !implicit-vars (atom nil)
          constraints (letfn [(free-colls [x]
                                          (if (or (set? x)
                                                  (listlike? ReaderParseMode x)
                                                  (map? x))
                                            (let [symb (-> (str "hash__" (hash x))
                                                           symbol)]
                                              (swap! !implicit-vars conj symb)
                                              symb)
                                            x))]
                        (->> ps
                             (mapcat pattern-facts)
                             (map #(cons (first %)
                                         (->> (rest %)
                                              (map free-colls)
                                              doall)))
                             doall))]
      `(for [[~@explicit-vars] (pldb/with-db
                                 (.-content ~s)
                                 ; bogus var required when explicit-vars is empty
                                 (l/run* [~@explicit-vars bogus-var#]
                                         (l/fresh [~@@!implicit-vars]
                                                  ~@constraints
                                                  (l/== bogus-var# nil))))]
         ~expr))))

(def empty-db-set (Set. pldb/empty-db))

(defn into-db-set [s facts-coll]
  (binding [*parse-mode* ExtensionalParseMode]
    (Set. (apply pldb/db-facts
                 (.-content s)
                 (doall (mapcat pattern-facts facts-coll))))))

(defn db-set [& ps]
  (into-db-set empty-db-set ps))