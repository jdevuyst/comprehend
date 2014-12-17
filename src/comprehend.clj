(ns comprehend
  (:require [clojure.walk :as w]
            [comprehend.engine :as ce]
            [comprehend.tools :as ctools]
            [clojure.tools.trace :refer [deftrace trace trace-ns]]))

(ctools/assert-notice)

(declare indexed-set indexed-set?
         unbound-symbols describe annotate-ungrounded-terms
         comprehend* cursor up* top* close-update-map)

;;
;; PUBLIC API
;;

(deftype Set [hs !cache markers]
  clojure.lang.IHashEq
  (hasheq [this] (hash-combine (.hasheq hs)
                               (.hasheq markers)))
  clojure.lang.Counted
  (count [this] (.count hs))
  clojure.lang.IPersistentSet
  (seq [this] (.seq hs))
  (cons [this o] (Set. (.cons hs o)
                       (atom @!cache)
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
                          (atom @!cache)
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

(defn index [hs] ; XXX new
  {:pre [(set? hs)]
   :post [(indexed-set? %)]}
  (Set. hs (atom (ctools/soft-cache)) {}))

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

(comment defmacro up
  ([x] `(apply* ~(cursor x) #'up*))
  ([x n] (let [x' (gensym "up_x__")]
           `(apply* ~(cursor x)
                    (fn [~x']
                      ~(nth (iterate (partial list mapcat `#'up*)
                                     [x'])
                            n))))))

(comment defmacro top [x]
  `(apply* ~(cursor x) #'top*))

(comment defmacro subst [form new-val]
  `(->> (#'paths ~(cursor form))
        (map (juxt identity (constantly ~new-val)))
        (into {})))

(comment defmacro oust [form]
  `(subst ~form ::remove))

(comment defn update [s & ms]
  (let [recipe (->> ms
                    (mapcat identity)
                    close-update-map)]
    recipe))

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

(comment deftype Scope [crumbs m]) ; XXX: remove?

(comment def ^{:dynamic true :private true} *scope*) ; XXX: remove?

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
              (ctools/with-cache-atom
                (.-!cache ~s-name)
                (ce/find-models #{~@patterns}
                                #{(.-hs ~s-name)}))
              )
            ; (pldb/with-db
            ;   (.-idx ~s-name)
            ;   (l/run* [breadcrumbs# ~@explicit-vars]
            ;           (let [~ren-name (memoize #(cond (~explicit-vars %) %
            ;                                           (and (coll? %)
            ;                                                (-> % meta ::opaque not)) (l/lvar)
            ;                                           :else (hash %)))
            ;                 !crumbs# (atom {})
            ;                 make-goal# (fn [f# & args#]
            ;                              (let [args# (map #(if (l/lvar? %)
            ;                                                  (or (.-oname %) [% (.-name %)])
            ;                                                  %)
            ;                                               args#)]
            ;                                (swap! !crumbs# update-in [(last args#)] conj (butlast args#)))
            ;                              (apply f# args#))]
            ;             (fn [a#]
            ;               (-> (partial #'describe nil ~ren-name make-goal#)
            ;                   (mapcat [~@patterns])
            ;                   ~@(if marker
            ;                       [`(conj (l/conde ~@(for [pattern patterns]
            ;                                            `[(marks-rel ~marker-name (~ren-name ~pattern))])))])
            ;                   (concat [(l/== breadcrumbs# @!crumbs#)])
            ;                   (as-> $# (reduce lp/bind a# $#)))))))
            ; (map #(cons (first %)
            ;             (map (.-m ~s-name) (rest %))))
            ; trace
            ; (~f (fn [~s-name [breadcrumbs# ~@explicit-vars]]
            (~f (fn [~s-name ~(->> explicit-vars
                                   (map (fn [x] [x (ce/variable x)]))
                                   (into {}))]

                  ; (binding [*scope* (Scope. breadcrumbs# (.-m ~s-name))]
                  ;   ~expr)
                  ~expr)
                ~s-name)))))
; ~(if marker? ; and [either using rcomprehend or using let syntax] ; TODO optimize further
;    `(mark ~s-name ~marker-name)
;    s-name))))))

(comment defprotocol ^:private ICursor
  (value-with-cursor [this])
  (apply* [this f]))

(comment defrecord Cursor [value crumb-id]
  ICursor
  (value-with-cursor [this]
                     (with-meta value {::cursor this}))
  (apply* [this f]
          (->> this
               f
               distinct
               (map value-with-cursor)
               doall)))

(comment defn- cursor [form]
  `(let [v# ~form]
     (or (-> v# meta ::cursor)
         (Cursor. v# '~form))))

(comment defn- up* [c]
  (->> (.-crumb-id c)
       ((.-crumbs *scope*))
       (map first)
       (map #(if-let [v ((.-m *scope*) (first %))]
               (Cursor. v %)))))

(comment defn- top* [x#]
  (let [ys# (up* x#)
        zs# (->> ys#
                 (filter some?)
                 (mapcat top*))]
    (if (some nil? ys#)
      (cons x# zs#)
      zs#)))

(comment defn- paths [c]
  (->> (.-crumb-id c)
       ((.-crumbs *scope*))
       (mapcat (fn [[[h n :as hn] & args]]
                 (if hn
                   (let [v ((.-m *scope*) h)
                         idx (if args
                               (first args)
                               v)]
                     (->> (paths (Cursor. v hn))
                          (map #(conj % idx))))
                   [[(.-value c)]])))))

(comment defn- close-update-map [m]
  (->> m
       (mapcat (fn [[k v :as kv]]
                 (->> k
                      butlast
                      (iterate butlast)
                      (take-while (comp pos? count))
                      (map #(vector % ::traverse))
                      (cons kv))))
       (into {})))