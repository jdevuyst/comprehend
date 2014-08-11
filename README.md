# Comprehend

A Clojure library for performing pattern matching on indexed sets.

Comprehend contains a data structure for persistent indexed sets and a macro `comprehend` for pattern matching on indexed sets.

Indexed sets effectively serve as in-memory databases, but are just as easy to set up as native Clojure sets. The syntax for the `comprehend` macro is reminiscent of the set comprehension idiom {expr : {pattern1, pattern2, …} ⊆ S}.

## Basic usage

To start, create a [Leiningen](http://leiningen.org) project and add the following dependency to `project.clj`:

```clojure
[comprehend "0.4.5"]
```

Next, load Comprehend as follows:

```clojure
(require '[comprehend :as c])
```

Creating indexed sets is no different than creating other collections in Clojure:

```clojure
(c/indexed-set 1 2 3) ; indexed counterpart of (hash-set 1 2 3)
```

The functions `cons`, `conj`, `disj`, `contains?`, `get`, `count`, `hash`, `empty`, and `seq` operate on indexed sets as expected. Moreover, `(s k)` = `(get s k)` if `s` is an indexed set, and if `k` is a symbol or keyword then `(k s)` = `(get s k)`.

Use the function `c/indexed-set?` to test if an object is an indexed set.

Indexed sets shine when you want to perform pattern matching on them:

```clojure
(def s (c/indexed-set [:person 1] [:person 2] [:person 3]
                      [:parent-of 1 2] [:parent-of 2 3] [:parent-of 3 4]))
(c/comprehend s
              [:parent-of a b]
              [:parent-of b c]
              [:grandparent-of a c])
;=> ([:grandparent-of 1 3] [:grandparent-of 2 4])
```

In the above example we match the patterns `[:parent-of a b]` and `[:parent-of b c]` on the indexed set `s`. For every match we yield the pattern `[:grandparent-of a c]`.

It is also possible to match on patterns in subcollections:

```clojure
(c/comprehend (c/indexed-set [[1 2]] [[2 3]] [[1 2 3]])
              [[1 x]]
              x)
;=> (2)
```

Like [`core.match`](https://github.com/clojure/core.match), vector patterns will match only vectors of the same member count but map patterns will also match supermaps:

```clojure
(c/comprehend (c/indexed-set {1 2 3 4})
              {x y}
              [x y])
;=> ([1 2] [3 4])
```

Similar to `core.match`, unbound symbols are interpreted as logical variables:

```clojure
(def bound-symb 1)
(c/comprehend (c/indexed-set [1 2] [3 4])
              [bound-symb unbound-symb]
              [bound-symb unbound-symb])
;=> ([1 2])
```

Notice that round brackets `()` in patterns are not interpreted as lists, contrary to `core.match`. There's no need to as Comprehend considers sequential structures interchangeble, and so you can simply use square brackets `[]` to do pattern matching on lists. This has the advantage that functions can be called from within patterns:

```clojure
(c/comprehend (c/indexed-set [0 1] [1 2])
              [(dec 1) x]
              x)
;=> (1)
```

## Navigating from variables in a pattern up to the toplevel elements of an indexed set

When a complex pattern is successfully matched against an indexed set, it can sometimes be useful to know what collections were found to contain the variables in that pattern.

Use `(c/up x)`, `(c/up x n)`, or `(c/top x)` within a result expression to obtain the supercollections of a variable `x`.

```clojure
(c/comprehend (c/indexed-set {:a 1} {:b 1} {:a 2 :b 2})
              {:a x}
              (c/up x))
;=> (({:a 1}) ({:b 2, :a 2}))
```

Notice that `c/up` and `c/top` return lists of containers. The following example illustrates why a pattern may have more than one matching container.

```clojure
(c/comprehend (c/indexed-set #{:a 1} #{[:a] 2} #{[[:a]] 3})
              #{[x]}
              #{[[x]]}
              (c/top x))
;=> ((#{2 [:a]} #{3 [[:a]]}))
```

## Updating indexed sets

There's a macro `c/rcomprehend` that is to `c/comprehend` as `reduce` is to `map`. It is useful when updating indexed sets based on existing patterns:

```clojure
(c/rcomprehend [s (c/indexed-set [1] [2] [3])]
               [x]
               (conj s [(- x)]))
;=> (c/indexed-set [1] [-1] [2] [-2] [3] [-3])
```

Notice that a let-like syntax is used for the first argument to `c/rcomprehend`. On the first match `s` is bound to the indexed-set containing the elements `[1]`, `[2]`, and `[3]`. For every match an updated indexed set is returned, and this updated result is bound to `s` for the next match (if any).

The let-like syntax can also be used with `c/comprehend`, in which case `s` is bound to the same indexed set for every match.

## Forward matching

Forward pattern matching refers to pattern matching that only returns results that are new relative to some previous state.

Comprehend comes with a function `c/mark` for naming past states. Consider the following indexed set:

```clojure
(-> (c/indexed-set 1)
    (c/mark :a :b)
    (conj 2)
    (c/mark :a)
    (conj 3))
```

In this set, `:a` marks the state where the indexed set contains `1` and `2` but not `3`. Similarly, `:b` marks the point where the indexed set contained `1` but `2` and `3` had not yet been added.

Pass the keyword `:mark` as the first argument to `c/comprehend` or `c/rcomprehend` to limit results to matches that are new relative to the marker that immediately follows `:mark`:

```clojure
(c/comprehend :mark "marker"
              (-> (c/indexed-set [1 2] [2 3])
                  (c/mark "marker")
                  (conj [3 4]))
              [a b]
              [b c]
              [a b c])
;=> ([2 3 4])
```

When `:mark` is used in combination with let-syntax, as previously discussed in relation to `c/rcomprehend`, the bound indexed set is marked afresh.

```clojure
(c/comprehend :mark :a
              [s (-> (c/indexed-set 1)
                     (c/mark :a)
                     (conj 2))]
              x
              {x (c/comprehend :mark :a
                               (conj s 3)
                               y
                               y)})
;=> ({2 (3)})
```

Finally, use `c/unmark` to remove markers from an indexed set. Like `c/mark`, it takes an indexed set and a variable number of markers as arguments.

## Other features

A `c/comprehend` expression may return `::c/skip` to filter results:

```clojure
(c/comprehend (c/indexed-set 1 2 3 4)
              x
              (if (even? x)
                  x
                  ::c/skip))
;=> (2 4)
```

The macro `c/auto-comprehend` is used like `c/comprehend` but with the last argument (the result expression) omitted. Instead `c/auto-comprehend` always returns maps from variables-as-keywords to values:

```clojure
(c/auto-comprehend (c/indexed-set [1 [2 [3]]]
                                  [10 [20 [30 [40]]]])
                   [a [b c]])
;=> ({:a 1 :b 2 :c [3]} {:a 10 :b 20 :c [30 [40]]})
```

Indexing collections can be slow. That is why Comprehend allows indexing to be disabled on a collection by collection basis:

```clojure
(c/comprehend (c/indexed-set [1] ^::c/opaque [2])
              [x]
              y
              [x y])
;=> ([1 [1]] [1 [2]])
```

Think of `^::c/opaque x` as saying that you will not attempt pattern matching on the contents of `x`. Beware that such matching might in fact succeed under certain conditions:

```clojure
(c/comprehend (c/indexed-set [^::c/opaque [1]])
              [[x]]
              x)
;=> nil

(c/comprehend (c/indexed-set [1] [^::c/opaque [1]])
              [[x]]
              x)
;=> (1)
```

Sets are considered equivalent by `=` if and only if they are indexed and marked equivalently.

```clojure
(assert (not= (c/indexed-set 1)
              (c/mark (c/indexed-set 1) :a)))

(assert (= (-> (c/indexed-set 1)
               (conj 2)
               (c/mark :a))
           (-> (c/indexed-set 1)
               (c/mark :a)
               (conj 2)
               (c/mark :a))))

(assert (not= (c/indexed-set [1])
              (c/indexed-set ^::c/opaque [1])))

(assert (not= (c/indexed-set 1) #{1}))
```

Finally, Comprehend comes with a function `c/fix` and a macro `c/fixpoint` for computing fixed points. They are useful for closing indexed sets under a rewriting operation. The following example computes the transitive closure of an indexed set:

```clojure
(c/fixpoint [s (c/indexed-set [1 2] [2 3] [3 4])]
            (c/rcomprehend [s' s]
                           [a b]
                           [b c]
                           (conj s' [a c])))
;=> (c/indexed-set [1 2] [2 3] [3 4] [1 3] [2 4] [1 4])
```

Similarly, `(c/fix f)` returns a function that iteratively applies `f` to its arguments until a fixed point is found, which it then returns.

## Further information

Comprehend is powered by [`core.logic`](https://github.com/clojure/core.logic). I explain some of the main ideas behind the implementation in a [blog post](http://jdevuyst.blogspot.com/2014/05/comprehend-clojure-pattern-matching.html).

More examples:

- [Simple benchmark](https://github.com/jdevuyst/comprehend/blob/master/test/comprehend/benchmark.clj)
- [Theorem prover for propositional logic](https://github.com/jdevuyst/comprehend/blob/master/test/comprehend/examples/theorem_prover.clj)

## License

Copyright © 2014 Jonas De Vuyst

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
