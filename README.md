# Comprehend

A Clojure library for performing pattern matching on indexed sets.

Comprehend contains a data structure for persistent indexed sets and a macro `comprehend` for pattern matching on indexed sets.

Indexed sets effectively serve as in-memory databases, but are just as easy to set up as native Clojure sets. The syntax for the `comprehend` macro is reminiscent of the set comprehension idiom { ∀ patterns ⊆ S : expr }.

## Usage

To start, create a [Leiningen](http://leiningen.org) project and add the following dependency to `project.clj`:

```clojure
[comprehend "0.2.1"]
```

Next, load Comprehend as follows:

```clojure
(require '[comprehend :as c])
```

Creating indexed sets is no different than creating other collections in Clojure:

```clojure
(c/indexed-set 1 2 3) ; indexed counterpart of #{1 2 3}
```

The functions `cons`, `conj`, `disj`, `contains?`, `get`, `count`, `hash`, `empty`, and `seq` operate on indexed sets as expected. Moreover, `(s k)` = `(get s k)` if `s` is an indexed set, and if `k` is a symbol or keyword then `(k s)` = `(get s k)`.

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

Like [core.match](https://github.com/clojure/core.match), vector patterns will match only vectors of the same member count but map patterns will also match supermaps:

```clojure
(c/comprehend (c/indexed-set {1 2 3 4})
              {x y}
              [x y])
;=> ([1 2] [3 4])
```

Similar to [core.match](https://github.com/clojure/core.match), unbound symbols are interpreted as logical variables:

```clojure
(def bound-symb 1)
(c/comprehend (c/indexed-set [1 2] [3 4])
              [bound-symb unbound-symb]
              [bound-symb unbound-symb])
;=> ([1 2])
```

Notice that round brackets `()` in patterns are not interpreted as lists, contrary to [core.match](https://github.com/clojure/core.match). There's no need to as Comprehend considers sequential structures interchangeble, and so you can simply use square brackets `[]` to do pattern matching on lists. This has the advantage that functions can be called from within patterns:

```clojure
(c/comprehend (c/indexed-set [0 1] [1 2])
              [(dec 1) x]
              x)
;=> (1)
```

An expression may return `::c/skip` to filter results:

```clojure
(comprehend (indexed-set 1 2 3 4)
            x
            (if (even? x)
                 x
                 ::c/skip))
;=> (2 4)
```

Creating indexes for collections is slow. It is possible to disable indexing on a collection by collection basis:

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

Sets are considered equivalent by `=` iff they index the same information.

```clojure
(assert (not= (c/indexed-set [1])
              (c/indexed-set ^::c/opaque [1])))

(assert (not= (c/indexed-set 1) #{1}))
```

## Further information

Comprehend is powered by [core.logic](https://github.com/clojure/core.logic). I explain the main ideas behind the implementation in a [blog post](http://jdevuyst.blogspot.com/2014/05/comprehend-clojure-pattern-matching.html).

## License

Copyright © 2014 Jonas De Vuyst

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
