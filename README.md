# Comprehend

A Clojure library for performing pattern matching on indexed sets.

Comprehend contains a data structure for functional indexed sets and a macro `comprehend` for pattern matching on indexed sets.

Indexed sets effectively serve as in-memory databases, but are just as easy to set up as native Clojure sets. The syntax for the `comprehend` macro is reminiscent of [core.match](https://github.com/clojure/core.match).

Comprehend is powered by [core.logic](https://github.com/clojure/core.logic).

## Usage

To start:

```clojure
(require '[comprehend.core :as c])
```

Creating indexed sets is no different than creating other collections in Clojure:

```clojure
(indexed-set) ; empty indexed set
(indexed-set x y z) ; indexed counterpart of #{x y z}
```

The functions `cons`, `conj`, `disj`, `contains?`, `get`, `count`, `hash`, `empty`, and `seq` operate on indexed sets as expected.

Indexed sets shine when you want to perform pattern matching on them:

```clojure
(def s (c/indexed-set [:person 1] [:person 2] [:person 3]
                      [:parent-of 1 2] [:parent-of 2 3] [:parent-of 3 4]))
(c/comprehend s
              [:parent-of a b]
              [:parent-of b c]
              [:grandparent-of a c])
; => '([:grandparent-of 1 3] [:grandparent-of 2 4])
```

In the above example we match the patterns `[:parent-of a b]` and `[:parent-of b c]` on the indexed set `s`. For every match we yield the pattern `[:grantparent-of a c]`.

It is also possible to match on patterns in subcollections:

```clojure
(c/comprehend (c/indexed-set [[1 2]] [[2 3]] [[1 2 3]])
              [[1 x]]
              x)
; => '(2)
```

Like `core.match`, vector patterns will match only vectors of the same member count but map patterns will also match supermaps:

```clojure
(c/comprehend (c/indexed-set {1 2 3 4})
              {x y}
              [x y])
; => '([1 2] [3 4])
```

When a symbol that is not bound in the local scope is encountered in a pattern, it is interpreted as a logical variable. To wit,

```clojure
(def bound-var 1)
(c/comprehend (c/indexed-set [1 2] [3 4])
              [bound-var y]
              [bound-var y])
; => '([1 2])
```

Notice that round brackets `()` in patterns are not interpreted as lists. There's no need to, as comprehend considers sequential structures interchangeble. This means functions can be called in patterns:

```clojure
(c/comprehend (c/indexed-set [0 1] [1 2])
              [(dec 1) x]
              x)
; => '(1)
```

Creating indexes for (sub)collections is slow. It is possible to disable indexing on a (sub)collection by (sub)collection basis:

```clojure
(c/comprehend (c/indexed-set [1] ^::c/opaque [2])
              [x]
              y
              [x y])
; => '([1 [1]] [1 [2]])
```

Equality of indexed sets respects value semantics. Indexed sets are considered equivalent iff they index identical facts. Thus, `(c/indexed-set [1])` and `(c/indexed-set ^::c/opaque [1])` are considered different. Indexed sets are never equal to native Clojure sets.

## License

Copyright © 2014 Jonas De Vuyst

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
