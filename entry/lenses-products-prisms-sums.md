Lenses embody Products, Prisms embody Sums
==========================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/lenses-products-prisms-sums.html)

I've written about a variety of topics on this blog, but one thing I haven't
touched in too much detail is the topic of lenses and optics. A big part of this
is because there are already so many great resources on lenses, like the famous
(and my favorite) [lenses over tea](https://artyom.me/lens-over-tea-1) series.

This post won't be a "lens tutorial", but rather a dive into a (what I believe
is an) insightful perspective on lenses and prisms that I've heard repeated many
times, but not yet all gathered together into a single place. In particular, I'm
going to talk about the perspective of lenses and prisms as embodying the
essences of products and sums (respectively), and how that observation can help
you with a more "practical" understanding of lenses and prisms.

Products and Sums
-----------------

In Haskell, "products and sums" can roughly be said to correspond to "tuples and
`Either`". If I have two types `A` and `B`, `(A, B)` is their "product" type.
It's often called an "anonymous product", because we can make one without having
to give it a fancy name. It's called a product type because `A` has
![n](https://latex.codecogs.com/png.latex?n "n") possible values and `B` has
![m](https://latex.codecogs.com/png.latex?m "m") possible values, then `(A, B)`
has
![n \\times m](https://latex.codecogs.com/png.latex?n%20%5Ctimes%20m "n \times m")
possible values[^1]. And, `Either A B` is their (anonymous) "sum" type. It's
called a sum type because `Either A B` has
![n + m](https://latex.codecogs.com/png.latex?n%20%2B%20m "n + m") possible
values. I won't go much deeper into this, but there are [many useful tutorials
already
online](https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types)
on this topic!

It's easy to recognize `(Int, Double)` as a product between `Int` and `Bool`.
However, did you know that some types are secretly product types in disguise?

For example, here's a classic example of a lensable data type

``` {.haskell}
data Person = P { _pName :: String
                , _pAge  :: Int
                }
```

`Person` is an algebraic data type --- so-called because it is actually a
*product* between a `String` and `Int`. `Person` is isomorphic to

[^1]: All of this is disregarding the notorious "bottom" value that inhabits
    every type.
