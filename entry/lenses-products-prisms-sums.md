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

Let's Get Productive!
---------------------

It's easy to recognize `(Int, Double)` as a product between `Int` and `Bool`.
However, did you know that some types are secretly product types in disguise?

For example, here's a classic example of a lensable data type

``` {.haskell}
data Person = P { _pName :: String
                , _pAge  :: Int
                }
```

`Person` is an algebraic data type --- so-called because it is actually a
*product* between a `String` and `Int`. `Person` is *isomorphic* to
`(String, Int)`. I will be writing this as `Person <~> (String, Int)`.

By *isomorphic*, I mean that there are functions
`split :: Person -> (String, Int)` and `unsplit :: (String, Int) -> Person`
where `unsplit . split = id` and `split . unsplit = id`. You can think of this
property as stating formally that you should be able to go from one type to the
other without "losing any information".

In our case, we have:

``` {.haskell}
split :: Person -> (String, Int)
split (P n a) = (n, a)

unsplit :: (String, Int) -> Person
unsplit (n, a) = P n a
```

And we can verify that `unsplit . split` is `id`:

``` {.haskell}
unsplit . split :: Person -> Person
unsplit . split
    = \x          -> unsplit (split x)        -- substitute definition of (.)
    = \case P n a -> unsplit (split (P n a))  -- expand patterns
    = \case P n a -> unsplit (n, a)           -- substitute definition of split
    = \case P n a -> P n a                    -- substitute definition of unsplit
    = \x      -> x                            -- condense patterns
    = id                                      -- definition of id
```

And verification of `split . unsplit = id` is left as an exercise.

There are some other interesting products in Haskell, too. One such example is
`NonEmpty a` being a product between `a` (the head/first item) and `[a]` (the
tail/rest of the items). This means that `NonEmpty a` is isomorphic to
`(a, [a])` --- we have `NonEmpty a <~> (a, [a])`!

Another curious product is the fact that every type `a` is a product between
*itself* and unit, `()`. That is, every type `a` is isomorphic to `(a, ())`.
Freaky, right?

``` {.haskell}
-- a <~> (a, ())

split :: a -> (a, ())
split x = (x, ())

unsplit :: (a, ()) -> a
unsplit (x, _) = x
```

One final interesting "product in disguise" is `Either a a`. "But wait," you
say. "That's a sum...right??"

Well, yeah. But in addition, any `Either a a` is the product between `Bool` and
`a`. That is, `Either a a` is isomorphic to `(Bool, a)`. The `Bool` tells you
"left or right?" and the `a` is the contents!

``` {.haskell}
-- Either a a <~> (Bool, a)

split :: Either a a -> (Bool, a)
split (Left  x) = (False, x)
split (Right x) = (True , x)

unsplit :: (Bool, a) -> Either a a
unsplit (False, x) = Left  x
unsplit (True , x) = Right x
```

Proving that `unsplit . split = id`:

``` {.haskell}
unsplit . split :: Either a a -> Either a a
unsplit . split =
    = \x            -> unsplit (split x)          -- substitute definition of (.)
      -- trying case 1
    = \case Left  y -> unsplit (split (Left  y))  -- expand pattern for case 1
    = \case Left  y -> unsplit (False, y)         -- substitute definition of split
    = \case Left  y -> Left  y                    -- substitute definition of unsplit
    = \x            -> x                          -- condense pattern for case 1
    = id                                          -- definition of id
      -- trying case 2
    = \case Right y -> unsplit (split (Right y))  -- expand pattern for case 2
    = \case Right y -> unsplit (True , y)         -- substitute definition of split
    = \case Right y -> Right y                    -- substitute definition of unsplit
    = \x            -> x                          -- condense pattern for case 2
    = id                                          -- definition of id
```

And `split . unsplit = id` is again left as an exercise.

(`\case` here is from the *-XLambdaCase* extension)

### Lenses

So, how do lenses come into the picture?

Let's review a bit. A `Lens' s a` is a way to "access" an `a` "inside" an `s`,
respecting some laws.

A `Lens' s a` is a data type with the following API:

``` {.haskell}
view :: Lens' s a -> (s -> a)                -- get the 'a' from an 's'
set  :: Lens' s a -> (a -> s -> s)           -- set the 'a' inside an 's'
```

respecting [some
laws](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial#the-lens-laws-)
--- get-put, put-get, and put-put.

Abstract mathematical laws are great and all, but I'm going to tell you a secret
that will render those laws obsolete.

At first, you might naively implement lenses like:

``` {.haskell}
data Lens' s a = Lens' { view :: s -> a
                       , set  :: a -> s -> s
                       }
```

But this is bad bad bad. That's because you can use this to represent lenses
that "break the laws". This representation is, to use the technical term, "too
big". It allows more more values than are actual lenses.

So, here's the secret: A `Lens' s a` means that *`s` is a product between `a`
and some type `x`*.

That means that if it is possible to represent `s` as some `(a, q)` (that is,
`s <~> (a, q)`), *then you have a lens*! Lenses are nothing more than
**descriptions of products**!

In other words, a `Lens' s a` is nothing more than a witness for an
`exists q. s <~> (a, q)` isomorphism.

With that in mind, let's re-visit a saner definition of lenses based on the idea
that lenses embody descriptions of products:

``` {.haskell}
data Lens' s a = forall q.
                 Lens' { split   :: s -> (a, q)
                       , unsplit :: (a, q) -> s
                       }    -- ^ s <~> (a, q)
```

Now, if `split` and `join` form an isomorphism, *this can only represent valid
lenses*!\[\^big\]

This type is technically also "too big" (you can write a value where `split` and
`unsplit` do not form an isomorphism), but I think, to me, "`split` and `join`
must form an isomorphism" is a much clearer and natural law than
get-put/put-get/put-put.

We can implement our necessary lens API as so:

``` {.haskell}
view :: Lens' s a -> (s -> a)
view Lens'{..} = fst . split

set :: Lens' s a -> (a -> s -> s)
set Lens'{..} newVal x = unsplit (newVal, q)      -- "replace" the `a`
  where
    (_, q) = split x
```

(Using the *-XRecordWildcards* extension, where `Lens'{..}` binds `split` and
`unsplit` to the fields of the lens)

The implementation of the helper function `over` (which modifies the `a` with a
function) is also particularly elegant:

``` {.haskell}
over :: Lens' s a -> (a -> a) -> (s -> s)
over Lens'{..} f = unsplit . first f . split
```

The surprising result of this perspective is that **every product yields
lenses** (one for every item in the product), and **every lens witnesses a
product**.

### Insights Gleamed

Let's take a look at our first product we talked about:

``` {.haskell}
data Person = P { _pName :: String
                , _pAge  :: Int
                }

split :: Person -> (String, Int)
split (P n a) = (n, a)

unsplit :: (String, Int) -> Person
unsplit (n, a) = P n a
```

Because `Person` is a product between `String` and `Int`, we get *two lenses*: a
`Lens' Person String` and `Lens' Person Int`. *Every product* gives us a lens
for every item in the product.

``` {.haskell}
pName :: Lens' Person String
pName = Lens' { split   = \(P n a) -> (n, a)
              , unsplit = \(n, a)  -> P n a
              }

pAge :: Lens' Person String
pAge = Lens' { split   = \(P n a) -> (a, n)
             , unsplit = \(a, n)  -> P n a
             }
```

The inverse is true too. **Every lens witnesses a product**. The fact that we
have a lawful `pName :: Lens' Person String` means that a `Person` *must* be a
product between `String` and some other (hidden) type.

It can be insightful to look at products that we know and see what lenses those
correspond to.

For example, our `NonEmpty a <~> (a, [a])` product tells us that `NonEmpty a`
has at least two lenses: a "head" lens `Lens' (NonEmpty a) a` and a "tail" lens
`Lens' (NonEmpty a) [a]`.

Our `a <~> (a, ())` product gives some interesting insight. This tells us that
we always have an "identity" lens `Lens' a a`, and a "unit" lens `Lens' a ()`,
for any `a`:

``` {.haskell}
identity :: Lens' a a
identity = Lens' { split   = \x      -> (x, ())
                 , unsplit = \(x, _) -> x
                 }

unital :: Lens' a ()
unital = Lens' { split   = \x       -> ((), x)
               , unsplit = \((), x) -> x
               }
```

In the language of lens, `identity :: Lens' a a` tells us that all `a`s have an
`a` "inside" them. However, in the language of products, this just tells us that
`a` can be represented as `(a, ())`. In the language of lens,
`unital :: Lens' a ()` tells us that all `a`s have a `()` "inside" them. In the
language of products, this just tells us that `a <~> (a, ())`.

What insight does our `Either a a <~> (Bool, a)` product perspective give us?
Well, let's write out their types and see what it might suggest:

``` {.haskell}
mysteryLens1 :: Lens' (Either a a) Bool
mysteryLens2 :: Lens' (Either a a) a
```

Looking at `mysteryLens1 :: Lens' (Either a a) Bool`, we are saying that every
`Either a a` has some `Bool` "inside" it. From our knowledge of our product, we
know that this `Bool` is really a *flag* for left-ness or right-ness. Getting
the `Bool` is finding out if we're in `Left` or `Right`, and flipping the `Bool`
"inside" is really just swapping from `Left` to `Right`.

Looking at `mysteryLens2 :: Lens' (Either a a) a`, we are saying that every
`Either a a` has some `a` "inside" it. From what we know about the underlying
product, the `a` is just the "contained value", *ignoring* leftness or
rightness. Getting the `a` is getting the contained value and losing
leftness/rightness, and re-setting the `a` inside is modifying the contained
value but preserving leftness/rightness.

So that's really the essence of what a `Lens'` is. A `Lens' s a` is the
embodiment of the fact that `s` can be represented as a product between `a` and
something else --- that `s <~> (a, q)`. All of the lens laws just boil down to
this. **Lenses embody products**.

There's Sum-thing about This...
-------------------------------

It's easy to recognize `Either Int Bool` as a sum between `Int` and `Bool`.
However, did you know that some types are secretly sums in disguise?

For example, here's a data type you might encounter out there in the real world:

``` {.haskell}
data Shape = Circle  Double           -- radius
           | RegPoly Natural Double   -- number of sides, length of sides
```

`Circle 2.9` represents a circle with radius 2.9, and `RegPoly 8 4.6` represents
a octagon (8-sided figure) whose sides all have length 4.6.

`Shape` is an algebraic data type --- so-called because it is actually a *sum*
between `Double` and `(Natural, Double)` (a `Natural` is the non-negative
`Integer` type). `Shape` is *isomorphic* to `Either Double (Natural, Double)`.
To prove it, let's witness `Shape <~> Either Double (Natural, Double)` using the
functions `match` and `inject`:

``` {.haskell}
-- Shape <~> Either Double (Natural, Double)

match :: Shape -> Either Double (Natural, Double)
match (Circle  r  ) = Left r
match (RegPoly n s) = Right (n, s)

inject :: Either Double (Natural, Double) -> Shape
inject (Left   r    ) = Circle  r
inject (Right (n, s)) = RegPoly n s
```

Since `inject . match = id` and `match . inject = id`, this proves that `Shape`
is a sum in disguise.

Another interesting "hidden sum" is the fact that `[a]` in Haskell is actually a
sum between `()` and `(a, [a])`. That's right --- it's a sum between `()`
and...itself?

``` {.haskell}
-- [a] <~> Either () (a, [a])

match :: [a] -> Either () (a, [a])
match []     = Left  ()
match (x:xs) = Right (x, xs)

inject :: Either () (a, [a]) -> [a]
inject (Left   _     ) = []
inject (Right (x, xs)) = x:xs
```

If you don't believe me, just verify that `inject . match = id` and
`match . inject = id` :)

And, if we consider the "empty data type" `Void`, the type with no inhabitants:

``` {.haskell}
data Void           -- no constructors, no valid inhabitants
```

then we have a curious sum: every type `a` is a sum between *itself* and `Void`.
In other words, `a` is isomorphic to `Either a Void`:

``` {.haskell}
-- a <~> Either a Void

match :: a -> Either a Void
match x = Left x

inject :: Either a Void -> a
inject (Left  x) = x
inject (Right v) = case v of
                    {}  -- empty case statement because we have
                        -- no constructors of 'v' we need to
                        -- match on
```

Again, if you don't believe me, verify that `inject . match = id` and
`match . inject = id`!

One last example -- one of my favorite sums from math is the fact that the
natural numbers are a sum between ... themselves and themselves.
`Natural <~> Either Natural Natural`. Sometimes you might hear this stated as
![2 \\mathbb{N} \~ \\mathbb{N}](https://latex.codecogs.com/png.latex?2%20%5Cmathbb%7BN%7D%20~%20%5Cmathbb%7BN%7D "2 \mathbb{N} ~ \mathbb{N}")
(where
![2 \\mathbb{N}](https://latex.codecogs.com/png.latex?2%20%5Cmathbb%7BN%7D "2 \mathbb{N}")
can be thought of as a fancy way of writing
![\\mathbb{N} + \\mathbb{N}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BN%7D%20%2B%20%5Cmathbb%7BN%7D "\mathbb{N} + \mathbb{N}")).
So, the sum of the naturals with themselves is...exactly the naturals?

``` {.haskell}
-- Natural <~> Either Natural Natural

match :: Natural -> Either Natural Natural
match n = case n `divMod` 2 of
    (q, 0) -> Left  q       -- even number
    (q, 1) -> Right 1       -- odd number

inject :: Either Natural Natural -> Natural
inject (Left  q) = 2 * q
inject (Right q) = 2 * q + 1
```

Go figure!

### Through the Looking-Prism

[^1]: All of this is disregarding the notorious "bottom" value that inhabits
    every type.

---------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)
