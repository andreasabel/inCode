Lenses embody Products, Prisms embody Sums
==========================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/lenses-products-prisms-sums.html)

I've written about a variety of topics on this blog, but one thing I haven't
touched in too much detail is the topic of lenses and optics. A big part of this
is because there are already so many great resources on lenses, like the famous
(and my favorite) [lenses over tea](https://artyom.me/lens-over-tea-1) series.

This post won't be a "lens tutorial", but rather a dive into an insightful
perspective on lenses and prisms that I've heard repeated many times, but not
yet all compiled into a single place. In particular, I'm going to talk about the
perspective of lenses and prisms as embodying the essences of products and sums
(respectively), and how that observation can help you with a more "practical"
understanding of lenses and prisms.

An Algebraic Recap
------------------

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

For example, here's a classic example of a data type often used with *lens*:

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
and some type `q`*.

That means that if it is possible to represent `s` as some `(a, q)` (that is,
`s <~> (a, q)`), *then you have two lenses*! Lenses are nothing more than
**descriptions of products**!

In other words, a `Lens' s a` is nothing more than a witness for an
`exists q. s <~> (a, q)` isomorphism.[^2]

With that in mind, let's re-visit a saner definition of lenses based on the idea
that lenses embody descriptions of products:

``` {.haskell}
data Lens' s a = forall q.
                 Lens' { split   :: s -> (a, q)
                       , unsplit :: (a, q) -> s
                       }    -- ^ s <~> (a, q)
```

(the `forall q.` is the *-XExistentialQuantification* extension, and allows us
to hide type variables in consructors)

Now, if `split` and `join` form an isomorphism, *this can only represent valid
lenses*![^3]

We can implement our necessary lens API as so:

``` {.haskell}
view :: Lens' s a -> (s -> a)
view Lens'{..} = fst . split

set :: Lens' s a -> (a -> s -> s)
set Lens'{..} newVal x = case split x of
    (_, q) -> unsplit (newVal, q)      -- "replace" the `a`
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
lenses** (one for every item in the product), and **every lens witnesses one
side of a product**.

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
-- Person <~> (String, Int)

pName :: Lens' Person String
pName = Lens' { split   = \(P n a) -> (n, a)
              , unsplit = \(n, a)  -> P n a
              }

pAge :: Lens' Person Int
pAge = Lens' { split   = \(P n a) -> (a, n)
             , unsplit = \(a, n)  -> P n a
             }
```

These are actually the typical lenses associated with records! You get exactly
these lenses if you use `makeLenses` from the *lens* package.

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
-- a <~> (a, ())

identity :: Lens' a a
identity = Lens' { split   = \x      -> (x, ())
                 , unsplit = \(x, _) -> x
                 }

united :: Lens' a ()
united = Lens' { split   = \x       -> ((), x)
               , unsplit = \((), x) -> x
               }
```

In the language of lens, `identity :: Lens' a a` tells us that all `a`s have an
`a` "inside" them. However, in the language of products, this just tells us that
`a` can be represented as `(a, ())`. In the language of lens,
`united :: Lens' a ()` tells us that all `a`s have a `()` "inside" them. In the
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

\"Sum-thing\" Interesting
-------------------------

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
and...itself? We can interpret this as `()` being one possibility, and
`(a, [a])` (head consed with another list) as the other:

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

Actually, however, there is another way to deconstruct `[a]` as a sum in
Haskell. You can treat it as a sum between `()` and `([a], a)` --- where the
`()` represents the empty list and the `([a], a)` represents an "all but the
last item" list and "the last item":

``` {.haskell}
-- [a] <~> Either () ([a], a)

match  :: [a] -> Either () ([a], a)
match xs
  | null xs   = Left  ()
  | otherwise = Right (init xs, last xs)

-- init gives you all but the last item:
-- > init [1,2,3] = [1,2]

inject :: Either () (a, [a]) -> [a]
inject (Left   _     ) = []
inject (Right (xs, x)) = xs ++ [x]
```

I just think it's interesting that the same type can be "decomposed" into a sum
of two different types in multiple ways.

Another curious sum: if we consider the "empty data type" `Void`, the type with
no inhabitants:

``` {.haskell}
data Void           -- no constructors, no valid inhabitants
```

then we have a curious sum: every type `a` is a sum between *itself* and `Void`.
In other words, `a` is isomorphic to `Either a Void`:

``` {.haskell}
-- a <~> Either a Void

-- a useful helper function when working with `Void`
absurd :: Void -> a
absurd = \case -- empty case statement because we have
               -- no constructors of 'Void' we need to
               -- match on

match :: a -> Either a Void
match x = Left x

inject :: Either a Void -> a
inject (Left  x) = x
inject (Right v) = absurd v
```

Again, if you don't believe me, verify that `inject . match = id` and
`match . inject = id`!

### Through the Looking-Prism

Now let's bring prisms into the picture. A `Prism' s a` also refers to some `a`
"inside" an `s`, with the following API: `preview` and `review`[^4]

``` {.haskell}
preview :: Prism' s a -> (s -> Maybe a)   -- get the 'a' in the 's' if it exists
review  :: Prism' s a -> (a -> s)         -- reconstruct the 's' from an 'a'
```

Naively you might implement a prism like this:

``` {.haskell}
data Prism' s a = Prism' { preview :: s -> Maybe a
                         , review  :: a -> s
                         }
```

But, again, this implementation space is too big. There are way too many values
of this type that aren't *actual* "lawful" prisms. And the laws are kind of
muddled here.

You might be able to guess where I'm going at this point. Whereas a `Lens' s a`
is nothing more than a witness to the fact that `s` is a *product* `(a, q)` ...
a `Prism' s a` is nothing more than a witness to the fact that `s` is a *sum*
`Either a q`. If it is possible to represent `s` as some `Either a q`...then you
have two prisms! Prisms are nothing more than **descriptions of products**!

A `Prism' s a` is nothing more than a witness for an
`exists q. s <~> Either a q` isomorphism.

Under this interpretation, we can write a nice representation of `Prism'`:

``` {.haskell}
data Prism' s a = forall q.
                  Prism' { match  :: s -> Either a q
                         , inject :: Either a q -> s
                         }
```

Now, if `match` and `inject` form an isomorphism, *this can only represent valid
prisms*!

We can now implement the prism API:

``` {.haskell}
preview :: Prism' s a -> (s -> Maybe a)
preview Prism'{..} x = case match x of
    Left _  -> Nothing
    Right y -> Just y

review  :: Prism' s a -> (a -> s)
review Prism'{..} = inject . Left
```

Like for lenses, prisms also admit a particularly elegant formulation for
`over`:

``` {.haskell}
over :: Lens' s a  -> (a -> a) -> (s -> s)
over Lens'{..}  f = inject . first f . match    -- instance Bifunctor (,)

over :: Prism' s a -> (a -> a) -> (s -> s)
over Prism'{..} f = inject . first f . match    -- instance Bifunctor Either
```

Neat, they're actually exactly identical! Who would have thought?

So we see now, similar to lenses, **every sum yields prisms**, and **every prism
witnesses one side of a sum**.

### Prism Tour

Let's go back at our example prisms and see what sort of insight we can gain
from this perspective.

``` {.haskell}
data Shape = Circle  Double
           | RegPoly Natural Double

match :: Shape -> Either Double (Natural, Double)
match (Circle  r  ) = Left r
match (RegPoly n s) = Right (n, s)

inject :: Either Double (Natural, Double) -> Shape
inject (Left   r    ) = Circle  r
inject (Right (n, s)) = RegPoly n s
```

Because `Shape` is a sum between `Double` and `(Natural, Double)`, we get *two
prisms*:

``` {.haskell}
-- Shape <~> Either Natural (Natural, Double)

_Circle :: Prism' Shape Natural
_Circle = Prism' { match  = \case Circle  r    -> Left r
                                  RegPoly n s  -> Right (n, s)
                 , inject = \case Left   r     -> Circle r
                                  Right (n, s) -> RegPoly n s
                 }

_RegPoly :: Prism' Shape (Natural, Double)
_RegPoly = Prism' { match  = \case Circle  r    -> Right r
                                   RegPoly n s  -> Left (n, s)
                  , inject = \case Left  (n, s) -> RegPoly n s
                                   Right  r     -> Circle r
                  }
```

And these are actually the typical prisms associated with an ADT. You actually
get exactly these if you use `makePrisms` from the *lens* package.

What can we get out of our decomposition of `[a]` as a sum between `()` and
`(a, [a])`? Let's look at them:

``` {.haskell}
-- [a] <~> Either () (a, [a])

_Nil :: Prism' [a] ()
_Nil = Prism' { match  = \case []            -> Left ()
                               x:xs          -> Right (x, xs)
              , inject = \case Left _        -> []
                               Right (x, xs) -> x:xs
              }

_Cons :: Prism' [a] (a, [a])
_Cons = Prism' { match  = \case []            -> Right ()
                                x:xs          -> Left (x, xs)
               , inject = \case Left  (x, xs) -> x:xs
                                Right _       -> []
               }
```

We see a sort of pattern here. And, if we look deeper, we will see that *all
prisms* correspond to some sort of "constructor".

After all, what do constructors give you? Two things:

1.  The ability to "create" a value. This corresponds to `review`, or `inject`
2.  The ability to do "case-analysis" or check if a value was created using that
    constructor. This corresponds to `preview`, or `match`.

The API of a "constructor" is pretty much exactly the Prism API. In fact, we
often use Prisms to simulate "abstract" constructors.

An *abstract constructor* is exactly what our *other* `[a]` sum decomposition
gives us! If we look at that isomorphism `[a] <~> Either () ([a], a)` and write
out the prisms, we see that they correspond to the abstract constructors `_Nil`
and `_Snoc`:

``` {.haskell}
-- [a] <~> Either () ([a], a)

_Nil :: Prism' [a] ()
_Nil = Prism' { match  = \xs -> if null xs
                                  then Left  ()
                                  else Right (init xs, last xs)
              , inject = \case Left _        -> []
                               Right (xs, x) -> xs ++ [x]
              }

_Snoc :: Prism' [a] ([a], a)
_Snoc = Prism' { match  = \xs -> if null xs
                                   then Right ()
                                   else Left  (init xs, last xs)
               , inject = \case Left  (xs, x) -> xs ++ [x]
                                Right _       -> []
               }
```

`_Snoc` (`mysteryPrism2`) is an abstract constructor for a list that lets us:

1.  "Construct" an `[a]` given an original list `[a]` and an item to add to the
    end, `a`
2.  "Deconstruct" an `[a]` into an initial run `[a]` and its last element `a`
    (as a pattern match that might "fail").

And, our final sum, `a <~> Either a Void`...what does that decomposition give
us, conceptually?

``` {.haskell}
-- a <~> Either a Void

identity :: Prism' a a
identity = Prism' { match = Left
                  , inject = \case
                      Left  x -> x
                      Right v -> absurd v
                  }


_Void :: Prism' a Void
_Void = Prism' { match = Right
               , inject = \case
                   Left  v -> absurd v
                   Right x -> x
               }
```

In lens-speak, `identity :: Prism' a a` tells us that all `a`s have an `a`
"inside" them (since `match` always matches) and that you can construct an `a`
with only an `a` (whoa). In our "sum" perspective, however, it just witnesses
that an `a <~> Either a Void` sum.

In lens-speak, `_Void :: Prism' a Void` tells us that you can pattern match a
`Void` out of any `a`...but that that pattern match will never fail.
Furthermore, it tells us that if you have a value of type `Void`, you can use
the `_Void` "constructor" to make a value of any type `a`! That is,
`review :: Prism' a Void -> (Void -> a)`!

However, in our "sum" perspective, it is nothing more than the witness of the
fact that `a` is the sum of `a` and `Void`.

### Prism or Not

To me, however, one of the most useful things about this prism perspective is
that it helps me see what *isn't* a prism.

For example, is it possible to have a prism into the *head* of a list? That is,
is the following prism possible?

``` {.haskell}
_head :: Prism' [a] a           -- get the head of a list
```

If you think of a prism as just "a lens that might fail" (as it's often taught),
you might think yes. If you think of a prism as just "a constructor and
deconstructor", you might also think yes, since you can construct an `[a]` with
only a single `a`.[^5]

However, if you think of it as witnessing a sum, you might see that this prism
isn't possible. There is no possible type `q` where `[a]` is a sum of `a` and
`q`. The isomorphism `[a] <~> Either a q` cannot be made for *any* type `q`.
There is no way to express `[a]` as the sum of `a` and some other type. Try
thinking of a type `q` --- it's just not possible!

excercises:

1.  Is (a, Void) a decomp
2.  what does the (Bool, a) \<\~\> Either a a sum give us

(Fun haskell challenge: the version of `match` I wrote there is conceptually
simple, but very inefficient. It traverses the input list three times, uses two
partial functions, and uses a `Bool`. Can you write a `match` that does the same
thing while traversing the input list only once and using no partial functions
or `Bool`s?)

[^1]: All of this is disregarding the notorious "bottom" value that inhabits
    every type.

[^2]: The `exists q. s <~> (a, q)` is a way of saying that `Lens' s a` witnesses
    an isomorphism between `s` and the product of `a` and some "hidden" type
    `q`. A `Lens' s a` is a statement that some `q` *exists* in the first place.
    If no such type exists, no lens is possible. And, for many lenses, `q` might
    be an abstract type.

[^3]: This type is technically also "too big" (you can write a value where
    `split` and `unsplit` do not form an isomorphism), but I think, to me,
    "`split` and `join` must form an isomorphism" is a much clearer and natural
    law than get-put/put-get/put-put.

[^4]: I didn't invent these names :)

[^5]: Although, upon further thought, you might realize that the constructor and
    deconstructor don't match

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
