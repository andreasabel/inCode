Introduction to Singletons (Part 3)
===================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/introduction-to-singletons-3.html)

Welcome back! This article is part 3 of our journey through the *singleton
design pattern*, and the great
*[singletons](http://hackage.haskell.org/package/singletons)* library!

This post will be a continuation of [Part
1](https://blog.jle.im/entry/introduction-to-singletons-1.html) and [Part
2](https://blog.jle.im/entry/introduction-to-singletons-2.html), so if you
haven't read those first, now would be a good time to pause and do so and also
work on some of the exercises. Today we will be expanding on the ideas in those
posts by working with more complex relationships between values and lifting
functions on values to functions on types. Like the previous posts, we will
start by writing things "by hand", and then jumping into the singletons library
and seeing how the framework gives you tools to work with these ideas in a
smoother way.

Code in this post is built on *GHC 8.4.3* with the
*[lts-12.9](https://www.stackage.org/lts-12.9)* snapshot (so, singletons-2.4.1).

Review
------

In the first post we looked at the `Door` type, indexed with a phantom type of
kind `DoorState`.

``` {.haskell}
$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s
```

This gives us (at least) three distinct types `Door 'Opened`, `Door 'Closed`,
and `Door 'Locked`, which can be used to represent opened, closed, and locked
doors, respectively.

This scheme gives us a few super-powers:

-   The fact that these are three *distinct* types allows us to enforce
    type-safety by prohibiting operations on certain types of doors.

    We saw this in the types of functions like
    `openDoor :: Door 'Opened -> Door 'Closed`, which can only work on
    non-locked and closed doors.

-   Because the types are distinct, this allows our functions to be more
    expressive by stating how they change door states, programmatically.

    We saw this to a certain extent in functions like `openDoor`, `closeDoor`,
    and `lockDoor`, where the function type signatures tell the user how the
    input and output doors are related. However, we will be taking this to a new
    level in this post.

-   But, because these types are all "derived" from the same type, we can also
    write functions that work on *all* `Door`s. We saw this in functions like
    `lockAnyDoor`, and we also exploit this in our definition of `SomeDoor`.

    Essentially we also get a fourth type "for free": `forall s. Door s`, the
    type that can be used as any door. It's a subtype of all three of the above
    types![^1]

Then we talked about situations where we want to "not care" about the door
status in the type system, or when we want to return a door with a state that is
not known statically, and must be determined dynamically at runtime. After going
through many "analogous" and equivalent type, we arrived at the existential
wrapper `SomeDoor`:

``` {.haskell}
data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor
```

``` {.haskell}
mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
    MkSomeDoor dsSing mat
```

We must be careful to pack the `Sing s` with the `Door s`, so that we can
pattern match at runtime to determine what the original `s` was.

``` {.haskell}
checkOpened :: SomeDoor -> Bool
checkOpened (MkSomeDoor SOpened _) = True
checkOpened (MkSomeDoor SClosed _) = False
checkOpened (MkSomeDoor SLocked _) = False
```

``` {.haskell}
ghci> let x = mkSomeDoor Opened "Oak"
ghci> let y = mkSomeDoor Closed "Spruce"
ghci> checkOpened x
True
ghci> checkOpened y
False
```

Finally, we talked a bit about the "unified" singleton system that the
*singleton* library offers. This included things like `SingI` to implicitly pass
singletons, and the `SingKind` kind-class that associates types with their
lifted kinds and lets you reify and reflect with functions like `withSomeSing`
and `fromSing`.

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

[^1]: Some subtle points for those more familiar with Haskell's type system: In
    Haskell, we say that a type `B` is a subtype of type `A` if, wherever a
    function expects an `A`, we can give a `B` instead. Any function that
    expects a `Door 'Opened` will take a `forall s. Door s` (a type that can be
    instantiated with any `s`). However, the opposite is not true --- if a
    Rank-N function expects a `forall s. Door s`, you cannot give it a
    `Door 'Opened`.
