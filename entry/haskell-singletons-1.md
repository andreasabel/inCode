Dependent Types in Haskell: Introduction to Singletons (Part 1)
===============================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/haskell-singletons-1.html)

Real dependent types are coming to Haskell soon! Until then, we have the great
*[singletons](http://hackage.haskell.org/package/singletons)* library :)

If you’ve ever run into dependently typed programming in Haskell, you’ve
probably encountered mentions of singletons (and the *singletons* library).

I’ve noticed that there aren’t very many comprehensive tutorials or
introductions on the motivation and usage of the singletons library, as well as
integration of it into your programs and how to find ways to use it. (But the
haddocks provide very thorough documentation on how to use it once you already
know what to use it for!) So this series of articles will be my attempt at
giving you the rundown of the library, why you’d want it, and how to start using
it in your Haskell programming today! And maybe also be a nice introduction to
the concept as a whole if you just want to learn.

This series will be based on [a
talk](http://talks.jle.im/lambdaconf-2017/singletons/) I gave over the summer,
and will expand on it eventually.

These posts will assume no knowledge of dependent types, and, for now, only
basic to intermediate Haskell knowledge. (Types, kinds, typeclasses, data types,
functions)

All code is built on *GHC 8.2.1* and with the
*[nightly-2017-07-30](https://www.stackage.org/nightly-2017-07-31)* snapshot
(so, singletons-2.3). However, there are negligible changes in the GHC type
system between GHC 8.0 and 8.2 (the only difference is in the libraries, more or
less), so everything should work on GHC 8.0 as well!

The Phantom of the Types
------------------------

Let’s start with a very common Haskell trick that most learn early in their
Haskelling journey: the [phantom type](https://wiki.haskell.org/Phantom_type).

Phantom types in Haskell are a very easy way to add a layer of “type safety” for
your types and DSL’s. It helps you restrict what values functions can take and
encode pre- and post-conditions directly into your types.

For example, in

``` {.haskell}
data Foo a = MkFoo
```

The `a` parameter is phantom, because nothing of type `a` in the data type…it
just exists as a dummy parameter for the `Foo` type. We can use `MkFoo` without
ever requiring something of type `a`:

``` {.haskell}
ghci> :t MkFoo :: Foo Int
Foo Int
ghci> :t MkFoo :: Foo Bool
Foo Bool
```

A common use case of phantom type parameters is to tag data as “sanitized” or
“unsanitized”, for instance. For a simple example, let’s check out a simple DSL
for a type-safe door:

``` {.haskell}
data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

data Door (s :: DoorState) = UnsafeMkDoor
```

A couple things going on here:

1.  We’re using the `DataKinds` extension to create both the *type* `DoorState`
    as well as the *kind* `DoorState`.

    Normally, `data DoorState = Opened | Closed | Locked` in Haskell defines the
    type `DoorState` and the value constructors `Opened`, `Closed`, and
    `Locked`.

    However, with `DataKinds`, that statement also defines a new *kind*
    `DoorState`, with *type* constructors `'Opened`, `'Closed`, and `'Locked`.
    (note the `'` ticks!)

    ``` {.haskell}
    ghci> :k 'Opened
    DoorState
    ghci> :k 'Locked
    DoorState
    ```

2.  We’re defining the `Door` type with a *phantom parameter* `s`. It’s a
    phantom type because we don’t actually have any *values* of type `s` in our
    data type…the `s` is only just there as a dummy parameter for the type.

    We can use `UnsafeMkDoor` without ever using anything of type `s`. In
    reality, a real `Door` type would be a bit more complicated (and the direct
    `MkDoor` constructor would be hidden).

    ``` {.haskell}
    ghci> :t UnsafeMkDoor :: Door 'Opened
    Door 'Opened
    ghci> :t UnsafeMkDoor :: Door 'Locked
    Door 'Locked
    ```

We’ll take a `Door s` to mean the type of a door with that current status. So a
`Door 'Opened` is the type of an opened door, a `Door 'Closed` is the type of a
closed (and unlocked) door, etc.

Alternatively, we can define `Door` using [*GADT*
syntax](https://en.wikibooks.org/wiki/Haskell/GADT#Syntax).

``` {.haskell}
data Door :: DoorState -> Type where
    UnsafeMkDoor :: Door s
```

This is defining the exact same type in the alternate GADT syntax – here, we
define types by giving the type of its constructors.

### Phantoms in Action

At first, this seems a bit silly. Why even have the extra type parameter if you
don’t ever use it?

Well, right off the bat, we can write functions that expect only a certain type
of `Door`, and return a specific type of `Door`:

``` {.haskell}
closeDoor :: Door 'Opened -> Door 'Closed
closeDoor UnsafeMkDoor = UnsafeMkDoor
```

So, the `closeDoor` function will *only* take a `Door 'Opened` (an opened door).
And it will return a `Door 'Closed` (a closed door).

``` {.haskell}
ghci> let myDoor = UnsafeMkDoor :: Door 'Opened
ghci> :t closeDoor myDoor
Door 'Closed
ghci> let yourDoor = UnsafeMkDoor :: Door 'Closed
ghci> :t closeDoor yourDoor
TYPE ERROR!  TYPE ERROR!
```

You can think of this as a nice way of catching *logic errors* at compile-time.
If your door type did not have its status in the type, the `closeDoor` could
have been given a closed or locked door, and you’d have to reject it at runtime!

By adding the state of the door into its type, we can encode our pre-conditions
and post-conditions directly into the type. And any opportunity to move runtime
errors to compiletime errors should be celebrated with a little party!

This would also stop you from doing silly things like closing a door twice in a
row:

``` {.haskell}
ghci> :t closeDoor . closeDoor
TYPE ERROR!  TYPE ERROR!
```

Do you see why?

With a couple of state transitions, we can write compositions that are
typechecked to all be legal:

``` {.haskell}
lockDoor :: Door 'Closed -> Door 'Locked
lockDoor UnsafeMkDoor = UnsafeMkDoor

openDoor :: Door 'Closed -> Door 'Opened
openDoor UnsafeMkDoor = UnsafeMkDoor
```

``` {.haskell}
ghci> :t closeDoor . openDoor
Door 'Closed -> Door 'Closed
ghci> :t lockDoor . closeDoor . openDoor
Door 'Closed -> Door 'Locked
```

### The Phantom Menace

However, in standard Haskell, we quickly run into some practical problems if we
program with phantom types this way.

For example, how could we write a function to get the state of a door?

``` {.haskell}
doorStatus :: Door s -> DoorState
doorStatos _ = -- ?
```

(It can be done with an ad-hoc typeclass, but it’s not simple, and it’s prone to
implementation bugs)

And, perhaps even more important, how can you create a `Door` with a given state
that isn’t known until runtime? If we know the type of our doors at compiletime,
we can just explicitly write `UnsafeMkDoor :: Door 'Opened`. But what if we
wanted to make a door based on a `DoorState` input?

``` {.haskell}
mkDoor :: DoorState -> Door s
mkDoor Opened = -- ?
mkDoor Closed = -- ?
mkDoor Locked = -- ?
```

Ah hah, you say. That’s easy!

``` {.haskell}
mkDoor :: DoorState -> Door s
mkDoor Opened = UnsafeMkDoor
mkDoor Closed = UnsafeMkDoor
mkDoor Locked = UnsafeMkDoor
```

Unfortunately, that’s not how types work in haskell. Remember that for a
polymorphic type `forall s. DoorState -> Door s`, the *caller* picks the type
variable.

``` {.haskell}
ghci> :t mkDoor Opened :: Door 'Closed
Door 'Closed
```

Oops. What about

``` {.haskell}
mkDoor :: DoorState -> Door s
mkDoor Opened = UnsafeMkDoor :: Door 'Opened
mkDoor Closed = UnsafeMkDoor :: Door 'Closed
mkDoor Locked = UnsafeMkDoor :: Door 'Locked
```

Here, you get a similar problem. The type variable `s` *must be able to be
freely chosen* by the caller. And for all of those branches, we return an `s`
that is potentially different than one asked for by the caller. In fact, this
one doesn’t even compile.

### The Fundamental Issue in Haskell

We’ve hit upon a fundamental issue in Haskell’s type system: **type erasure**.
In Haskell, types only exist *at compiletime*, for help with typechecking. They
are completely erased at runtime.

This is usually what we want. It’s great for performance, and you can bypass
things like the ad-hoc runtime type checking that you have to deal with in
dynamic languages like python.

But in our case, it makes functions like `doorState` fundamentally impossible.
Or, does it?

The Singleton Pattern
---------------------

A singleton in Haskell is a type that has exactly one inhabitant, and whose
constructor (when pattern matched on) reveals its type.

``` {.haskell}
data SingDS :: DoorState -> Type where
    SOpened :: SingDS 'Opened
    SClosed :: SingDS 'Closed
    SLocked :: SingDS 'Locked
```

Here we’re using GADT syntax again. Note that, if we use `SOpened`, we will get
a `SingDS 'Opened`. And if we have a `SingDS 'Opened`, we know that it was
constructed using `SOpened`. Essentially, this gives us three values:

``` {.haskell}
SOpened :: SingDS 'Opened
SClosed :: SingDS 'Closed
SLocked :: SingDS 'Locked
```

### The Power of the Pattern Match

The power of singletons is that we can now *pattern match* on types,
essentially.

``` {.haskell}
doorStatus :: SingDS s -> Door s -> DoorState
doorStatus = \case
    SOpened -> -- in this branch, `s` is `'Opened`
        \_ -> "Door is opened"
    SClosed -> -- in this branch, `s` is `'Closed`
        \_ -> "Door is closed"
    SLocked -> -- in this branch, `s` is `'Locked`
        \_ -> "Door is locked"
```

(using LambdaCase syntax because why not)

We can rewrite `doorStats` to take an additional `SingDS`, which we can use to
figure out what `s` is. When we pattern match on it, we reveal what `s` is.

The singleton property of `SingDS` ensures us that whatever `s` the `SingDS` has
is the *same* `s` that the `Door` has.

Not only do *we* know what `s` is, but GHC can also take advantage of it. In the
scope of the case statement branch, GHC *knows* what `s` must be:

``` {.haskell}
closeDoor :: Door 'Opened -> Door 'Closed
lockDoor  :: Door 'Closed -> Door 'Locked

lockAnyDoor :: SingDS s -> (Door s -> Door 'Locked)
lockAnyDoor = \case
    SOpened -> lockDoor . closeDoor
    SClosed -> lockDoor
    SLocked -> id
```

Note that `lockDoor . closeDoor :: Door 'Opened -> Door 'Locked`. GHC will only
allow you to compile that if it *knew* that the input is `Door 'Opened`…and,
because of the GADT pattern match, it does!

Similar for the `SLocked -> id` branch — `id` is only a valid response if `s` is
`'Locked` (and so `id :: Door 'Locked -> Door 'Locked`). But, because we pattern
matched on `SLocked`, GHC knows that this is legal!

We say that `SOpened` is a runtime *witness* to `s` being `'Opened`.

### Recovering Implicit Passing

One downside is that we are required to manually pass in our witness. Wouldn’t
it be nice if we could have it be passed implicitly? We can do something with
typeclasses:

``` {.haskell}
class SingDSI s where
    singDS :: SingDSI s

instance SingDSI 'Opened where
    singDS = SOpened
instance SingDSI 'Closed where
    singDS = SClosed
instance SingDSI 'Locked where
    singDS = SLocked
```

And so now we can do:

``` {.haskell}
doorStatus_ :: SingDSI s => Door s -> DoorState
doorStatus_ = doorStatus singDS

lockAnyDoor :: SingDSI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor singDS
```

Here, type inference will tell GHC that you want `singDS :: Sing s`, and it will
pull out the proper singleton for the door you want to check!

#### The Same Power

In Haskell, a constraint `SingDSI s =>` is essentially the same as passing in
`SingDS s` explicitly. Either way, you are passing in a runtime witness that
your function can use. You can think of `SingDSI s =>` as passing it in
*implicitly*, and `SingDS s ->` as passing it in *explicitly*.

Earlier, I disparaged the “ad-hoc typeclass” approach. But, here, the typeclass
isn’t quite ad-hoc; it’s basically exactly carrying around an implicit witness
of `s` that we can grab at any time.

### Fun with Witnesses

We can write a nice version of `mkDoor` this way:

``` {.haskell}
mkDoor :: SingDS s -> Door s
mkDoor = \case
    SOpened -> UnsafeMkDoor
    SClosed -> UnsafeMkDoor
    SLocked -> UnsafeMkDoor
```

So we can call it values of `SingDS`:

``` {.haskell}
ghci> :t mkDoor SOpened
Door 'Opened
ghci> :t mkDoor SLocked
Door 'Locked
```

And now we can’t do something silly like pass in `SLocked` to get a
`Door 'Opened`!

However, this is still a step away from a `Door` whose status can vary at
runtime.

Ditching the Phantom
--------------------

Now, sometimes we don’t actually care about the state of the door, and we don’t
*want* the state of the door in its type. Our `closeAnyDoor` function earlier
was an example.

We can make a new type, `SomeDoor`, that contains a door of *some* status.

``` {.haskell}
data SomeDoor :: Type where
    MkSomeDoor :: Door s -> SomeDoor
```

``` {.haskell}
ghci> let myDoor = MkSomeDoor (mkDoor SClosed)
ghci> :t myDoor
myDoor :: SomeDoor
```

But, this isn’t too ideal. This is because when we pattern match on the
`MkSomeDoor` constructor, we have no idea what the original actual status of the
door actually was. This is because of, again, type erasure — once we put a
`Door s` into `MkSomeDoor :: Door s -> SomeDoor`, the `s` is lost to the
typechecker forever.

``` {.haskell}
ghci> :t case myDoor of
           MkSomeDoor d -> lockDoor d
TYPE ERROR!  TYPE ERROR!  Is d a closed door?  Who knows!
```

Even though we know we created `myDoor` with `SClosed`, once it becomes a
`SomeDoor`, it’s basically indistinguishable from any other door type, and has
to be treated as if we didn’t know what the original door type was.

### Can I get a witness?

Luckily for us, though, we now have a way to “store” `s` and “pattern match” on
it later, using a runtime witness. So, here is our new `SomeDoor`:

``` {.haskell}
data SomeDoor :: Type where
    MkSomeDoor :: SingDS s -> Door s -> SomeDoor
```

``` {.haskell}
ghci> let myDoor = MkSomeDoor SClosed (mkDoor SClosed)
ghci> :t myDoor
myDoor :: SomeDoor
ghci> :t case myDoor of
           MkSomeDoor SOpened d -> lockDoor (closeDoor d)
           MkSomeDoor SClosed d -> lockDoor d
           MkSomeDoor SLocked d -> d
Door 'Locked
-- alternatively
ghci> :t case myDoor of
           MkSomeDoor sd d -> lockAnyDoor sd d
Door 'Locked
```

By storing the witness of the state of the door inside `MkSomeDoor`, we have a
way to recover the original type information!

### An Existential Quandary

Those who have been exposed to dependent types before might recognize
`MkSomeDoor` as an existential type constructor. It “hides” a type variable
inside it. In our case, using `MkSomeDoor :: SingDS s -> Door s -> SomeDoor`
essentially hides the type variable `s` from the outside world. To the type
system, a `SomeDoor` is the same type as any other `SomeDoor`. We say that
`SomeDoor` is a `Door s` that is *existentially quantified* over `s`.

By pattern matching into `MkSomeDoor sd d`, we get the value of the door (`d`)
back. However, you can’t know the *type* of the door (the `s` in `d :: Door s`)
because its source could have been *any* type. The type system requires you to
treat `d` as if it could be a door of *any* status.

However, because we have `sd`, we can pattern match on it to recover the
original *type* of the door, as well. This is because whatever type `d`
originally had, `sd` *had* to use the constructor corresponding to it. So if `d`
was originally a `Door 'Opened`, `sd` would have to be `SOpened`.

#### Some Lingo

In the language of dependently typed programming, we call `SomeDoor` a
**dependent sum**, because you can imagine it basically as:

``` {.haskell}
data SomeDoor = SDOpened (SingDS 'Opened) (Door 'Opened)
              | SDClosed (SingDS 'Closed) (Door 'Closed)
              | SDLocked (SingDS 'Locked) (Door 'Locked)
```

A three-way sum between a `Door 'Opened`, a `Door 'Closed`, and a
`Door 'Locked`, essentially. If you have a `SomeDoor`, it’s *either* an opened
door, a closed door, or a locked door.

You might also see `SomeDoor` called a **dependent pair**, because it’s
basically an existentially quantified tuple of the type (the `s`, witnessed by
the `SingDS s`) with a value (the `Door s`).

### Types at Runtime

With this last tool, we finally have enough to build a function to “make” a door
with the status unknown until runtime:

``` {.haskell}
mkSomeDoor :: DoorState -> SomeDoor
mkSomeDoor = \case
    Opened -> MkSomeDoor SOpened (mkDoor SOpened)
    Closed -> MkSomeDoor SClosed (mkDoor SClosed)
    Locked -> MkSomeDoor SLocked (mkDoor SLocked)
```

``` {.haskell}
ghci> let mySomeDoor = mkSomeDoor Opened
ghci> :t mySomeDoor
SomeDoor
ghci> putStrLn $ case mySomeDoor of
        MkSomeDoor SOpened _ -> "mySomeDoor was opened!"
        MkSomeDoor SClosed _ -> "mySomeDoor was closed!"
        MkSomeDoor SLocked _ -> "mySomeDoor was locked!"
mySomeDoor was opened!
```

Using `mkSomeDoor`, we can truly pass in a `DoorState` that we generate at
runtime (from IO, or a user prompt, or a configuration file, maybe), and create
a `Door` based on it.

The Singletons Library
----------------------

Now that we understand some of the benefits of singletons as they relate to
phantom types, we can appreciate what the singletons *library* has to offer: a
fully unified, coherent system for working with singletons of almost *all*
Haskell types!

First, there’s Template Haskell for generating our singletons given our type:

``` {.haskell}
data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

genSingletons [''DoorState]

-- or

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])
```

This generates, for us:

``` {.haskell}
-- not the actual code, but essentially what happens
data Sing :: DoorState -> Type where
    SOpened :: Sing 'Opened
    SClosed :: Sing 'Closed
    SLocked :: Sing 'Locked
```

`Sing` is a poly-kinded type constructor (family). `STrue :: Sing 'True` is the
singleton for `True`, `SJust SOpened :: Sing ('Just 'Opened)` is the singleton
for `Just Opened`, etc.

It also generates us instances for `SingI`, a poly-kinded typeclass:

``` {.haskell}
instance SingI 'Opened where
    sing = SOpened
instance SingI 'Closed where
    sing = SClosed
instance SingI 'Locked where
    sing = SLocked
```

Which is basically our `SingDSI` typeclass, except we have instances for
singletons of all kinds! (heh) There’s a `SingI` instance for `'True`, a `SingI`
instance for `10`, a `SingI` instance for `'Just 'Opened`, etc.:

``` {.haskell}
ghci> sing :: Sing 'True
STrue
ghci> sing :: Sing ('Just 'Opened)
SJust SOpened
```

The great thing about the library is that these types and instances are
generated, that they’re correct (note that I could have implemented `SingDSI`
incorrectly…using the library guarantees that I don’t), and that they all work
together.

Note that if you have singletons for a kind `k`, you also have instances for
kind `Maybe k`, as well. And also for `[k]`, even!

``` {.haskell}
ghci> :t SOpened `SCons` SClosed `SCons` SLocked `SCons` SNil
Sing '[ 'Opened, 'Closed, 'Locked ]
```

(Remember that, because of `DataKinds`, `Maybe` is a kind constructor, who has
two type constructors, the type `'Nothing` and the type constructor
`'Just :: k -> Maybe k`)

Singletons for all integrate together seamlessly, and you have mechanisms to
generate them for your own type and roll it all into the system!

### Extra Goodies

In addition to generating singletons for our libraries, it gives us convenient
functions for working with the different “manifestations” of our types.

Recall that `DoorState` has four different things associated with it now:

1.  The *type* `DoorState`, whose value constructors are `Opened`, `Closed`, and
    `Locked`.
2.  The *kind* `DoorState`, whose type constructors are `'Opened`, `'Closed`,
    and `'Locked`
3.  The singletons for `'Opened`, `'Closed`, and `'Locked`.
4.  The `SingI` instances for `'Opened`, `'Closed`, and `'Locked'`

Kind of confusing, and in the future, when we have real dependent types, we can
hopefully combine all of these manifestations into the same thing. But for now,
we do have to deal with converting between them, and for that, we have,
generated for us:

-   `fromSing :: Sing (s :: DoorState) -> DoorState` takes us from singletons to
    values:

    ``` {.haskell}
    ghci> fromSing SOpened
    Opened
    ```

-   `toSing :: DoorState -> SomeSing DoorState` takes us from values to their
    (existentially quantified) singletons

    ``` {.haskell}
    ghci> let s = toSing Opened
    ghci> :t s
    s :: SomeSing DoorState
    ghci> putStrLn $ case s of
            SomeSing SOpened -> "Opened."
            SomeSing SClosed -> "SClosed."
            SomeSing SLocked -> "SLocked."
    "Opened."
    ```

    `SomeSing` is like `SomeDoor`, an existentially quantified singleton:

    ``` {.haskell}
    data SomeSing DoorState :: Type where
        SomeSing :: Sing s -> SomeSing DoorState

    -- or, more accurately, since `SomeSing` is polykinded
    data SomeSing :: k -> Type where
        SomeSing :: Sing (a ;: k) -> SomeSing k
    ```

It does this by defining a type class (actually, a “kind class”), `SingKind`,
associating each type to the corresponding datakinds-generated kind. The
`SingKind` instance for `DoorState` links the type `DoorState` to the kind
`DoorState`.

There are definitely more useful utility functions, but we will investigate
these later on in the series! For now, you can look at the
[documentation](http://hackage.haskell.org/package/singletons/docs/Data-Singletons.html)
for the library to see more interesting utility functions!

The Singularity
---------------

In this post, we looked the opportunity for using the singleton pattern to give
us more power when using phantom types, enabling us to do things that we
normally couldn’t do. Then, we looked at how the *singletons* library makes
using this pattern extremely easy and smooth to integrate into your existing
code.

You can see all of the “manual singletons” code in this post
[here](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/DoorSingletons.hs),
and then see the code re-implemented using the *singletons* library
\[here\]\[singletons-door\].

Today we almost exclusively looked at using the *singletons* package and the
singleton pattern as it applies to programming with phantom types. However, you
also might have heard about singletons enabling “type-level programming”, as
well, and providing tools for writing writing full programs at the type level.

Be sure to come back as we go deeper into more advanced techniques for
programming with singletons for phantom types, and then also into the wonderful
world that is type-level programming!

To me, before I began using the singletons library, I had all sorts of ad-hoc
trickery everywhere, and every single thing I did at the type level was a bunch
of one-off hacks and tricks. Once I found singletons, however, everything was
revealed, and all became unified, neat, and tidy, utilizing the same mechanisms
for everything. I hope you could see this during this post, but if you didn’t,
it will definitely become a recurring theme in this series!

As always, let me know in the comments if you have any questions! You can also
usually find me idling on the freenode `#haskell` channel, as well, as *jle\`*.
Happy haskelling!
