Practical Dependent Types in Haskell: Type-Safe Neural Networks
===============================================================

(Originally posted by Justin Le [https://blog.jle.im/])

Whether you like it or not, programming with dependent types in Haskell
moving slowly but steadily to the mainstream of Haskell programming. In
the current state of Haskell education, dependent types are often
considered topics for “advanced” Haskell users. However, I can
definitely foresee a day where the ease of use of modern Haskell
libraries relying on dependent types as well as their ubiquitousness
forces programming with dependent types to be an integral part of
regular intermediate (or even beginner) Haskell education, as much as
Traversable or Maps.

The point of this post is to show some practical examples of using
dependent types in the real world, and to also walk through the “why”
and high-level philosophy of the way you structure your Haskell
programs. It’ll also hopefully instill an intuition of a dependently
typed work flow of “exploring” how dependent types can help your current
programs.

The first project in this series will build up to type-safe
**[artificial neural
network](https://en.wikipedia.org/wiki/Artificial_neural_network)**
implementations. Hooray!

Neural Networks
---------------

[Artificial neural
networks](https://en.wikipedia.org/wiki/Artificial_neural_network) have
been somewhat of a hot topic in computing recently. At their core they
involve matrix multiplication and manipulation, so they do seem like a
good candidate for a dependent types. Most importantly, implementations
of training algorithms (like back-propagation) are tricky to implement
correctly — despite being simple, there are many locations where
accidental bugs might pop up when multiplying the wrong matrices, for
example.

It’s not always easy to gauge before-the-fact what would or would not be
a good candidate for adding dependent types to, and often times, it can
be considered premature to start off with “as powerful types as you
can”. So let’s walk through programming things with as “dumb” types as
possible, and see where types can help.

Edwin Brady calls this process “type-driven development”. Start general,
recognize the partial functions and red flags, and slowly add more
powerful types.

### Background

![Feed-forward ANN
architecture](/img/entries/dependent-haskell-1/ffneural.png "Feed-forward ANN architecture")

Here’s a quick run through on background for ANN’s — but remember, this
isn’t an article on ANN’s, so we are going to be glossing over some of
this :) Feel free to explore this further too.

We’re going to be implementing a feed-forward neural network, with
back-propagation training. These networks are layers of “nodes”, each
connected to the each of the nodes of the previous layer. Input goes to
the first layer, which feeds information to the next year, which feeds
it to the next, etc., until the final layer, where we read it off as the
“answer” that the network is giving us. Layers between the input and
output layers are called *hidden* layers. Every node “outputs” a
weighted sum of all of the outputs of the *previous* layer, plus an
always-on “bias” term (so that its result can be non-zero even when all
of its inputs are zero). Symbolically, it looks like:

$$
y_j = b_j + \sum_i^m w_{ij} x_i
$$

Or, if we treat the output of a layer and the list of list of weights as
a matrix, we can write it a little cleaner:

$$
\mathbf{y} = \mathbf{b} + W \mathbf{x}
$$

To “scale” the result (and to give the system the magical powers of
nonlinearity), we actually apply an “activation function” to the output
before passing it down to the next step. We’ll be using the popular
[logistic function](https://en.wikipedia.org/wiki/Logistic_function),
$f(x) = 1 / (1 + e^{-x})$.

*Training* a network involves picking the right set of weights to get
the network to answer the question you want.

Vanilla Types
-------------

We can store a network by storing the matrix of of weights and biases
between each layer:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs#L17-19
data Weights = W { wBiases :: !(Vector Double)  -- n
                 , wNodes  :: !(Matrix Double)  -- n x m
                 }                              -- "m to n" layer

```

Now, a `Weights` linking an *m*-node layer to an *n*-node layer has an
*n*-dimensional bias vector (one component for each output) and an
*n*-by-*m* node weight matrix (one column for each output, one row for
each input).

(We’re using the `Matrix` type from the awesome
*[hmatrix](http://hackage.haskell.org/package/hmatrix)* library for
performant linear algebra, implemented using blas/lapack under the hood)

A feed-forward neural network is then just a linked list of these
weights:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs#L21-27
data Network :: * where
    O     :: !Weights
          -> Network
    (:&~) :: !Weights
          -> !Network
          -> Network
infixr 5 :&~

```

Note that we’re using [GADT](https://en.wikibooks.org/wiki/Haskell/GADT)
syntax here, which just lets us define `Network` by providing the type
of its *constructors*, `O` and `(:&~)`. A network with one input layer,
two inner layers, and one output layer would look like:

``` {.haskell}
ih :&~ hh :&~ O ho
```

The first component is the weights from the input to first inner layer,
the second is the weights between the two hidden layers, and the last is
the weights between the last hidden layer and the output layer.

<!-- TODO: graphs using diagrams? -->
We can write simple procedures, like generating random networks:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs#L45-55
randomWeights :: MonadRandom m => Int -> Int -> m Weights
randomWeights i o = do
    s1 <- getRandom
    s2 <- getRandom
    let wB = randomVector s1 Uniform o * 2 - 1
        wN = uniformSample s2 o (replicate i (-1, 1))
    return $ W wB wN

randomNet :: MonadRandom m => Int -> [Int] -> Int -> m Network
randomNet i [] o     =     O <$> randomWeights i o
randomNet i (h:hs) o = (:&~) <$> randomWeights i h <*> randomNet h hs o

```

(`randomVector` and `uniformSample` are from the *hmatrix* library,
generating random vectors and matrices from a random `Int` seed. We
configure them to generate them with numbers between -1 and 1)

And now a function to “run” our network on a given input vector,
following the matrix equation we wrote earlier:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs#L29-43
logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

runLayer :: Weights -> Vector Double -> Vector Double
runLayer (W wB wN) v = wB + wN #> v

runNet :: Network -> Vector Double -> Vector Double
runNet (O w)      !v = logistic (runLayer w v)
runNet (w :&~ n') !v = let v' = logistic (runLayer w v)
                       in  runNet n' v'

```

(`#>` is matrix-vector multiplication)

<!-- TODO: examples of running -->
If you’re a non-Haskell programmer, this might all seem perfectly fine
and normal, and you probably have only a slightly elevated heart rate.
If you are a Haskell programmer, you are most likely already having
heart attacks. Let’s imagine all of the bad things that could happen:

-   How do we know that we didn’t accidentally mix up the dimensions for
    our implementation of `randomWeights`? We could have switched
    parameters and be none the wiser.

-   How do we even know that each subsequent matrix in the network is
    “compatible”? We want the outputs of one matrix to line up with the
    inputs of the next, but there’s no way to know unless we have “smart
    constructors” to check while we add things. But it’s possible to
    build a bad network, and things will just explode at runtime.

-   How do we know the size vector the network expects? What stops you
    from sending in a bad vector at run-time?

-   How do we verify that we have implemented `runLayer` and `runNet` in
    a way that they won’t suddenly fail at runtime? We write `l #> v`,
    but how do we know that it’s even correct…what if we forgot to
    multiply something, or used something in the wrong places? We can it
    prove ourselves, but the compiler won’t help us.

### Back-propagation

Now, let’s try implementing back-propagation! It’s a basic “gradient
descent” algorithm. There are [many
explanations](https://en.wikipedia.org/wiki/Backpropagation) on the
internet; the basic idea is that you try to minimize the squared “error”
of what the neural network outputs for a given input vs. the actual
expected output. You find the direction of change that minimizes the
error, and move that direction. The implementation of Feed-forward
backpropagation is found in many sources online and in literature, so
let’s see the implementation in Haskell:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs#L57-93
train :: Double           -- ^ learning rate
      -> Vector Double    -- ^ input vector
      -> Vector Double    -- ^ target vector
      -> Network          -- ^ network to train
      -> Network
train rate x0 target = fst . go x0
  where
    go :: Vector Double    -- ^ input vector
       -> Network          -- ^ network to train
       -> (Network, Vector Double)
    go !x (O w@(W wB wN))
        = let y    = runLayer w x
              o    = logistic y
              -- the gradient (how much y affects the error)
              --   (logistic' is the derivative of logistic)
              dEdy = logistic' y * (o - target)
              -- new bias weights and node weights
              wB'  = wB - scale rate dEdy
              wN'  = wN - scale rate (dEdy `outer` x)
              w'   = W wB' wN'
              -- bundle of derivatives for next step
              dWs  = tr wN #> dEdy
          in  (O w', dWs)
    go !x (w@(W wB wN) :&~ n)
        = let y          = runLayer w x
              o          = logistic y
              -- get dWs', bundle of derivatives from rest of the net
              (n', dWs') = go o n
              -- the gradient (how much y affects the error)
              dEdy       = logistic' y * dWs'
              -- new bias weights and node weights
              wB'  = wB - scale rate dEdy
              wN'  = wN - scale rate (dEdy `outer` x)
              w'   = W wB' wN'
              -- bundle of derivatives for next step
              dWs  = tr wN #> dEdy
          in  (w' :&~ n', dWs)

```

The algorithm computes the *updated* network by recursively updating the
layers, from the output layer all the way up to the input layer. At
every step it returns the updated layer/network, as well as a bundle of
derivatives for the next layer to use to calculate its descent
direction. At the output layer, all it needs to calculate the direction
of descent is just `o - targ`, the target. At the inner layers, it has
to use the `dWs` bundle to figure it out.

Writing this is a bit of a struggle. Truth be told, I implemented this
incorrectly several times before writing it now as you see here. The
type system doesn’t help you like it normally does in Haskell, and you
can’t really use parametricity to help you write your code like normal
Haskell. Everything is monomorphic, and everything multiplies with
everything else. You don’t have any hits about what to multiply with
what at any point in time. Seeing the implementation here basically
amplifies and puts on displays all of the red flags/awfulness mentioned
before.

In short, you’re leaving yourself open to many potential bugs…and the
compiler doesn’t help you write your code at all! This is the nightmare
of every Haskell programmer. There must be a better way!

#### Putting it to the test

Pretty much the only way you can verify this code is to test it out on
example cases. In the [source
file](https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs)
I have `main` test out the backprop, training a network on a 2D function
that was “on” for two small circles and “off” everywhere else (A nice
cute non-linearly-separable function to test our network on). We
basically train the network to be able to recognize the two-circle
pattern. I implemented a simple printing function and tested the trained
network on a grid:

``` {.bash}
$ stack install hmatrix MonadRandom
$ stack ghc -- -O2 ./NetworkUntyped.hs
$ ./NetworkUntyped
# Training network...
#
#
#            .=########=
#          .##############.
#          ################
#          ################
#          .##############-
#            .###########
#                 ...             ...
#                             -##########.
#                           -##############.
#                           ################
#                           ################
#                            =############=
#                              .#######=.
#
#
```

Not too bad! But, I was basically forced to resort to unit testing to
ensure my code was correct. Let’s see if we can do better.

### The Call of Types

Before we go on to the “typed” version of our program, let’s take a step
back and look at some big checks you might want to ask yourself after
you write code in Haskell.

1.  Are any of my functions partial, or implemented using partial
    functions?
2.  How could I have written things that are *incorrect*, and yet still
    type check? Where does the compiler *not* help me by restricting my
    choices?

Both of these questions usually yield some truth about the code you
write and the things you should worry about. As a Haskeller, they should
always be at the back of your mind!

Looking back at our untyped implementation, we notice some things:

1.  Almost every single function we wrote is partial. If we had passed
    in the incorrectly sized matrix/vector, or stored mismatched vectors
    in our network, everything would fall apart.
2.  There are literally billions of ways we could have implemented our
    functions where they would still typechecked. We could multiply
    mismatched matrices, or forget to multiply a matrix, etc.

With Static Types
-----------------

### Networks

Gauging our potential problems, it seems like the first major class of
bugs we can address is improperly sized and incompatible matrices. If
the compiler always made sure we used compatible matrices, we can avoid
bugs at compile-time, and we also can get a friendly helper when we
write programs.

Let’s write a `Weights` type that tells you the size of its output and
the input it expects. Let’s have, say, a `Weights 10 5` be a set of
weights that takes you from a layer of 10 nodes to a layer of 5 nodes.
`w :: Weights 4 6` would take you from a layer of 4 nodes to a layer of
6 nodes:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped.hs#L22-24
data Weights i o = W { wBiases :: !(R o)
                     , wNodes  :: !(L o i)
                     }

```

We’re using the `Numeric.LinearAlgebra.Static` module from
*[hmatrix](http://hackage.haskell.org/package/hmatrix)*, which offers
matrix and vector types with their size in their types: an `R 5` is a
vector of Doubles with 5 elements, and a `L 3 6` is a 3x6 vector of
Doubles.

The `Static` module relies on the `KnownNat` mechanism that GHC offers.
Almost all operations in the library require a `KnownNat` constraint on
the type-level Nats — for example, you can take the dot product of two
vectors with `dot :: KnownNat n => R n -> R n`. The `KnownNat`
constraint allows the functions to *use* the information in the size
parameter (the `Integer` it represents) at run-time. (More on this
later!)

Following this, a reasonable type for a *network* might be
`Network 10 2`, taking 10 inputs and popping out 2 outputs. This might
be an ideal type to export, because it abstracts away the size of the
hidden layers. But it’d be nice for us to keep all of the hidden layers
in the type for now — we’ll see how it can be useful, and we’ll also
talk about how to later hide/abstract it away when we export the type.

Our network type for this post will be something like
`Network 10 '[7,5,3] 2`: Take 10 inputs, return 2 outputs — and
internally, have hidden layers of size 7, 5, and 3. (The `'[7,5,3]` is a
type-level list of Nats; the optional `'` apostrophe is just for our own
benefit to distinguish it from a value-level list of integers.)

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped.hs#L26-33
data Network :: Nat -> [Nat] -> Nat -> * where
    O     :: !(Weights i o)
          -> Network i '[] o
    (:&~) :: KnownNat h
          => !(Weights i h)
          -> !(Network h hs o)
          -> Network i (h ': hs) o
infixr 5 :&~

```

We use GADT syntax here again. The *kind signature* of the type
constructor means that the `Network` type constructor takes three
inputs: a `Nat` (type level number, like `10` or `5`), list of `Nat`s,
and another `Nat` (the input, hidden layers, and output sizes). Let’s go
over the two constructors.

-   The `O` constructor takes a `Weights i o` and returns a
    `Network i '[] o`. That is, if your network is just weights from `i`
    inputs to `o` outputs, your network itself just takes `i` inputs and
    returns `o` outputs, with no hidden layers.

-   The `(:&~)` constructor takes a `Network h hs o` – a network with
    `h` inputs and `o` outputs – and “conses” an extra input layer
    in front. If you give it a `Weights i h`, its outputs fit perfectly
    into the inputs of the subnetwork, and you get a
    `Network i (h ': hs) o`. (`(':)`, or `(:)`, is the same as normal
    `(:)`, but is for type-level lists. The apostrophe is optional here
    too, but it’s just nice to be able to visually distinguish the two)

    We add a `KnownNat` constraint on the `h`, so that whenever you
    pattern match on `w :&~ net`, you automatically get a `KnownNat`
    constraint for the input size of `net` that the *hmatrix* library
    can use.

We can still construct them the same way:

``` {.haskell}
-- given:
ho :: Weights  4 2
hh :: Weights  7 4
ih :: Weights 10 7

-- we have:
              O ho      :: Network  4 '[] 2
       hh :&~ O ho      :: Network  7 '[4] 2
ih :&~ hh :&~ O ho      :: Network 10 '[7,4] 2
```

Note that the shape of the constructors requires all of the weight
vectors to “fit together” Now if we ever pattern match on `:&~`, we know
that the resulting matrices and vectors are compatible!

One neat thing is that this approach is also self-documenting. I don’t
need to specify what the dimensions are in the docs and trust the users
to read it. The types tell them! And if they don’t listen, they get a
compiler error!

Generating random weights and networks is even nicer now:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped.hs#L57-64
randomWeights :: (MonadRandom m, KnownNat i, KnownNat o)
              => m (Weights i o)
randomWeights = do
    s1 <- getRandom
    s2 <- getRandom
    let wB = randomVector s1 Uniform * 2 - 1
        wN = uniformSample s2 (-1) 1
    return $ W wB wN

```

Notice that the `Static` versions of `randomVector` and `uniformSample`
don’t actually require the size of the vector/matrix you want as an
input – they just use type inference to figure out what size you want!
This is the same process that `read` uses to figure out what type of
thing you want to return. You would use
`randomVector s Uniform :: R 10`, and type inference would give you a
10-element vector the same way `read "hello" :: Int` would give you an
`Int`.

Here’s something important: note that it’s much harder to implement this
incorrectly. Before, you could give the matrix the wrong dimensions
(maybe you flipped the parameters?), or gave the wrong parameter to the
vector generator.

But here, you are guaranteed/forced to return the correctly sized
vectors and matrices. In fact, you *don’t even have to worry* about it —
it’s handled automatically by the magic of type inference! I consider
this a very big victory. One of the whole points of types is to give you
less to “worry about”, as a programmer. Here, we completely eliminate an
*entire dimension* of programmer concern.

### Singletons and Induction

The code for the updated `randomNet` takes a bit of background to
understand, so let’s take a quick detour through singletons and
induction in data types.

Let’s say we want to construct a `Network 4 '[3,2] 1` by implementing an
algorithm that can create any `Network i hs o`. In true Haskell fashion,
we do this recursively (“inductively”). After all, we know how to make a
`Network i '[] o` (just `O <$> randomWieights`), and we know how to
create a `Network i (h ': hs) o` if we had a `Network h hs o`. Now all
we have to do is just “pattern match” on the type-level list, and…

Oh wait. We can’t directly pattern match on lists like that in Haskell.
But what we *can* do is move the list from the type level to the value
level using *singletons*. The
*[typelits-witnesses](http://hackage.haskell.org/package/typelits-witnesses)*
library offers a handy singleton for just this job. If you have a type
level list of nats, you get a `KnowNats ns` constraint. This lets you
create a `NatList`:

``` {.haskell}
data NatList :: [Nat] -> * where
    ØNL   :: NatList '[]
    (:<#) :: (KnownNat n, KnownNats ns)
          => !(Proxy n) -> !(NatList ns) -> NatList (n ': ns)

infixr 5 :<#
```

Basically, a `NatList '[1,2,3]` is `p1 :<# p2 :<# p3 :<# ØNL`, where
`p1 :: Proxy 1`, `p2 :: Proxy 2`, and `p3 :: Proxy 3`. (Remember,
`data Proxy a = Proxy`; `Proxy` is like `()` but with an extra phantom
type parameter)

We can spontaneously generate a `NatList` for any type-level Nat list
with `natList :: KnownNats ns => NatList ns`:

``` {.haskell}
ghci> natList :: NatList '[1,2,3]
Proxy :<# Proxy :<# Proxy :<# ØNL
-- ^         ^         ^
-- `-- :: Pro|xy 1     |
--           `-- :: Pro|xy 2
--                     `-- :: Proxy 3
```

Now that we have an actual value-level *structure* (the list of
`Proxy`s), we can now essentially “pattern match” on `hs`, the type — if
it’s empty, we’ll get the `ØNL` constructor when we use `natList`,
otherwise we’ll get the `(:<#)` constructor, etc.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped.hs#L66-70
randomNet :: forall m i hs o. (MonadRandom m, KnownNat i, KnownNats hs, KnownNat o)
          => m (Network i hs o)
randomNet = case natsList :: NatList hs of
              ØNL     -> O     <$> randomWeights
              _ :<# _ -> (:&~) <$> randomWeights <*> randomNet

```

(Note that we need `ScopedTypeVariables` and explicit `forall` so that
can say `NatList hs` in the function body.)

The reason why `NatList` and `:<#` works for this is that its
constructors *come with proofs* that the head’s type has a `KnownNat`
and the tail’s type has a `KnownNats`. It’s a part of the GADT
declaration. If you ever pattern match on `p :<# ns`, you get a
`KnownNat n` constraint (that `randomWeights` uses) for the
`p :: Proxy n`, and also a `KnownNats ns` constraint (that the recursive
call to `randomNet` uses).

This is a common pattern in dependent Haskell: “building up” a
value-level singleton *structure* from a type that we want (often done
through typeclasses like `KnownNats`) and then inductively piggybacking
on that structure’s constructors to build the thing you *really* want
(called “elimination”). Here, we use `KnownNats hs` to build our
`NatList hs` structure, and use/“eliminate” that structure to create our
`Network i hs o`.

Along the way, the singletons and the typeclasses and the types play an
intricate dance. `randomWeights` needed a `KnownNat` constraint. Where
did it *come* from?

#### On Typeclasses

`natList` uses the `KnownNat n` to construct the `NatList ns` (because
any time you use `(:<#)`, you need a `KnownNat`). Then, when you pattern
match on the `(:<#)` in `randomNet`, you “release” the `KnownNat n` that
was stuffed in there by `natList`.

People say that pattern matching on `(:<#)` gives you a “context” in
that case-statement-branch where `KnownNat n` is in scope and satisfied.
But sometimes it helps to think of it in the way we just did — the
instance *itself* is actually a “thing” that gets passed around through
typeclasses and GADT constructors/deconstructors. The `KnownNat`
*instance* gets put *into* `:<#` by `natList`, and is then taken *out*
in the pattern match so that `randomWeights` can use it. (When we match
on `_ :<# _`, we really are saying that we don’t care about the normal
contents — we just want the typeclass instances that the constructor is
hiding!)

At a high-level, you can see that this is really no different than just
having a plain old `Integer` that you “put in” to the constructor (as an
extra field), and which you then take out if you pattern match on it.
Really, every time you see `KnownNat n => ..`, you can think of it as an
`Integer -> ..` (because all the typeclass is is a way to get an
`Integer` out of it with `natVal`). `(:<#)` requiring a `KnownNat n =>`
put into it is really the same as requiring an `Integer` in it, which
the pattern-matcher can then take out.

The difference is that GHC and the compiler can now “track” these at
compile-time to give you rudimentary checks on how your Nat’s act
together on the type level, allowing it to catch mismatches with
compile-time checks instead of run-time checks.

### Running with it

So now, you can use `randomNet :: IO (Network 5 '[4,3] 2)` to get a
random network of the desired dimensions!

Can we just pause right here to just appreciate how awesome it is that
we can generate random networks of whatever size we want by *just
requesting something by its type*?

Our implementation is also *guaranteed* to have the right sized
matrices…no worrying about using the right size parameters for the right
matrix in the right oder. GHC does it for you automatically!

The code for *running* the nets is actually literally identical:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped.hs#L43-55
runLayer :: (KnownNat i, KnownNat o)
         => Weights i o
         -> R i
         -> R o
runLayer (W wB wN) v = wB + wN #> v

runNet :: (KnownNat i, KnownNat o)
       => Network i hs o
       -> R i
       -> R o
runNet (O w)      !v = logistic (runLayer w v)
runNet (w :&~ n') !v = let v' = logistic (runLayer w v)
                       in  runNet n' v'

```

But now, we get the assurance that the matrices and vectors all fit
each-other, at compile-time. GHC basically writes our code for us. The
operations all demand vectors and matrices that “fit together”:

``` {.haskell}
(+)  :: KnownNat n
     => R n -> R n -> R n
(#>) :: (KnownNat n, KnownNat m)
     => L n m -> R m -> R n

logistic :: KnownNat n
         => R n -> R n
```

So you can only ever multiply a matrix by a properly sized vector!

Our back-prop algorithm is ported pretty nicely too:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped.hs#L72-110
train :: forall i hs o. (KnownNat i, KnownNat o)
      => Double           -- ^ learning rate
      -> R i              -- ^ input vector
      -> R o              -- ^ target vector
      -> Network i hs o   -- ^ network to train
      -> Network i hs o
train rate x0 target = fst . go x0
  where
    go  :: forall j js. KnownNat j
        => R j              -- ^ input vector
        -> Network j js o   -- ^ network to train
        -> (Network j js o, R j)
    go !x (O w@(W wB wN))
        = let y    = runLayer w x
              o    = logistic y
              -- the gradient (how much y affects the error)
              --   (logistic' is the derivative of logistic)
              dEdy = logistic' y * (o - target)
              -- new bias weights and node weights
              wB'  = wB - konst rate * dEdy
              wN'  = wN - konst rate * (dEdy `outer` x)
              w'   = W wB' wN'
              -- bundle of derivatives for next step
              dWs  = tr wN #> dEdy
          in  (O w', dWs)
    go !x (w@(W wB wN) :&~ n)
        = let y          = runLayer w x
              o          = logistic y
              -- get dWs', bundle of derivatives from rest of the net
              (n', dWs') = go o n
              -- the gradient (how much y affects the error)
              dEdy       = logistic' y * dWs'
              -- new bias weights and node weights
              wB'  = wB - konst rate * dEdy
              wN'  = wN - konst rate * (dEdy `outer` x)
              w'   = W wB' wN'
              -- bundle of derivatives for next step
              dWs  = tr wN #> dEdy
          in  (w' :&~ n', dWs)

```

It’s pretty much an exact copy-and-paste, but now with GHC checking to
make sure everything fits together in our implementation.

One thing that’s hard for me to convey here without walking through the
implementation step-by-step is how much the types *help you* in writing
this code.

Before starting writing a back-prop implementation without the help of
types, I’d probably be a bit concerned. I mentioned earlier that writing
the untyped version was no fun at all.

But, with the types, writing the implementation became a *joy* again.
And, you have the help of *hole driven development*, too.

The basic thought process goes like this:

1.  “I need an `R n`…how do I get it?”

    “Oh! I have an `L n m` and an `R m`, I can just multiply them!”

2.  “I need something of *this* type…how can I make it?”

    “Ah, there’s only one possible way to get this.” Parametric
    polymorphism to the rescue.

3.  “I need to multiply this with *something*…but what?”

    “Oh, if I just leave a `_` hole there, GHC will actually tell me
    *everything in scope* that can fit there!”

The more you can restrict the implementations of your functions with
your types, the more of a joy programming in Haskell is. Things fit
together and fall together before your eyes…and the best part is that if
they’re wrong, the compiler will nudge you gently into the correct
direction.

The most stressful part of programming happens when you have to
tenuously hold a complex and fragile network of ideas and constraints in
your brain, and any slight distraction or break in focus causes
everything to crash down in your mind. Over time, people have began to
believe that this is “normal”, and a sign of a good programmer. Don’t
believe this lie — it’s not! A good programming experience involves
maintaining as *little* in your head as possible, and letting the
compiler handle remembering/checking the rest!

#### The final test

You can download the [typed
network](https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped.hs)
source code and run it yourself. Again, the `main` method is written
identically to that of the other file, and tests the identical function.

``` {.bash}
$ stack install hmatrix MonadRandom typelits-witnesses
$ stack ghc -- -O2 ./NetworkTyped.hs
$ ./NetworkTyped
# Training network...
#
#
#             -#########-
#           -#############=
#          -###############-
#          =###############=
#           ##############=.
#            .##########=.
#                               .==#=-
#                            -###########-
#                           =##############.
#                          .###############=
#                           =##############-
#                            =############-
#                              -######=-.
#
#
```
