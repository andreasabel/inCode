Practical Dependent Types in Haskell: Type-Safe Linear Algebra
==============================================================

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

So, the point of this post is to show some practical examples of using
dependent types in the real world, and to also walk through the “why”
and high-level philosophy of the way you structure your Haskell
programs. It’ll also hopefully instill an intuition of a dependently
typed work flow of “exploring” how dependent types can help your current
programs.

The first project in this series will build up to type-safe artificial
neural network implementations. Hooray!

There are other great tutorials I’d recommend online if you want to
explore dependent types in Haskell further, including [this great
servant
“tutorial”](http://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/).
Also, I should provide a disclaimer — I’m also currently exploring all
of this as I’m going along too. It’s a wild world out there. Join me and
let’s be a part of the frontier!

Toy Example: Linear Algebra
---------------------------

We’re going to build up to type-safe neural networks in Haskell, but to
start off, let’s go over a really really small example of dependent
types in Haskell by applying it to the “hello world” of dependent types:
linear algebra.

We’re going to do two simple linear algebra concepts, and see why it’s
super scary to program without dependent types (and wonder how we ever
survived without them), and then add in a bit of dependent types to the
rescue!

For the most part, we’ll be using the awesome
*[hmatrix](http://hackage.haskell.org/package/hmatrix)* library, which
offers both “type-safe” and “non-type-safe” API’s, so we can better
compare the two approaches.

The Scary World of Unsafe Code
------------------------------

The first thing we’re going to do is simple:

1.  Read in two vectors from stdin
2.  Print the dot product of the two

``` {.haskell}
readList :: IO [Double]
readList = concat . readMaybe <$> getLine

dotStdin :: IO ()
dotStdin = do
    v1 <- vector <$> readList
    v2 <- vector <$> readList
    print $ l1 <.> l2
```

Now, already you should be feeling sweaty. The dot product, `<.>`, is
actually *undefined* for two vectors of different lengths. Our code here
is…unsafe! Runtime errors can happen that are unchecked by the compiler.
To fix this in an unsafe language, we’d have to:

1.  Be able to recognize by our own human reasoning that there is a
    potential for a runtime error.
2.  Restructure control flow to avoid situations where the runtime error
    would occur.
3.  Somehow prove to ourselves, as humans, that our new solution
    is safe. Then trust our human verification of our human proof.

Now, (2) might be cumbersome in some cases, but at least it’s usually a
mechanical process. It’s (1) and (3) that are killer. There is no way I
trust myself to recognize *every* potential runtime error. And, in the
case that I *do* actually catch opportunities for runtime errors, I
definitely don’t always trust myself that my fix works. If only the
compiler could handle (1) and (3) for us!

The second task we’ll do is build up a chain of matrices, and then ask
for a vector from stdin to multiply by each one successively.

``` {.haskell}
readChain :: IO [Matrix Double]
readChain = do
    l <- getLine
    case fromLists <$> readMaybe l of
      Just m  -> (m:) <$> readChain
      Nothing -> return []
    
collapseChain :: Vector Double -> Matrix Double -> Vector Double
collapseChain v []     = v
collapseChain v (m:ms) = collapseChain (m #> v) ms

chainMats :: IO ()
chainMats = do
    ms <- readChain
    v  <- vector <$> readList
    print $ collapseChain v ms
```

Now, this should be giving you heart attacks. Matrix-vector
multiplication is only defined when the number of columns the matrix has
is the number of rows the vector has. If *any* of the matrices along the
chain is the wrong dimension, the entire thing will blow up.

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

However, it’s not always easy to gauge before-the-fact what would or
would not be a good candidate for adding dependent types to, and often
times, it can be considered premature to start off with “as powerful
types as you can”. So we’ll walk through a simple implementation
*without*, and see all of the red flags that hint that you might want to
start considering stronger types.

Vanilla Types
-------------

![Feed-forward ANN
architecture](/img/entries/dependent-haskell-1/ffneural.png "Feed-forward ANN architecture")

We’re going to be implementing a feed-forward neural network, with
back-propagation training. A feed-forward neural network consists
structurally of layers of “nodes”, each connected to the each of the
nodes of the previous layer and each of the nodes of the next layer. The
most important feature of the network itself is the “strength” of these
connections, called “weights”. To make a prediction, each node takes, as
an input, the weighted sum of all of the outputs of the previous layer,
weighted by the connection weights (plus an additional “bias” shift). It
then outputs a function of this weighted sum, $f(x)$, to be used by all
of the nodes of the next layer. At the high-level, the user feeds in an
input vector to the top-level nodes, the network processes these
layer-by-layer, and the result of the final nodes is what is taken as
the network’s output. The “goal” of designing/training a network is to
somehow pick the right set of weights that will give the output that you
want for the given input.

While it’s nice to think about neural networks in terms of their nodes,
it makes more sense computationally to only identify a network by simply
the matrices of weights alone — let’s imagine one “layer”, which is
actually a matrix of weights from one layer to another:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs#L10-13
data Weights = W { wBiases  :: !(Vector Double)
                 , wWeights :: !(Matrix Double)
                 }
  deriving (Show, Eq)

```

Now, a `Weights` linking a layer of $n$ nodes to a layer of $m$ nodes
will have a bias vector of size $m$ (the bias shift for each of the
output nodes) and a weight matrix of size $m \times n$.

(We’re using the `Matrix` type from the awesome
*[hmatrix](http://hackage.haskell.org/package/hmatrix)* library for
linear algebra, implemented using blas/lapack under the hood)

Now let’s represent a feed-forward network:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs#L15-18
data Network = O !Weights
             | !Weights :&~ !Network
  deriving (Show, Eq)
infixr 5 :&~

```

So a network with one input layer, two inner layers, and one output
layer would look like:

``` {.haskell}
i2h :&~ h2h :&~ O h2o
```

Where the first component is the weights from the input to the first
hidden layer, the second is the weights from the first hidden layer to
the second, and the final is the weights from the second hidden layer to
the outputs.

TODO: graphs using diagrams?

We can write simple procedures, like generating random networks:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs#L36-46
randomWeights :: MonadRandom m => Int -> Int -> m Weights
randomWeights i o = do
  s1 <- getRandom
  s2 <- getRandom
  let wBiases  = randomVector s1 Uniform o * 2 - 1
      wWeights = uniformSample s2 o (replicate i (-1, 1))
  return W{..}

randomNet :: MonadRandom m => Int -> [Int] -> Int -> m Network
randomNet i [] o     =     O <$> randomWeights i o
randomNet i (h:hs) o = (:&~) <$> randomWeights i h <*> randomNet h hs o

```

(`randomVector` and `uniformSample` are from the *hmatrix* library,
generating random vectors and matrices from a random `Int` seed. We
configure them to generate them with numbers between -1 and 1)

And now a function to “run” our network on a given input vector:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkUntyped.hs#L20-34
logistic :: Double -> Double
logistic x = 1 / (1 + exp (-x))

runLayer :: Weights -> Vector Double -> Vector Double
runLayer (W wB wW) v = wB + (wW #> v)

runNet :: Network -> Vector Double -> Vector Double
runNet (O w)      !v = logistic `cmap` runLayer w v
runNet (w :&~ n') !v = let v' = logistic `cmap` runLayer w v
                       in  runNet n' v'

```

(`#>` is matrix-vector multiplication)

TODO: examples of running

If you’re a normal programmer, this might seem perfectly fine. If you
are a Haskell programmer, you should already be having heart attacks.
Let’s imagine all of the bad things that could happen:

-   How do we even know that each subsequent matrix in the network is
    “compatible”? We want the outputs of one matrix to line up with the
    inputs of the next, but there’s no way to know unless we have “smart
    constructors” to check while we add things. But it’s possible to
    build a bad network, and things will just explode at runtime.

-   How do we know the size vector the network expects? What stops you
    from sending in a bad vector at run-time and having everything
    explode?

-   How do we verify that we have implemented `runLayer` and `runNet` in
    a way that they won’t suddenly fail at runtime? We write `l #> v`,
    but how do we know that it’s even correct? We can it prove
    ourselves, but the compiler won’t help us.

Now, let’s try implementing back-propagation:

``` {.haskell}
train :: Vector Double -> Vector Double -> Network -> Network
train i o = go i
  where
    go :: Vector Double -> Network -> (Vector Double, Network)
    go = undefined
```
