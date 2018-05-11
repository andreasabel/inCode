A Purely Functional Typed Approach to Trainable Models
======================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/purely-functional-typed-models.html)

With the release of [backprop](http://hackage.haskell.org/package/backprop),
I've been exploring the space of parameterized models of all sorts, from linear
and logistic regression and other statistical models to artificial neural
networks, feed-forward and recurrent (stateful). I wanted to see to what extent
we can really apply automatic differentiation and iterative gradient
decent-based training to all of these different models.

I'm starting to see a picture unifying all of these models, painted in the
language of purely typed functional programming. I'm already applying these to
models I'm using in real life and in my research, and I thought I'd take some
time to put my thoughts to writing in case anyone else finds these illuminating
or useful.

As a big picture, I really believe that a purely functional typed approach is
*the* way to move forward in the future for models like artificial neural
networks -- and that one day, object-oriented and imperative approaches will
seem quaint.

I'm not the first person to attempt to build a conceptual framework for these
types of models in a purely functional typed sense -- [Christopher Olah's famous
post](http://colah.github.io/posts/2015-09-NN-Types-FP/) comes to mind, and is
definitely worth a read. However, Olah's post is more of an abstract piece; the
approach I am describing here can be applied *today*, to start building and
*discovering* effective models and training them. And I have code! :)

The code in this post is written in Haskell, using the
[backprop](http://hackage.haskell.org/package/backprop),
[hmatrix](http://hackage.haskell.org/package/hmatrix) (with
[hmatrix-backprop](http://hackage.haskell.org/package/hmatrix-backprop)), and
[vector-sized](http://hackage.haskell.org/package/vector-sized) libraries.

Essence of a Model
------------------

For the purpose of this post, a *parameterized model* is a function from some
input "question" (predictor, independent variable) to some output "answer"
(predictand, dependent variable)

Notationally, we might write it as a function:

![
f\_p(x) = y
](https://latex.codecogs.com/png.latex?%0Af_p%28x%29%20%3D%20y%0A "
f_p(x) = y
")

The important thing is that, for every choice of *parameterization*
![p](https://latex.codecogs.com/png.latex?p "p"), we get a *different function*
![f\_p(x)](https://latex.codecogs.com/png.latex?f_p%28x%29 "f_p(x)").

For example, you might want to write a model that, when given an email, outputs
whether or not that email is spam.

The parameterization *p* is some piece of data that we tweak to produce a
different ![f\_p(x)](https://latex.codecogs.com/png.latex?f_p%28x%29 "f_p(x)").
So, "training" (or "learning", or "estimating") a model is a process of picking
the ![p](https://latex.codecogs.com/png.latex?p "p") that gives the "correct"
function ![f\_p(x)](https://latex.codecogs.com/png.latex?f_p%28x%29 "f_p(x)")
--- that is, the function that accurately predicts spam or whatever thing you
are trying to predict.

For example, for [linear
regression](https://en.wikipedia.org/wiki/Linear_regression), you are trying to
"fit" your
![(x, y)](https://latex.codecogs.com/png.latex?%28x%2C%20y%29 "(x, y)") data
points to some function
![f(x) = \\beta + \\alpha x](https://latex.codecogs.com/png.latex?f%28x%29%20%3D%20%5Cbeta%20%2B%20%5Calpha%20x "f(x) = \beta + \alpha x").
The *parameters* are
![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\alpha") and
![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\beta"), the *input* is
![x](https://latex.codecogs.com/png.latex?x "x"), and the *output* is
![\\beta + \\alpha x](https://latex.codecogs.com/png.latex?%5Cbeta%20%2B%20%5Calpha%20x "\beta + \alpha x").

As it so happens, a
![f\_p(x)](https://latex.codecogs.com/png.latex?f_p%28x%29 "f_p(x)") is really
just a "partially applied"
![f(p,x)](https://latex.codecogs.com/png.latex?f%28p%2Cx%29 "f(p,x)"). Imagining
that function, it has type:[^1]

![
f : P \\times A \\rightarrow B
](https://latex.codecogs.com/png.latex?%0Af%20%3A%20P%20%5Ctimes%20A%20%5Crightarrow%20B%0A "
f : P \times A \rightarrow B
")

If we [curry](https://en.wikipedia.org/wiki/Currying) this, we get the original
model representation we talked about:

![
f : P \\rightarrow (A \\rightarrow B)
](https://latex.codecogs.com/png.latex?%0Af%20%3A%20P%20%5Crightarrow%20%28A%20%5Crightarrow%20B%29%0A "
f : P \rightarrow (A \rightarrow B)
")

### Optimizing Models with Observations

Something interesting happens if we flip the script. What if, instead of
![f\_p(x)](https://latex.codecogs.com/png.latex?f_p%28x%29 "f_p(x)"), we talked
about ![f\_x(p)](https://latex.codecogs.com/png.latex?f_x%28p%29 "f_x(p)")? That
is, we fix the input and vary the parameter, and see what type of outputs we get
for the same output while we vary the parameter?

If we have an "expected output" for our input, then one thing we can do is look
at ![f\_p(x)](https://latex.codecogs.com/png.latex?f_p%28x%29 "f_p(x)") and see
when the result is close to
![y\_x](https://latex.codecogs.com/png.latex?y_x "y_x") (the expected output of
our model when given ![x](https://latex.codecogs.com/png.latex?x "x")).

In fact, we can turn this into an optimization problem by trying to pick
![p](https://latex.codecogs.com/png.latex?p "p") that minimizes the difference
between ![f\_x(p)](https://latex.codecogs.com/png.latex?f_x%28p%29 "f_x(p)") and
![y\_x](https://latex.codecogs.com/png.latex?y_x "y_x"). We can say that our
model with parameter ![p](https://latex.codecogs.com/png.latex?p "p") predicts
![y\_x](https://latex.codecogs.com/png.latex?y_x "y_x") the best when we
minimize:

![
(f\_x(p) - y\_x)\^2
](https://latex.codecogs.com/png.latex?%0A%28f_x%28p%29%20-%20y_x%29%5E2%0A "
(f_x(p) - y_x)^2
")

If we minimize the squared error between the result of picking the parameter and
the expected result, we find the best parameters for that given input!

In general, picking the best parameter for the model involves picking the
![p](https://latex.codecogs.com/png.latex?p "p") that minimizes the relationship

![
\\text{loss}(y\_x, f\_x(p))
](https://latex.codecogs.com/png.latex?%0A%5Ctext%7Bloss%7D%28y_x%2C%20f_x%28p%29%29%0A "
\text{loss}(y_x, f_x(p))
")

Where
![\\text{loss} : B \\times B \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?%5Ctext%7Bloss%7D%20%3A%20B%20%5Ctimes%20B%20%5Crightarrow%20%5Cmathbb%7BR%7D "\text{loss} : B \times B \rightarrow \mathbb{R}")
gives a measure of "how badly" the model result differs from the expected
target. Common loss functions include squared error, cross-entropy, etc.

This gives us a supervised way to train any model: if we have enough
observations
(![(x, y\_x)](https://latex.codecogs.com/png.latex?%28x%2C%20y_x%29 "(x, y_x)")
pairs) we can just pick a ![p](https://latex.codecogs.com/png.latex?p "p") that
does its best to make the loss between all observations as small as possible.

### Stochastic Gradient Descent

If our model is a *differentiable function*, then we have a nice tool we can
use: *stochastic gradient descent* (SGD).

That is, we can always calculate the *gradient* of the loss function with
respect to our parameters. This gives us the direction we can "nudge" our
parameters to make the loss bigger or smaller.

That is, if we get the gradient of the loss with respect to
![p](https://latex.codecogs.com/png.latex?p "p"):

![
\\nabla\_p \\text{loss}(f\_x(p), y\_x)
](https://latex.codecogs.com/png.latex?%0A%5Cnabla_p%20%5Ctext%7Bloss%7D%28f_x%28p%29%2C%20y_x%29%0A "
\nabla_p \text{loss}(f_x(p), y_x)
")

We now have a nice way to "train" our model:

1.  Start with an initial guess at the parameter
2.  Look at a random
    ![(x, y\_x)](https://latex.codecogs.com/png.latex?%28x%2C%20y_x%29 "(x, y_x)")
    observation pair.
3.  Compute the gradient
    ![\\nabla\_p \\text{loss}(f\_x(p), y\_x)](https://latex.codecogs.com/png.latex?%5Cnabla_p%20%5Ctext%7Bloss%7D%28f_x%28p%29%2C%20y_x%29 "\nabla_p \text{loss}(f_x(p), y_x)")
    of our current ![p](https://latex.codecogs.com/png.latex?p "p"), which tells
    us a direction we can "nudge"
    ![p](https://latex.codecogs.com/png.latex?p "p") in to make the loss
    smaller.
4.  Nudge ![p](https://latex.codecogs.com/png.latex?p "p") in that direction
5.  Repeat from \#2 until satisfied

With every new observation, we see how we can nudge the parameter to make the
model more accurate, and then we perform that nudge.

Functional Implementation
-------------------------

This naturally lends itself well to a functional implementation. That's because,
in this light, a model is nothing more than a function. And a model that is
trainable using SGD is simply a differentiable function.

Using the *[backprop](http://hackage.haskell.org/package/backprop)* library, we
can easily write functions to be differentiable.

Let's write the type of our models. A model from type `a` to type `b` with
parameter `p` can be written as

``` {.haskell}
type Model p a b = p -> a -> b
```

Not normally differentiable, but we can make it a differentiable function by
having it work with `BVar z p` and `BVar z a` (`BVar`s containing those values)
instead:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L53-L54

type Model p a b = forall z. Reifies z W
                => BVar z p -> BVar z a -> BVar z b
```

We can write a simple linear regression model:

![
f\_{\\alpha, \\beta}(x) = \\beta x + \\alpha
](https://latex.codecogs.com/png.latex?%0Af_%7B%5Calpha%2C%20%5Cbeta%7D%28x%29%20%3D%20%5Cbeta%20x%20%2B%20%5Calpha%0A "
f_{\alpha, \beta}(x) = \beta x + \alpha
")

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L49-L60

data a :& b = !a :& !b
  deriving (Show, Generic)
infixr 2 :&

linReg :: Model (Double :& Double) Double Double
linReg ab x = b * x + a
  where
    a = ab ^^. t1
    b = ab ^^. t2
```

(First we define a custom tuple data type; backprop works with normal tuples,
but using a custom tuple with a `Num` instance will come in handy later for
training models)

Here `Double :& Double` is a tuple of two `Double`s, which contains the
parameters (`a` and `b`). We extract the first item using `^^. t1` and the
second item with `^^. t2`, and then talk about the actual function, whose result
is `b * x + a`. Note that, because `BVar`s have a `Num` instance, we can use all
our normal numeric operators, and the results are still differentiable.

We can *run* `linReg` using `evalBP2`:

``` {.haskell}
ghci> evalBP2 linReg (0.3 :& (-0.1)) 5
-0.2        -- (-0.1) * 5 + 0.3
```

But the neat thing is that we can also get the gradient of the parameters, too,
if we identify a loss function:

![
\\nabla\_p (f(p, x) - y\_x)\^2
](https://latex.codecogs.com/png.latex?%0A%5Cnabla_p%20%28f%28p%2C%20x%29%20-%20y_x%29%5E2%0A "
\nabla_p (f(p, x) - y_x)^2
")

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L62-L70

squaredErrorGrad
    :: (Backprop p, Backprop b, Num b)
    => Model p a b      -- ^ Model
    -> a                -- ^ Observed input
    -> b                -- ^ Observed output
    -> p                -- ^ Parameter guess
    -> p                -- ^ Gradient
squaredErrorGrad f x targ = gradBP $ \p ->
    (f p (constVar x) - constVar targ) ^ 2
```

We use `constVar :: a -> BVar z a`, to lift a normal value to a `BVar` holding
that value, since our model `f` takes `BVar`s.

And finally, we can train it using stochastic gradient descent, with just a
simple fold over all observations:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L72-L78

trainModel
    :: (Fractional p, Backprop p, Num b, Backprop b)
    => Model p a b      -- ^ model to train
    -> p                -- ^ initial parameter guess
    -> [(a,b)]          -- ^ list of observations
    -> p                -- ^ updated parameter guess
trainModel f = foldl' $ \p (x,y) -> p - 0.1 * squaredErrorGrad f x y p
```

For convenience, we can define a `Random` instance for our tuple type using the
*[random](http://hackage.haskell.org/package/random)* library and make a wrapper
that uses `IO` to generate a random initial parameter:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L80-L87

trainModelIO
    :: (Fractional p, Backprop p, Num b, Backprop b, Random p)
    => Model p a b      -- ^ model to train
    -> [(a,b)]          -- ^ list of observations
    -> IO p             -- ^ updated parameter guess
trainModelIO m xs = do
    p0 <- (/ 10) . subtract 0.5 <$> randomIO
    return $ trainModel m p0 xs
```

Let's train our linear regression model to fit the points `(1,1)`, `(2,3)`,
`(3,5)`, `(4,7)`, and `(5,9)`! This should follow
![f(x) = 2 x - 1](https://latex.codecogs.com/png.latex?f%28x%29%20%3D%202%20x%20-%201 "f(x) = 2 x - 1"),
or
![\\alpha = -1,\\, \\beta = 2](https://latex.codecogs.com/png.latex?%5Calpha%20%3D%20-1%2C%5C%2C%20%5Cbeta%20%3D%202 "\alpha = -1,\, \beta = 2"):

``` {.haskell}
ghci> samps = [(1,1),(2,3),(3,5),(4,7),(5,9)]
ghci> trainModelIO linReg $ take 5000 (cycle samps)
(-1.0000000000000024) :& 2.0000000000000036
-- roughly:
(-1.0) :& 2.0
```

Neat! After going through all of those observations a thousand times, the model
nudges itself all the way to the right parameters to fit our model!

The important takeaway is that all we specified was the *function* of the model
itself. The training part all follows automatically.

### Feed-forward Neural Network

Here's another example: a feed-forward neural network.

We can start with a single layer. The model here will also take two parameters
(a weight matrix and a bias vector), take in a vector, and output a vector.

``` {.haskell}
import Numeric.LinearAlgebra.Static.Backprop
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L94-L111

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

feedForwardLog
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardLog wb x = logistic (w #> x + b)
  where
    w = wb ^^. t1
    b = wb ^^. t2
```

Here we use the `L n m` (an n-by-m matrix) and `R n` (an n-vector) types from
the *hmatrix* library, and `#>` for backprop-aware matrix-vector multiplication.

Let's try training a model to learn the simple [logical
"AND"](https://en.wikipedia.org/wiki/Logical_conjunction):

``` {.haskell}
ghci> import qualified Numeric.LinearAlgebra.Static as H
ghci> samps = [(H.vec2 0 0, 0), (H.vec2 1 0, 0), (H.vec2 0 1, 0), (H.vec2 1 1, 1)]
ghci> trained <- trainModelIO feedForwardLog $ take 10000 (cycle samps)
```

We have our trained parameters! Let's see if they actually model "AND"?

``` {.haskell}
ghci> evalBP2 feedForwardLog trained (H.vec2 0 0)
(7.468471910660985e-5 :: R 1)       -- 0.0
ghci> evalBP2 feedForwardLog trained (H.vec2 1 0)
(3.816205998697482e-2 :: R 1)       -- 0.0
ghci> evalBP2 feedForwardLog trained (H.vec2 0 1)
(3.817490115313559e-2 :: R 1)       -- 0.0
ghci> evalBP2 feedForwardLog trained (H.vec2 1 1)
(0.9547178031665701 :: R 1)         -- 1.0
```

Close enough for me!

### Functional composition

Because our functions are simply just *normal functions*, we can create new,
complex models from simpler ones using just functional composition.

For example, we can map the result of a model to create a new model. Here, we
compose `linReg ab` (linear regression with parameter `ab`) with the logistic
function to create a *[logistic
regression](https://en.wikipedia.org/wiki/Logistic_regression)* model.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L124-L125

logReg :: Model (Double :& Double) Double Double
logReg ab = logistic . linReg ab
```

We could have even written our `feedForwardLog` without its activation function:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L97-L103

feedForward
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForward wb x = w #> x + b
  where
    w = wb ^^. t1
    b = wb ^^. t2
```

And now we can swap out activation functions using simple function composition:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L127-L130

feedForwardLog'
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardLog' wb = logistic . feedForward wb
```

Maybe even a [softmax](https://en.wikipedia.org/wiki/Softmax_function)
classifier!

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L132-L140

softMax :: (Reifies z W, KnownNat n) => BVar z (R n) -> BVar z (R n)
softMax x = konst (1 / sumElements expx) * expx
  where
    expx = exp x

feedForwardSoftMax
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardSoftMax wb = logistic . feedForward wb
```

We can even write a function to *compose* two models, keeping their two original
parameters separate:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L142-L151

(<~)
    :: (Backprop p, Backprop q)
    => Model  p       b c
    -> Model       q  a b
    -> Model (p :& q) a c
(f <~ g) pq = f p . g q
  where
    p = pq ^^. t1
    q = pq ^^. t2
infixr 8 <~
```

And now we have a way to chain models! Maybe even make a multiple-layer neural
network? Let's see if we can get a two-layer model to learn
[XOR](https://en.wikipedia.org/wiki/Exclusive_or)!

Our model is two feed-forward layers with logistic activation functions, with 4
hidden layer units:

``` {.haskell}
ghci> twoLayer = feedForwardLog' @4 @1 <~ feedForwardLog' @2 @4
```

Note we use type application syntax to specify the input/output dimensions of
`feedForwardLog'`.

We can train it on sample points:

``` {.haskell}
ghci> samps = [(H.vec2 0 0, 0), (H.vec2 1 0, 1), (H.vec2 0 1, 1), (H.vec2 1 1, 1)]
ghci> trained <- trainModelIO twoLayer $ take 10000 (cycle samps)
```

Trained. Now, does it model "XOR"?

``` {.haskell}
ghci> evalBP2 twoLayer trained (H.vec2 0 0)
(3.0812844350410647e-2 :: R 1)          -- 0.0
ghci> evalBP2 twoLayer trained (H.vec2 1 0)
(0.959153369985914 :: R 1)              -- 1.0
ghci> evalBP2 twoLayer trained (H.vec2 0 1)
(0.9834757090696419 :: R 1)             -- 1.0
ghci> evalBP2 twoLayer trained (H.vec2 1 1)
(3.6846467867668035e-2 :: R 1)          -- 0.0
```

Not bad!

### Possibilities

We just built a working neural network using normal function composition and
simple combinators. No need for any objects or mutability or fancy explicit
graphs. Just pure, typed functions! Why would you ever bring anything imperative
into this?

You can build a lot with just these tools alone. By using primitive models and
the various combinators, you can create autoencoders, nonlinear regressions,
convolutional neural networks, multi-layered neural networks...you can create
complex "graphs" of networks that fork and re-combine with themselves.

The nice thing is that these are all just regular (Rank-2) functions, so...you
have two models? Just compose their functions like normal functions!

Time Series Models
------------------

Not all models are "question and answer" models, however -- some models
represent a time series. This is usually notated as:

As a generalization, we can talk about models that are intended to represent
time series:

![
f\_p(x,t) = y
](https://latex.codecogs.com/png.latex?%0Af_p%28x%2Ct%29%20%3D%20y%0A "
f_p(x,t) = y
")

Which says, given an input and a time, return an output based on both. The point
of this is to let us have recurrent relationships, like for autoregressive
models:

![
\\text{AR}\_{\\phi\_1, \\phi\_2, \\ldots}(x,t)
  = \\epsilon\_t + \\phi\_1 \\text{AR}\_{\\phi\_1, \\phi\_2, \\ldots}(x, t-1)
  + \\phi\_2 \\text{AR}\_{\\phi\_1, \\phi\_2, \\ldots}(x, t-2)
  + \\ldots
](https://latex.codecogs.com/png.latex?%0A%5Ctext%7BAR%7D_%7B%5Cphi_1%2C%20%5Cphi_2%2C%20%5Cldots%7D%28x%2Ct%29%0A%20%20%3D%20%5Cepsilon_t%20%2B%20%5Cphi_1%20%5Ctext%7BAR%7D_%7B%5Cphi_1%2C%20%5Cphi_2%2C%20%5Cldots%7D%28x%2C%20t-1%29%0A%20%20%2B%20%5Cphi_2%20%5Ctext%7BAR%7D_%7B%5Cphi_1%2C%20%5Cphi_2%2C%20%5Cldots%7D%28x%2C%20t-2%29%0A%20%20%2B%20%5Cldots%0A "
\text{AR}_{\phi_1, \phi_2, \ldots}(x,t)
  = \epsilon_t + \phi_1 \text{AR}_{\phi_1, \phi_2, \ldots}(x, t-1)
  + \phi_2 \text{AR}_{\phi_1, \phi_2, \ldots}(x, t-2)
  + \ldots
")

However, this is a bad way to look at models on time serieses, because nothing
is stopping the result of a model from depending on a future value (the value at
time ![t = 3](https://latex.codecogs.com/png.latex?t%20%3D%203 "t = 3"), for
instance, might depend explicitly only the value at time
![t = 5](https://latex.codecogs.com/png.latex?t%20%3D%205 "t = 5")). Instead, we
can imagine time series models as explicitly "stateful" models:

![
f\_p(x, s\_{\\text{old}}) = (y, s\_{\\text{new}})
](https://latex.codecogs.com/png.latex?%0Af_p%28x%2C%20s_%7B%5Ctext%7Bold%7D%7D%29%20%3D%20%28y%2C%20s_%7B%5Ctext%7Bnew%7D%7D%29%0A "
f_p(x, s_{\text{old}}) = (y, s_{\text{new}})
")

These have type:[^2]

![
f : P \\times A \\times S \\rightarrow B \\times S
](https://latex.codecogs.com/png.latex?%0Af%20%3A%20P%20%5Ctimes%20A%20%5Ctimes%20S%20%5Crightarrow%20B%20%5Ctimes%20S%0A "
f : P \times A \times S \rightarrow B \times S
")

This makes it clear that the output of our model can only depend on current and
*previously occurring* information, preserving causality.

### Examples

We can use this to represent an AR(2) model ([autoregressive model with degree
2](https://en.wikipedia.org/wiki/Autoregressive_model)), which is a model whose
output forecast is a linear regression on the *last two* most recent observed
values. We can do this by setting the "input" to be the last observed value, and
the "state" to be the second-to-last observed value:

![
\\begin{aligned}
s\_t & = x\_t \\\\
y\_t & = c + \\phi\_1 x\_t + \\phi\_2 s\_{t - 1}
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0As_t%20%26%20%3D%20x_t%20%5C%5C%0Ay_t%20%26%20%3D%20c%20%2B%20%5Cphi_1%20x_t%20%2B%20%5Cphi_2%20s_%7Bt%20-%201%7D%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
s_t & = x_t \\
y_t & = c + \phi_1 x_t + \phi_2 s_{t - 1}
\end{aligned}
")

Or, in our explicit state form:

![
f\_{c, \\phi\_1, phi\_2}(x, s) = (c + \\phi\_1 x + \\phi\_2 s, x) 
](https://latex.codecogs.com/png.latex?%0Af_%7Bc%2C%20%5Cphi_1%2C%20phi_2%7D%28x%2C%20s%29%20%3D%20%28c%20%2B%20%5Cphi_1%20x%20%2B%20%5Cphi_2%20s%2C%20x%29%20%0A "
f_{c, \phi_1, phi_2}(x, s) = (c + \phi_1 x + \phi_2 s, x) 
")

There's also the classic [fully-connected recurrent neural network
layer](http://karpathy.github.io/2015/05/21/rnn-effectiveness/), whose output is
a linear combination of the (logistic'd) previous output and the current input,
plus a bias:

![
\\begin{aligned}
s\_t & = W\_x \\mathbf{x}\_t + W\_s \\mathbf{s}\_{t-1} + \\mathbf{b} \\\\
y\_t & = \\sigma(s\_t)
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0As_t%20%26%20%3D%20W_x%20%5Cmathbf%7Bx%7D_t%20%2B%20W_s%20%5Cmathbf%7Bs%7D_%7Bt-1%7D%20%2B%20%5Cmathbf%7Bb%7D%20%5C%5C%0Ay_t%20%26%20%3D%20%5Csigma%28s_t%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
s_t & = W_x \mathbf{x}_t + W_s \mathbf{s}_{t-1} + \mathbf{b} \\
y_t & = \sigma(s_t)
\end{aligned}
")

Or, in our explicit state form:

![
f\_{W\_x, W\_s, \\mathbf{b}}(\\mathbf{x}, \\mathbf{s}) =
  ( W\_x \\mathbf{x} + W\_s \\mathbf{s} + \\mathbf{b}
  , \\sigma(W\_x \\mathbf{x} + W\_s \\mathbf{s} + \\mathbf{b})
  )
](https://latex.codecogs.com/png.latex?%0Af_%7BW_x%2C%20W_s%2C%20%5Cmathbf%7Bb%7D%7D%28%5Cmathbf%7Bx%7D%2C%20%5Cmathbf%7Bs%7D%29%20%3D%0A%20%20%28%20W_x%20%5Cmathbf%7Bx%7D%20%2B%20W_s%20%5Cmathbf%7Bs%7D%20%2B%20%5Cmathbf%7Bb%7D%0A%20%20%2C%20%5Csigma%28W_x%20%5Cmathbf%7Bx%7D%20%2B%20W_s%20%5Cmathbf%7Bs%7D%20%2B%20%5Cmathbf%7Bb%7D%29%0A%20%20%29%0A "
f_{W_x, W_s, \mathbf{b}}(\mathbf{x}, \mathbf{s}) =
  ( W_x \mathbf{x} + W_s \mathbf{s} + \mathbf{b}
  , \sigma(W_x \mathbf{x} + W_s \mathbf{s} + \mathbf{b})
  )
")

### The connection

This is nice and all, but these stateful models seem to be at odds with our
previous picture of models.

1.  They aren't stated in the same way. They require specifying a state of some
    sort, and also a modified state
2.  These can't be *trained* in the same way (using stochastic gradient
    descent), and look like they require a different algorithm for training.

However, because these are all *just functions*, we can really just manipulate
them as normal functions and see that the two aren't too different at all.

Functional Stateful Models
--------------------------

Alright, so what does this mean, and how does it help us?

To help us see, let's try implementing this in Haskell:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L166-L170

type ModelS p s a b = forall z. Reifies z W
                   => BVar z p
                   -> BVar z a
                   -> BVar z s
                   -> (BVar z b, BVar z s)
```

We can implement AR(2) as mentioned before by translating the math formula
directly:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L172-L178

ar2 :: ModelS (Double :& (Double :& Double)) Double Double Double
ar2 cφ yLast yLastLast = ( c + φ1 * yLast + φ2 * yLastLast, yLast )
  where
    c  = cφ ^^. t1
    φ  = cφ ^^. t2
    φ1 = φ  ^^. t1
    φ2 = φ  ^^. t2
```

Our implementation of a fully-connected recurrent neural network is a similar
direct translation:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L180-L189

fcrnn
    :: (KnownNat i, KnownNat o)
    => ModelS ((L o i :& L o o) :& R o) (R o) (R i) (R o)
fcrnn wb x s = ( y, logistic y )
  where
    y  = (wX #> x) + (wS #> s) + b
    w  = wb ^^. t1
    b  = wb ^^. t2
    wX = w  ^^. t1
    wS = w  ^^. t2
```

Because we again have normal functions, we can write a similar stateful model
composition function that combines both their parameters and their states:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L191-L204

(<*~*)
  :: (Backprop p, Backprop q, Backprop s, Backprop t)
    => ModelS  p        s       b c
    -> ModelS       q        t  a b
    -> ModelS (p :& q) (s :& t) a c
(f <*~* g) pq x st = let (y, t') = g q x t
                         (z, s') = f p y s
                     in  (z, reTup s' t')
  where
    p = pq ^^. t1
    q = pq ^^. t2
    s = st ^^. t1
    t = st ^^. t2
infixr 8 <*~*
```

(`reTup` will take two `BVar`s of values and tuple them back up into a `BVar` of
a tuple, essentially the inverse of `^^. t1` and `^^. t2`)

And maybe even a utility function to map a function on the result of a `ModelS`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L206-L210

mapS
    :: (forall s. Reifies s W => BVar s b -> BVar s c)
    -> ModelS p s a b
    -> ModelS p s a c
mapS f g p x = first f . g p x
```

With this we can do some neat things like define a two-layer fully-connected
recurrent neural network.

``` {.haskell}
ghci> twoLayerRNN = fcrnn @10 @5 <*~* mapS logistic (fcrnn @20 @10)
```

Hey, maybe even a three-layer one:

``` {.haskell}
ghci> threeLayers = fcrnn @10 @5
               <*~* mapS logistic (fcrnn @20 @10)
               <*~* mapS logistic (fcrnn @40 @20)
```

### Let there be State

Because these are all just normal functions, we can manipulate them just like
any other function using higher order functions.

For example, we can "upgrade" any non-stateful function to a stateful one, just
by returning a new normal function:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L212-L214

toS :: Model  p   a b
    -> ModelS p s a b
toS f p x s = (f p x, s)
```

This means we can make a hybrid "recurrent" and "non-recurrent" neural network:

``` {.haskell}
ghci> hybrid = toS @_ @NoState (feedForwardLog' @20 @10)
          <*~* mapS logistic (fcrnn @20 @10)
          <*~* mapS logistic (fcrnn @40 @20)
```

We made a dummy type `NoState` to use for our stateless model

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L402-L403

data NoState = NoState
  deriving (Show, Generic)
```

But we can also be creative with our combinators, as well, and write one to
compose a stateless model with a stateful one:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L216-L225

(<*~)
  :: (Backprop p, Backprop q)
    => Model   p         b c
    -> ModelS       q  s a b
    -> ModelS (p :& q) s a c
(f <*~ g) pq x = first (f p) . g q x
  where
    p = pq ^^. t1
    q = pq ^^. t2
infixr 8 <*~
```

``` {.haskell}
ghci> hybrid = feedForwardLog' @20 @10
          <*~  mapS logistic (fcrnn @20 @10)
          <*~* mapS logistic (fcrnn @40 @20)
```

Everything is just your simple run-of-the-mill function composition and higher
order functions that Haskellers use every day, so there are many ways to do
these things --- just like there are many ways to manipulate normal functions.

### Unrolling in the Deep (Learning)

There's something neat we can do with stateful functions --- we can
"[unroll](https://machinelearningmastery.com/rnn-unrolling/)" them by explicitly
propagating their state through several inputs.

This is illustrated very well by [Christopher
Olah](http://colah.github.io/posts/2015-09-NN-Types-FP/), who made a nice
diagram:

![Christopher Olah's RNN Unrolling
Diagram](/img/entries/functional-models/RNN-general.png "Unrolled RNN")

If we look at each one of those individual boxes, they all have two inputs
(normal input, and previous state) and two outputs (normal output, new state).

"Unrolling" a stateful model means taking a model that takes in an `X` and
producing a `Y` and turning it into a model that takes an `[X]` and produces a
`[Y]`, by feeding it each of the `X`s one after the other, propagating the
state, and collecting all of the `Y` responses.

The "type" of this sounds like:

``` {.haskell}
unroll :: Model p s a b -> Model p s [a] [b]
```

In writing this out as a type, we also note that the `p` parameter is the same,
and the `s` state type is the same. If you're familiar with category theory,
this looks a little bit like a sort of "fmap" under a `Model p s` category -- it
takes a (stateful and backpropagatable) `a -> b` and turns it into an
`[a] -> [b]`.

Olah's post suggests that this is a `mapAccum`, in functional programming
parlance. And, surely enough, we can actually write this as a `mapAccumL`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L227-L235

unroll
    :: (Traversable t, Backprop a, Backprop b, Backprop (t b))
    => ModelS p s    a     b
    -> ModelS p s (t a) (t b)
unroll f p xs s0 = swap $ mapAccumL f' s0 xs
  where
    -- we have to re-arrange the order of arguments and tuple a bit to
    -- match what `mapAccumL` expects
    f' s x = swap (f p x s)
```

This is *exactly* the just the normal functional programming `mapAccumL` of a
stateful function over a container. And, `mapAccumL` is general enough to be
definable for all `Traversable` containers (not just lists)! (We use `mapAccumL`
"lifted" for `BVar`s from the
*[Prelude.Backprop](http://hackage.haskell.org/package/backprop/docs/Prelude-Backprop.html)*
module)

We can also tweak `unroll`'s result a bit to get a version of `unroll` that
shows only the "final" result:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L237-L242

unrollLast
    :: (Backprop a, Backprop b)
    => ModelS p s  a  b
    -> ModelS p s [a] b
unrollLast f = mapS (last . sequenceVar) (unroll f)
-- TODO: switch to (last . toList)
```

To see how this applies to our `threeLayer`:

``` {.haskell}
threeLayers            :: ModelS _ _ (R 40) (R 5)
unroll threeLayers     :: ModelS _ _ [R 40] [R 5]
unrollLast threeLayers :: ModelS _ _ [R 40] (R 5)
```

### State-be-gone

Did you enjoy the detour through stateful time series models?

Good! Because the whole point of it was to talk about how we can *get rid of
state* and bring us back to our original models!

You knew this had to come, because all of our methods for "training" these
models and learn these parameters involves non-stateful models. Let's see now
how we can turn our functional stateful models into functional non-stateful
models!

One way is to *fix the initial state and throw away the resulting one*. This is
very common in machine learning contexts, where many people simply fix the
initial state to be a zero vector.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L244-L254

fixState
    :: s
    -> ModelS p s a b
    -> Model  p   a b
fixState s0 f p x = fst $ f p x (constVar s0)

zeroState
    :: Num s
    => ModelS p s a b
    -> Model  p   a b
zeroState = fixState 0
```

We use `constVar :: a -> BVar s a` again to introduce a `BVar` of our initial
state, but to indicate that we don't expect to track its gradient. `zeroState`
is a nice utility combinator for a common pattern.

Another way is to *treat the initial state as a trainable parameter* (and also
throw away the final state). This is not done as often, but is still common
enough to be mentioned often. And, it's just as straightforward!

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L256-L263

trainState
    :: (Backprop p, Backprop s)
    => ModelS  p    s  a b
    -> Model  (p :& s) a b
trainState f ps x = fst $ f p x s
  where
    p = ps ^^. t1
    s = ps ^^. t2
```

`trainState` will take a model with trainable parameter `p` and state `s`, and
turn it into a model with trainable parameter `p :& s`, where the `s` is the
(trainable) initial state.

We can now *train* our recurrent/stateful models, by **unrolling and
de-stating**:

``` {.haskell}
threeLayers                        :: ModelS _ _ (R 40) (R 5)
unrollLast threeLayers             :: ModelS _ _ [R 40] (R 5)
zeroState (unrollLast threeLayers) :: Model  _   [R 40] (R 5)
```

`zeroState (unrollLast threeLayers)` is now a normal stateless (and trainable)
model. It takes a list of inputs `R 40`s and produces the "final output" `R 5`.
We can now train this by feeding it with `([R 40], R 5)` pairs: give a history
and an expected next output.

Let's see this play out with our AR(2) model:

``` {.haskell}
ar2                        :: ModelS _ _  Double  Double
unrollLast ar2             :: ModelS _ _ [Double] Double
zeroState (unrollLast ar2) :: Model  _   [Double] Double
```

`zeroState (unrollLast ar2)` is now a trainable stateless model. Let's see if we
can use it to learn how to model a sine wave:

``` {.haskell}
-- sine signal with period 25
ghci> series = [ sin (2 * pi * t / 25) | t <- [0..]              ]
-- chunks of runs and "next results"
ghci> samps  = [ (init c, last c)      | c <- chunksOf 19 series ]
ghci> trained <- trainModelIO (zeroState (unrollLast ar2)) $ take 10000 samps
```

Trained! `trained` is the parameterization of `ar2` that will simulate a sine
wave of period 25.

Let's define some helper functions to test our model. First, a function `prime`
that takes a stateful model and gives a "warmed-up" state by running it over a
list of inputs. This serves to essentially initialize the memory of the model.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L265-L272

prime
    :: Foldable t
    => ModelS p s a b     -- ^ model
    -> p                  -- ^ parameterization
    -> s                  -- ^ initial state
    -> t a                -- ^ priming input
    -> s                  -- ^ primed state
prime f p = foldl' $ evalBP2 (\s x -> snd $ f (constVar p) x s)
```

Then a function `feedback` that iterates a stateful model over and over again by
feeding its previous output as its next input:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L274-L285

feedback
    :: (Backprop a, Backprop s)
    => ModelS p s a a     -- ^ model
    -> p                  -- ^ parameterization
    -> s                  -- ^ initial state
    -> a                  -- ^ initial input
    -> [a]                -- ^ inifinite feedback loop
feedback f p s0 x0 = unfoldr go (x0, s0)
  where
    go (x, s) = Just (x, (y, s'))
      where
        (y, s') = evalBP (uncurry T2 . f (constVar p) (constVar x)) s
```

Now let's prime our trained model over the first 19 items in our sine wave and
start it running in feedback mode on the 20th item!

``` {.haskell}
ghci> let primed = prime    ar2 trained 0      (take 19 series)
ghci> let output = feedback ar2 trained primed (series !! 19)
ghci> mapM_ print $ take 30 output
-0.9980267284282716
-0.9510565162972417
-0.8443279255081759
-0.6845471059406962
-0.48175367412103653
-0.24868988719256901
-3.673766846290505e-11
0.24868988711894977
0.4817536740469978
0.6845471058659982
0.8443279254326351
0.9510565162207472
0.9980267283507953
0.9822872506502898
0.9048270523889208
0.7705132427021685
0.5877852522243431
0.3681245526237731
0.12533323351198067
-0.1253332336071494
-0.36812455271766376
-0.5877852523157643
-0.7705132427900961
-0.9048270524725681
-0.9822872507291605
-0.9980267284247174
-0.9510565162898851
-0.844327925497479
-0.6845471059273313
-0.48175367410584324
```

Looks like a beautiful sine wave! It starts out at -0.998, gradually rolls back
towards 0, cross over and peaks out at positive 0.998, then swings back around
past zero and reaches a minimum at -0.998 before swinging back again. Pretty
much a perfect sine wave with period 25. This is pretty much as good as it gets,
so it seems like AR(2) works pretty well!

For kicks, let's try it with a two-layer fully connected neural network with 30
hidden units, where the first layer is fully recurrent:

``` {.haskell}
-- first layer is RNN, second layer is normal ANN, 30 hidden units
ghci> let rnn :: ModelS _ _ (R 1) (R 1)
          rnn = feedForward @30 @1 <*~ mapS logistic (fcrnn @1 @30)
ghci> trained <- trainModelIO (trainZero (unrollLast rnn)) $ take 10000 samps
ghci> let primed = prime    rnn trained 0      (take 19 series)
ghci> let output = feedback rnn trained primed (series !! 19)
ghci> mapM_ print $ take 30 output
(-0.9980267284282716 :: R 1)
(-0.9530599469923343 :: R 1)
(-0.855333250123637 :: R 1)
(-0.7138776465246676 :: R 1)
(-0.5359655931506458 :: R 1)
(-0.3276007378757607 :: R 1)
(-9.49789925462907e-2 :: R 1)
(0.15326329240850092 :: R 1)
(0.4036006817890014 :: R 1)
(0.6365988256374424 :: R 1)
(0.8297644575358999 :: R 1)
(0.9644601077595601 :: R 1)
(1.0322337479560069 :: R 1)
(1.0354328415838387 :: R 1)
(0.98271304349553 :: R 1)
(0.8838820861246679 :: R 1)
(0.7469384572032421 :: R 1)
(0.5774599954294803 :: R 1)
(0.37953522246889254 :: R 1)
(0.1576543900562724 :: R 1)
(-8.079295377239147e-2 :: R 1)
(-0.32316184922616614 :: R 1)
(-0.5509792378000917 :: R 1)
(-0.7428726769386842 :: R 1)
(-0.8804772463971613 :: R 1)
(-0.9537270792131795 :: R 1)
(-0.9620288708442922 :: R 1)
(-0.9114012854243098 :: R 1)
(-0.8104104643872705 :: R 1)
(-0.6672968115106706 :: R 1)
```

Also nice, but not quite as perfect as AR(2). It seems to overshoot the positive
peak slightly (hitting 1.03) and undershoot the negative peak (only reaching
-0.96)...but it still seems pretty nice considering that its memory units are
all sigmoidally squashed, while AR(2) gets to have a continuous memory space. At
this point we're picking hairs of 1% difference, though! Sounds like these RNNs
have proven to be quite "unreasonably effective", eh?

[^1]: Those familiar with Haskell idioms might recognize this type as being
    isomorphic to `a -> Reader p b` (or `Kleisli (Reader p) a b`) which roughly
    represents the notion of "A function from `a` to `b` with an 'environment'
    of type `p`".

[^2]: If you recognized our original stateless model type as `a -> Reader p b`,
    then you might have also recognized that this is the Haskell idiom
    `a -> StateT s (Reader p) b` (or `Kleisli (StateT s (Reader p)) a b`), which
    represents the notion of a "function from `a` to `b` with environment `p`,
    that takes and returns a modified version of some 'state' `s`".
