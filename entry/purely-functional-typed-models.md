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

For example, for linear regression, you are trying to "fit" your
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
that function, it has type:

![
f : P \\cross A \\rightarrow B
](https://latex.codecogs.com/png.latex?%0Af%20%3A%20P%20%5Ccross%20A%20%5Crightarrow%20B%0A "
f : P \cross A \rightarrow B
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
5.  Pick a new
    ![(x, y\_x)](https://latex.codecogs.com/png.latex?%28x%2C%20y_x%29 "(x, y_x)")
    observation pair.

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
having it work with `BVar s p` and `BVar s a` (`BVar`s containing those values)
instead:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L42-L43

type Model p a b = forall s. Reifies s W
                 => BVar s p -> BVar s a -> BVar s b
```

We can write a simple linear regression model:

![
f\_{\\alpha, \\beta}(x) = \\beta x + \\alpha
](https://latex.codecogs.com/png.latex?%0Af_%7B%5Calpha%2C%20%5Cbeta%7D%28x%29%20%3D%20%5Cbeta%20x%20%2B%20%5Calpha%0A "
f_{\alpha, \beta}(x) = \beta x + \alpha
")

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L45-L49

linReg :: Model (T2 Double Double) Double Double
linReg ab x = b * x + a
  where
    a = ab ^^. _1
    b = ab ^^. _2
```

Here `T2 Double Double` is a tuple of two `Double`s, which contains the
parameters (`a` and `b`). We extract the first item using `^^. _1` and the
second item with `^^. _2`, and then talk about the function

We can *run* `linReg` using `evalBP2`:

``` {.haskell}
ghci> evalBP2 linReg (T2 0.3 (-0.1)) 5
-0.2        -- (-0.1) * 5 + 0.3
```

But the neat thing is that we can also get the gradient of the parameters, too,
if we identify a loss function:

![
\\nabla\_p (f\_x(p) - y\_x)\^2
](https://latex.codecogs.com/png.latex?%0A%5Cnabla_p%20%28f_x%28p%29%20-%20y_x%29%5E2%0A "
\nabla_p (f_x(p) - y_x)^2
")

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L51-L59

squaredErrorGrad
    :: (Num p, Num b)
    => Model p a b      -- ^ Model
    -> a                -- ^ Observed input
    -> b                -- ^ Observed output
    -> p                -- ^ Parameter guess
    -> p                -- ^ Gradient
squaredErrorGrad f x targ = gradBP $ \p ->
    (f p (constVar x) - constVar targ) ^ 2
```

We use `constVar :: a -> BVar s a`, to lift a normal value to a `BVar` holding
that value, since our model `f` takes `BVar`s.

And finally, we can train it using stochastic gradient descent, with just a
simple fold over all observations:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L61-L67

trainModel
    :: (Fractional p, Num b)
    => Model p a b      -- ^ model to train
    -> p                -- ^ initial parameter guess
    -> [(a,b)]          -- ^ list of observations
    -> p                -- ^ updated parameter guess
trainModel f = foldl' $ \p (x,y) -> p - 0.1 * squaredErrorGrad f x y p
```

Let's train our linear regression model to fit the points `(1,1)`, `(2,3)`,
`(3,5)`, `(4,7)`, and `(5,9)`! This should follow
![f(x) = 2 x - 1](https://latex.codecogs.com/png.latex?f%28x%29%20%3D%202%20x%20-%201 "f(x) = 2 x - 1"),
or
![\\alpha = -1,\\, \\beta = 2](https://latex.codecogs.com/png.latex?%5Calpha%20%3D%20-1%2C%5C%2C%20%5Cbeta%20%3D%202 "\alpha = -1,\, \beta = 2"):

``` {.haskell}
ghci> samps = [(1,1),(2,3),(3,5),(4,7),(5,9)]
ghci> trainModel linReg (T2 0 0) (concat (replicate 1000 samps))
T2 (-1.0000000000000024) 2.0000000000000036
```

Neat! After going through all of those observations a thousand times, the model
nudges itself all the way to the right parameters to fit our model!

The important takeaway is that all we specified was the *function* of the model
itself. The training part all follows automatically!

### Feed-forward Neural Network

Here's another example: a feed-forward neural network.

We can start with a single layer. The model here will also take two parameters
(a weight matrix and a bias vector), take in a vector, and output a vector.

``` {.haskell}
import Numeric.LinearAlgebra.Static.Backprop

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L75-L84

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

feedForward
    :: (KnownNat i, KnownNat o)
    => Model (T2 (L o i) (R o)) (R i) (R o)
feedForward wb x = logistic (w #> x + b)
  where
    w = wb ^^. _1
    b = wb ^^. _2
```

Here we use the `L n m` (an n-by-m matrix) and `R n` (an n-vector) types from
the *hmatrix* library, and `#>` for backprop-aware matrix-vector multiplication.

Let's try training a model to learn the simple logical "AND":

``` {.haskell}
ghci> import qualified Numeric.LinearAlgebra.Static as H
ghci> samps = [(H.vec2 0 0, 0), (H.vec2 1 0, 0), (H.vec2 0 1, 0), (H.vec2 1 1, 1)]
ghci> trained = trainModel feedForward (T2 0 0) (concat (replicate 10000 samps))
```

We have our trained parameters! Let's see if they actually model "AND"?

``` {.haskell}
ghci> evalBP2 feedForward trained (H.vec2 0 0)
(7.468471910660985e-5 :: R 1)
ghci> evalBP2 feedForward trained (H.vec2 1 0)
(3.816205998697482e-2 :: R 1)
ghci> evalBP2 feedForward trained (H.vec2 0 1)
(3.817490115313559e-2 :: R 1)
ghci> evalBP2 feedForward trained (H.vec2 1 1)
(0.9547178031665701 :: R 1)
```

Close enough!
