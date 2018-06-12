Automatic Propagation of Uncertainty with AD
============================================

> Originally posted by [Justin Le](https://blog.jle.im/) on May 9, 2016.
> [Read online!](https://blog.jle.im/entry/automatic-propagation-of-uncertainty-with-ad.html)

> This post and [series](https://blog.jle.im/entries/series/+uncertain.html) is
> a walk-through of the implementation of my
> *[uncertain](https://hackage.haskell.org/package/uncertain)* library, now on
> hackage!

Some of my favorite Haskell "tricks" involve working with exotic numeric types
with custom "overloaded" numeric functions and literals that let us work with
data in surprisingly elegant and expressive ways.

Here is one example --- from my work in experimental physics and statistics, we
often deal with experimental/sampled values with inherent uncertainty. If you
ever measure something to be
![12.3\\,\\mathrm{cm}](https://latex.codecogs.com/png.latex?12.3%5C%2C%5Cmathrm%7Bcm%7D "12.3\,\mathrm{cm}"),
that doesn't mean it's
![12.300000\\,\\mathrm{cm}](https://latex.codecogs.com/png.latex?12.300000%5C%2C%5Cmathrm%7Bcm%7D "12.300000\,\mathrm{cm}")
--- it means that it's somewhere between
![12.2\\,\\mathrm{cm}](https://latex.codecogs.com/png.latex?12.2%5C%2C%5Cmathrm%7Bcm%7D "12.2\,\mathrm{cm}")
and
![12.4\\,\\mathrm{cm}](https://latex.codecogs.com/png.latex?12.4%5C%2C%5Cmathrm%7Bcm%7D "12.4\,\mathrm{cm}")...and
we don't know exactly. We can write it as
![12.3 \\pm 0.1\\,\\mathrm{cm}](https://latex.codecogs.com/png.latex?12.3%20%5Cpm%200.1%5C%2C%5Cmathrm%7Bcm%7D "12.3 \pm 0.1\,\mathrm{cm}").
The interesting thing happens when we try to add, multiply, divide numbers with
uncertainty. What happens when you "add"
![12 \\pm 3](https://latex.codecogs.com/png.latex?12%20%5Cpm%203 "12 \pm 3") and
![19 \\pm 6](https://latex.codecogs.com/png.latex?19%20%5Cpm%206 "19 \pm 6")?

The initial guess might be
![31 \\pm 9](https://latex.codecogs.com/png.latex?31%20%5Cpm%209 "31 \pm 9"),
because one is ![\\pm 3](https://latex.codecogs.com/png.latex?%5Cpm%203 "\pm 3")
and the other is
![\\pm 6](https://latex.codecogs.com/png.latex?%5Cpm%206 "\pm 6"). But! If you
actually do experiments like this several times, you'll see that this isn't the
case. If you tried this out experimentally and simulate several hundred trials,
you'll see that the answer is actually something like
![31 \\pm 7](https://latex.codecogs.com/png.latex?31%20%5Cpm%207 "31 \pm 7").
(We'll explain why later, but feel free to stop reading this article now and try
this out yourself![^1])

Let's write ourselves a Haskell data type that lets us work with "numbers with
inherent uncertainty":

``` {.haskell}
ghci> let x = 14.6 +/- 0.8
ghci> let y = 31   +/- 2
ghci> x + y
46 +/- 2
ghci> x * y
450 +/- 40
ghci> sqrt (x + y)
6.8 +/- 0.2
ghci> logBase y x
0.78 +/- 0.02
ghci> log (x**y)
85.9 +/- 0.3
```

Along the way, we'll also learn how to harness the power of awesome
[ad](https://hackage.haskell.org/package/ad) library, a library used in
implementing back-propagation and other optimization algorithms, to analyze
numerical functions in a mathematical way and break down their derivatives and
gradients.

You can follow along with [the source
code](https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs),
which is actually a *[stack](http://www.haskellstack.org)* executable! If you
download the source and you have *[stack](http://www.haskellstack.org)*
installed, you can run it (and run the tests above) as an executable:

``` {.bash}
$ ./Uncertain.hs
```

Otherwise, you can run it directly with stack (using `runhaskell`) and the
[linear](shttp://hackage.haskell.org/package/linear/docs/Linear-V2.html) and
[ad](https://hackage.haskell.org/package/ad) packages installed...or load it up
with `stack ghci` to play with it. If you want to be sure to reproduce the
behavior, this article was written under [stackage](https://www.stackage.org/)
snapshot [lts-5.15](https://www.stackage.org/lts-5.15).

Dealing with Uncertainty Precisely
----------------------------------

First of all, let's think about why adding two "uncertain" values doesn't
involve simply adding the uncertainties linearly. (If you don't care about the
math and just want to get on to the Haskell, feel free to skip this section!)

If I have a value
![16 \\pm 3](https://latex.codecogs.com/png.latex?16%20%5Cpm%203 "16 \pm 3")
(maybe I have a ruler whose ticks are 3 units apart, or an instrument that
produces measurements with 3 units of noise), it either means that it's a little
below 16 or a little above 16. If I have an independently sampled value
![25 \\pm 4](https://latex.codecogs.com/png.latex?25%20%5Cpm%204 "25 \pm 4"), it
means that it's a little below 25 or a little above 25.

What happens if I want to think about their sum? Well, it's going to be
somewhere around 41. But, the uncertainty won't be
![\\pm 7](https://latex.codecogs.com/png.latex?%5Cpm%207 "\pm 7"). It would only
be ![\\pm 7](https://latex.codecogs.com/png.latex?%5Cpm%207 "\pm 7") if the
errors in the two values are *always aligned*. Only if or when every "little bit
above" 16 error lines up perfectly with a "little bit above" 25 error, and when
every single "little bit below" 16 error lines up perfectly with a "little bit
above" 25 error, would you really get something that is
![\\pm 7](https://latex.codecogs.com/png.latex?%5Cpm%207 "\pm 7"). But, because
the two values are sampled independently, you shouldn't expect such alignment.
So, you'll get an uncertainty that's *less than*
![\\pm 7](https://latex.codecogs.com/png.latex?%5Cpm%207 "\pm 7"). In fact,
it'll actually be around
![\\pm 5](https://latex.codecogs.com/png.latex?%5Cpm%205 "\pm 5").

In general, we find that for *independent*
![X](https://latex.codecogs.com/png.latex?X "X") and
![Y](https://latex.codecogs.com/png.latex?Y "Y"):

![
\\operatorname{Var}\[aX + bY + c\] = a\^2 \\sigma\_X\^2 + b\^2 \\sigma\_Y\^2
](https://latex.codecogs.com/png.latex?%0A%5Coperatorname%7BVar%7D%5BaX%20%2B%20bY%20%2B%20c%5D%20%3D%20a%5E2%20%5Csigma_X%5E2%20%2B%20b%5E2%20%5Csigma_Y%5E2%0A "
\operatorname{Var}[aX + bY + c] = a^2 \sigma_X^2 + b^2 \sigma_Y^2
")

Where
![\\sigma\_X\^2](https://latex.codecogs.com/png.latex?%5Csigma_X%5E2 "\sigma_X^2")
is the variance in ![X](https://latex.codecogs.com/png.latex?X "X"). We consider
![\\sigma\_X](https://latex.codecogs.com/png.latex?%5Csigma_X "\sigma_X") to be
the standard deviation of ![X](https://latex.codecogs.com/png.latex?X "X"), or
the "plus or minus" part of our numbers. In the simple case of addition, we have
![\\operatorname{Var}\[X + Y\] = \\sigma\_X\^2 + \\sigma\_Y\^2](https://latex.codecogs.com/png.latex?%5Coperatorname%7BVar%7D%5BX%20%2B%20Y%5D%20%3D%20%5Csigma_X%5E2%20%2B%20%5Csigma_Y%5E2 "\operatorname{Var}[X + Y] = \sigma_X^2 + \sigma_Y^2"),
so our new uncertainty[^2] is
![\\sqrt{\\sigma\_X\^2 + \\sigma\_Y\^2}](https://latex.codecogs.com/png.latex?%5Csqrt%7B%5Csigma_X%5E2%20%2B%20%5Csigma_Y%5E2%7D "\sqrt{\sigma_X^2 + \sigma_Y^2}").

However, not all functions that combine
![X](https://latex.codecogs.com/png.latex?X "X") and
![Y](https://latex.codecogs.com/png.latex?Y "Y") can be expressed as simple
linear combinations
![aX + bY + c](https://latex.codecogs.com/png.latex?aX%20%2B%20bY%20%2B%20c "aX + bY + c").
But! If you dig back to your days of high school calculus, you might remember a
method for expressing any arbitrary function as a linear approximation -- the
[Taylor Expansion](https://en.wikipedia.org/wiki/Taylor_series)!

In general, we can attempt to approximate any well-behaving function around a
point as its tangent hyperplane:

![
f(x\_0 + x, y\_0 + y) \\approx f\_x(x\_0, y\_0) x + f\_y(x\_0, y\_0) y + f(x\_0, y\_0)
](https://latex.codecogs.com/png.latex?%0Af%28x_0%20%2B%20x%2C%20y_0%20%2B%20y%29%20%5Capprox%20f_x%28x_0%2C%20y_0%29%20x%20%2B%20f_y%28x_0%2C%20y_0%29%20y%20%2B%20f%28x_0%2C%20y_0%29%0A "
f(x_0 + x, y_0 + y) \approx f_x(x_0, y_0) x + f_y(x_0, y_0) y + f(x_0, y_0)
")

Where
![f\_x(x\_0,y\_0)](https://latex.codecogs.com/png.latex?f_x%28x_0%2Cy_0%29 "f_x(x_0,y_0)")
is the first (partial) derivative with respect to
![x](https://latex.codecogs.com/png.latex?x "x") at
![(x\_0, y\_0)](https://latex.codecogs.com/png.latex?%28x_0%2C%20y_0%29 "(x_0, y_0)").
This gives us an approximation of
![f](https://latex.codecogs.com/png.latex?f "f") at locations close to
![(x\_0, y\_0)](https://latex.codecogs.com/png.latex?%28x_0%2C%20y_0%29 "(x_0, y_0)").

Look familiar? This is exactly the form that we used earlier to calculate
"combined" variance! If we approximate the functions around
![(\\mu\_X, \\mu\_Y)](https://latex.codecogs.com/png.latex?%28%5Cmu_X%2C%20%5Cmu_Y%29 "(\mu_X, \mu_Y)"),
the center/expected value of ![X](https://latex.codecogs.com/png.latex?X "X")
and ![Y](https://latex.codecogs.com/png.latex?Y "Y"), we see:

![
\\operatorname{Var}\[f(X,Y)\] \\approx f\_x(\\mu\_X, \\mu\_Y)\^2 \\sigma\_X\^2 + f\_y(\\mu\_X,\\mu\_Y)\^2 \\sigma\_Y\^2
](https://latex.codecogs.com/png.latex?%0A%5Coperatorname%7BVar%7D%5Bf%28X%2CY%29%5D%20%5Capprox%20f_x%28%5Cmu_X%2C%20%5Cmu_Y%29%5E2%20%5Csigma_X%5E2%20%2B%20f_y%28%5Cmu_X%2C%5Cmu_Y%29%5E2%20%5Csigma_Y%5E2%0A "
\operatorname{Var}[f(X,Y)] \approx f_x(\mu_X, \mu_Y)^2 \sigma_X^2 + f_y(\mu_X,\mu_Y)^2 \sigma_Y^2
")

A similar analysis can be used to figure out how the expected value changes by
taking the taylor expansion to the *second* degree:

![
\\operatorname{E}\[f(X,Y)\] \\approx
f(\\mu\_X, \\mu\_Y) + \\frac{1}{2}
\\left\[ f\_{xx}(\\mu\_X, \\mu\_Y) \\sigma\_X\^2 + f\_{yy}(\\mu\_X, \\mu\_Y) \\sigma\_Y\^2 \\right\]
](https://latex.codecogs.com/png.latex?%0A%5Coperatorname%7BE%7D%5Bf%28X%2CY%29%5D%20%5Capprox%0Af%28%5Cmu_X%2C%20%5Cmu_Y%29%20%2B%20%5Cfrac%7B1%7D%7B2%7D%0A%5Cleft%5B%20f_%7Bxx%7D%28%5Cmu_X%2C%20%5Cmu_Y%29%20%5Csigma_X%5E2%20%2B%20f_%7Byy%7D%28%5Cmu_X%2C%20%5Cmu_Y%29%20%5Csigma_Y%5E2%20%5Cright%5D%0A "
\operatorname{E}[f(X,Y)] \approx
f(\mu_X, \mu_Y) + \frac{1}{2}
\left[ f_{xx}(\mu_X, \mu_Y) \sigma_X^2 + f_{yy}(\mu_X, \mu_Y) \sigma_Y^2 \right]
")

Where
![f\_{xx}(\\mu\_X, \\mu\_Y)](https://latex.codecogs.com/png.latex?f_%7Bxx%7D%28%5Cmu_X%2C%20%5Cmu_Y%29 "f_{xx}(\mu_X, \mu_Y)")
is the second (partial) derivative with respect to
![x](https://latex.codecogs.com/png.latex?x "x") twice at
![(\\mu\_X, \\mu\_Y)](https://latex.codecogs.com/png.latex?%28%5Cmu_X%2C%20%5Cmu_Y%29 "(\mu_X, \mu_Y)")

For our case of simple addition,
![\\operatorname{E}\[X + Y\] = \\mu\_X + \\mu\_Y](https://latex.codecogs.com/png.latex?%5Coperatorname%7BE%7D%5BX%20%2B%20Y%5D%20%3D%20%5Cmu_X%20%2B%20%5Cmu_Y "\operatorname{E}[X + Y] = \mu_X + \mu_Y"),
because the second-order partials of
![f(x,y) = x + y](https://latex.codecogs.com/png.latex?f%28x%2Cy%29%20%3D%20x%20%2B%20y "f(x,y) = x + y")
are 0.

Uncertain Values in Haskell
---------------------------

So, how are we going to model our uncertain values in Haskell ... ? With an
Algebraic Data Type, of course!

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L18-L20

data Uncert a = Un { uMean :: !a
                   , uVar  :: !a
                   }
```

We'll keep track of the mean (the central point) and the *variance*, which is
the standard deviation *squared*. We keep track of the variance and not the
standard deviation (the "plus or minus") because the mathematics is a bit more
straightforward.

We can write a function to turn a "plus or minus" statement into an `Uncert`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L22-L23

(+/-) :: Num a => a -> a -> Uncert a
x +/- dx = Un x (dx*dx)
```

Give the `dx` (the standard deviation) and store `dx^2`, the variance.

Let's also throw in a handy helper function for "exact" values:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L25-L26

exact :: Num a => a -> Uncert a
exact x = x +/- 0
```

But, we can do better (if just for fun). We can use pattern synonyms to
basically "abstract" away the data type itself, and let people pattern match on
a mean and standard deviation:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L28-L31

pattern (:+/-) :: Floating a => a -> a -> Uncert a
pattern x :+/- dx <- Un x (sqrt->dx)
  where
    x :+/- dx = Un x (dx*dx)
```

(Note that GHC 8.0 introduces a new type signature format for pattern synonyms,
so this will only work in GHC 8.0 or above as-is)

Now, people can pattern match on `x :+/- dx` and receive the mean and
uncertainty directly. Neat!

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L33-L34

uStdev :: Floating a => Uncert a -> a
uStdev (_ :+/- dx) = dx
```

### Making it Numeric

Now, time for the magic! Let's write a `Num` instance!

``` {.haskell}
instance Num a => Num (Uncert a) where
    fromIntegral      = exact . fromIntegral
    Un x vx + Un y vy = Un (x + y)    (vx + vy)
    Un x vx - Un y vy = Un (x - y)    (vx + vy)
    Un x vx * Un y vy = Un (x * y)    (y^2 * vx + x^2 * vy)
    negate (Un x vx)  = Un (negate x) vx
    -- ...
```

And...that's it! Do the same thing for every numeric typeclass, and you get
automatic propagation of uncertainty.

### The Problem

But, wait --- this method is definitely not ideal. It's pretty repetitive, and
involves a but of copy-and-pasting code that is slightly different in ways the
typechecker can't verify. What if we didn't change something we were supposed
to? And, if you look at the `Fractional` instance...

``` {.haskell}
instance Fractional a => Fractional (Uncert a) where
    fromRational      = exact . fromRational
    Un x vx / Un y vy = Un (x/y + x/y^3*vy)   (x^2/y^4*vx + vy/y^2)
    recip (Un x vx)   = Un (recip x + vx/x^3) (vx / x^4)
```

Yikes. All that ugly and complicated numerical code that the typechecker can't
help us with (and, honestly, I'm not very confident in the results myself!).
Those are runtime bugs just waiting to happen. How do we even *know* that we
calculated the right derivatives, and implemented the formula correctly?

What if we could reduce this boilerplate? What if we could somehow analytically
compute derivatives for functions instead of computing them manually?

Automatic Differentiation
-------------------------

Automatic differentiation is honestly one of the coolest Haskell tricks you can
show that any beginner can immediately understand. Like our trick with `Uncert`,
it's nice to use because of its overloaded `Num`/numeric typeclasses.

``` {.haskell}
ghci> diff (\x -> x^2) 10       -- 2*x
20
ghci> diff (\x -> sin x) 0      -- cos x
1.0
ghci> diff (\x -> x^3 - 3*x^2 + 2*x - 4) 3  -- 3*x^2 - 6*x + 2
11
```

A very rough explanation about how forward-mode automatic differentiation works
is that it uses a wrapper type (like ours) that defines `*`, `negate`, etc. so
that they also compute the *derivative(s)* of the function, instead of just the
*result*, like normal. There are a lot of nice tutorials online, like [this
one](http://www.danielbrice.net/blog/10/) by Daniel Brice, if you want to follow
up on this fun little subject.

### Single-variable functions

And, now that we can automatically differentiate functions, we can use this
knowledge directly in our implementations. Let's define a universal "lifter" of
single-variable functions.

We use the function `diffs0` to get a "tower" of derivatives:

``` {.haskell}
ghci> diffs0 (\x -> x^2 - 2 x^3) 4
[-112, -88, -46, -12, 0, 0, 0, 0...
```

The first value is actually
![4\^2 - 2 \\times 4\^3](https://latex.codecogs.com/png.latex?4%5E2%20-%202%20%5Ctimes%204%5E3 "4^2 - 2 \times 4^3").
The second is the derivative
(![2 x - 6x\^2](https://latex.codecogs.com/png.latex?2%20x%20-%206x%5E2 "2 x - 6x^2"))
at 4, the third is the second derivative
![2 - 12 x](https://latex.codecogs.com/png.latex?2%20-%2012%20x "2 - 12 x") at
4, then the third derivative
![-12](https://latex.codecogs.com/png.latex?-12 "-12"), then the fourth
derivative ![0](https://latex.codecogs.com/png.latex?0 "0"), etc.

We only need the actual value and the first two derivatives, so we can pattern
match them as `fx:dfx:ddfx:_ = diffs0 f x`, the derivatives and values of the
function we lift, `f`, around the mean `x`.

At that point, the equations we have from before just translate nicely:

![
\\operatorname{E}\[f(X)\] = f(\\mu\_X) + \\frac{1}{2} f\_{xx}(\\mu\_X) \\sigma\_X\^2
](https://latex.codecogs.com/png.latex?%0A%5Coperatorname%7BE%7D%5Bf%28X%29%5D%20%3D%20f%28%5Cmu_X%29%20%2B%20%5Cfrac%7B1%7D%7B2%7D%20f_%7Bxx%7D%28%5Cmu_X%29%20%5Csigma_X%5E2%0A "
\operatorname{E}[f(X)] = f(\mu_X) + \frac{1}{2} f_{xx}(\mu_X) \sigma_X^2
")

![
\\operatorname{Var}\[f(X)\] = f\_x(\\mu\_X)\^2 \\sigma\_X\^2
](https://latex.codecogs.com/png.latex?%0A%5Coperatorname%7BVar%7D%5Bf%28X%29%5D%20%3D%20f_x%28%5Cmu_X%29%5E2%20%5Csigma_X%5E2%0A "
\operatorname{Var}[f(X)] = f_x(\mu_X)^2 \sigma_X^2
")

And we call ![\\mu\_X](https://latex.codecogs.com/png.latex?%5Cmu_X "\mu_X") `x`
and
![\\sigma\_X\^2](https://latex.codecogs.com/png.latex?%5Csigma_X%5E2 "\sigma_X^2")
`vx`, and this becomes:

``` {.haskell}
y  = fx + ddfx * vx / 2
vy = dfx^2 * vx
```

Putting it all together:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L36-L44

liftU :: Fractional a
      => (forall s. AD s (Tower a) -> AD s (Tower a))
      -> Uncert a
      -> Uncert a
liftU f (Un x vx) = Un y vy
  where
    fx:dfx:ddfx:_ = diffs0 f x
    y             = fx + ddfx * vx / 2
    vy            = dfx^2 * vx
```

The type `forall s. AD s (Tower a) -> AD s (Tower a)` looks a little scary, but
you can think of it as representing a function on `a` (like `negate`, `(*2)`,
etc.) that the *ad* library can differentiate several times --- something you
could use with `diff0` to get a "tower" of derivatives.

And ... that's it! We can already define things like:

``` {.haskell}
negate = liftU negate
recip  = liftU recip
sqrt   = liftU sqrt
sin    = liftU sin
```

### Multivariable functions

*ad* also lets you work multivariable functions, too. To model multivariable
functions, it takes a function from a `Traversable` of vales to a single value.
We can use the `V2` type from the
*[linear](shttp://hackage.haskell.org/package/linear/docs/Linear-V2.html)*
package to pass in a two-variable function:

``` {.haskell}
ghci> grad (\(V2 x y) -> x * y^2 + 3*x) (V2 3 1)
V2 4 6
```

The gradient of
![f(x, y) = x y\^2 + 3x](https://latex.codecogs.com/png.latex?f%28x%2C%20y%29%20%3D%20x%20y%5E2%20%2B%203x "f(x, y) = x y^2 + 3x")
is
![(y\^2 + 3, 2xy)](https://latex.codecogs.com/png.latex?%28y%5E2%20%2B%203%2C%202xy%29 "(y^2 + 3, 2xy)"),
which, at
![(3, 1)](https://latex.codecogs.com/png.latex?%283%2C%201%29 "(3, 1)"), is
indeed ![(4, 6)](https://latex.codecogs.com/png.latex?%284%2C%206%29 "(4, 6)").

The gradient gives us the first order partials, but we need the second order
partials to calculate the new mean, so for that, we can use `hessian`:

``` {.haskell}
ghci> hessian (\(V2 x y) -> x * y^2 + 3*x) (V2 3 1)
V2 (V2 0 2)
   (V2 2 6)
```

The [hessian](https://en.wikipedia.org/wiki/Hessian_matrix) of a function
![f(x,y)](https://latex.codecogs.com/png.latex?f%28x%2Cy%29 "f(x,y)") is
basically a matrix of second-order partial derivatives:

![
\\begin{bmatrix}
f\_{xx}(x, y) & f\_{yx}(x, y) \\\\
f\_{yx}(x, y) & f\_{yy}(x, y)
\\end{bmatrix}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Bbmatrix%7D%0Af_%7Bxx%7D%28x%2C%20y%29%20%26%20f_%7Byx%7D%28x%2C%20y%29%20%5C%5C%0Af_%7Byx%7D%28x%2C%20y%29%20%26%20f_%7Byy%7D%28x%2C%20y%29%0A%5Cend%7Bbmatrix%7D%0A "
\begin{bmatrix}
f_{xx}(x, y) & f_{yx}(x, y) \\
f_{yx}(x, y) & f_{yy}(x, y)
\end{bmatrix}
")

In our case, we only care about the diagonal -- the repeated double-derivatives,
![f\_{xx}](https://latex.codecogs.com/png.latex?f_%7Bxx%7D "f_{xx}") and
![f\_{yy}](https://latex.codecogs.com/png.latex?f_%7Byy%7D "f_{yy}"). Indeed,
the double-partial of our function respect to
![x](https://latex.codecogs.com/png.latex?x "x") is
![0](https://latex.codecogs.com/png.latex?0 "0"), and the double-partial with
respect to ![y](https://latex.codecogs.com/png.latex?y "y") is
![2x](https://latex.codecogs.com/png.latex?2x "2x"), which gives us a hessian
with a diagonal
![(0, 6)](https://latex.codecogs.com/png.latex?%280%2C%206%29 "(0, 6)") for the
input ![(3, 1)](https://latex.codecogs.com/png.latex?%283%2C%201%29 "(3, 1)").

The *ad* package generously gives us a function that lets us calculate the
function's result, its gradient, and its hessian all in one pass:

``` {.haskell}
ghci> hessian' (\(V2 x y) -> x * y^2 + 3*x) (V2 3 1)
(12, V2 (4, V2 0 3)
     V2 (6, V2 2 6)
)
```

We can access the gradient by using `fmap fst` on the second component of the
tuple and access the hessian by using `fmap snd`.

We need a couple of helpers, first --- one to get the "diagonal" of our hessian,
because we only care about the repeated partials:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L46-L50

diag :: [[a]] -> [a]
diag = \case
    []        -> []
    []   :yss -> diag (drop 1 <$> yss)
    (x:_):yss -> x : diag (drop 1 <$> yss)
```

And then a "dot product", utility function, which just multiplies two lists
together component-by-component and sums the results:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L52-L53

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)
```

And now we can write our multi-variate function lifter:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L55-L72

liftUF :: (Traversable f, Fractional a)
       => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a))
       -> f (Uncert a)
       -> Uncert a
liftUF f us = Un y vy
  where
    xs          =         uMean <$> us
    vxs         = toList (uVar  <$> us)
    (fx, hgrad) = hessian' f xs
    dfxs        = fst <$> hgrad
    hess        = snd <$> hgrad
    y           = fx + partials / 2
      where
        partials = dot vxs
                 . diag
                 $ toList (fmap toList hess) -- from f (f a) to [[a]]
    vy          = dot vxs
                $ toList ((^2) <$> dfxs)
```

(Again, don't mind the scary type
`forall s. f (AD s (Sparse a)) -> AD s (Sparse a)`, it's just *ad*'s type for
things you can use `hessian'` on)

And we can write some nice helper functions so we can use them more naturally:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs#L74-L87

liftU2 :: Fractional a
       => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
       -> Uncert a
       -> Uncert a
       -> Uncert a
liftU2 f x y = liftUF (\(V2 x' y') -> f x' y') (V2 x y)

liftU3 :: Fractional a
       => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
       -> Uncert a
       -> Uncert a
       -> Uncert a
       -> Uncert a
liftU3 f x y z = liftUF (\(V3 x' y' z') -> f x' y' z') (V3 x y z)
```

At this point, our code is pretty much complete. We can fill in the other
two-argument functions from the numeric typeclasses:

``` {.haskell}
(+)     = liftU2 (+)
(*)     = liftU2 (*)
(/)     = liftU2 (/)
(**)    = liftU2 (**)
logBase = liftU2 logBase
```

Admittedly, there's still some slight boilerplate (that you can get rid of with
some Template Haskell, maybe), but you have a *lot* less room for error, and a
lot simpler to check over and read to make sure you didn't miss any bugs.

Wrapping it up
--------------

The full code (with all of the numeric instances fully implemented) is up [on
github](https://github.com/mstksg/inCode/tree/master/code-samples/uncertain/Uncertain.hs),
which you can run and explore and test by executing it or loading it with
`stack ghci`. I've added a special *Show* instance that "rounds" your values to
as many digits that your uncertainty suggests, to give more meaningful `show`s.

All of what's in this post is actually up on my
*[uncertain](https://hackage.haskell.org/package/uncertain)* package on hackage,
if you want to use it in your own projects, or see how I take this and make it
more robust for real-world applications. The project also has more features on
top of the basic things shown here.

### Verification and Accuracy

My *[uncertain](https://hackage.haskell.org/package/uncertain)* package has a
Monte Carlo module to propagate uncertainty through Monte Carlo simulations.
Let's see how the values compare!

``` {.haskell}
ghci> x + y         -- Monte Carlo Results:
46 +/- 2            -- actually 46 +/- 2
ghci> x * y
450 +/- 40          -- actually 450 +/- 40
ghci> sqrt (x + y)
6.8 +/- 0.2         -- actually 6.8 +/- 0.2
ghci> logBase y x
0.78 +/- 0.02       -- actually 0.78 +/- 0.02
ghci> log (x**y)
85.9 +/- 0.3        -- actually 83 +/- 6
```

So, it looks like the mathematical model of uncertainty propagation matched up
well with the "actual" results we gain from Monte Carlo simulations! The only
one of our examples that was significantly wrong was the
![\\operatorname{log}(x\^y)](https://latex.codecogs.com/png.latex?%5Coperatorname%7Blog%7D%28x%5Ey%29 "\operatorname{log}(x^y)")
example, which heavily underestimated the uncertainty by about a factor of 20.
But, remember, the model was derived after dropping the 2nd, 3rd, 4th, etc.
terms of the taylor expansion for the calculation of the new uncertainty, and
the 4th, 6th, etc. terms of the taylor expansion for the calculation of the new
mean. For functions that have high second, third, fourth derivatives relative to
the mean and the uncertainty, it's going to be a bit off.

### What next?

For an extension on the mathematics behind this method, Dan Piponi has a [great
article](http://blog.sigfpe.com/2011/08/computing-errors-with-square-roots-of.html)
with a lot of good references for further reading on the formal method.

Going off of what we've done here, a simple extension of this would be to
implement the Monte Carlo simulator I mentioned above, which is pretty
straightforward to implement with the
*[mwc-random](https://hackage.haskell.org/package/mwc-random)* package.

However, the most unsettling thing here that we never deal with is what happens
correlated terms that are combined. All of our math assumed uncorrelated
samples. But what happens if we have expressions that involve additions of
correlated values?

For example:

``` {.haskell}
ghci> let x = 14.6 +/- 0.8 in x + x
29 +/- 1
ghci> let x = 14.6 +/- 0.8 in 2*x
29 +/- 2
```

Unfortunately, `x + x` is different than `2*x`. This is because `x` acts like an
*independent generator*, so when you say `x + x`, it expands to
`(14.6 +/- 0.8) + (14.6 +/- 0.8)`, which represents the addition of two
independent samples.

When you say `2*x`, that represents sampling `x` *once* and *doubling* it. If
you sample `x` and double it, any error in `x` will also be doubled. That's why
the uncertainty is greater in the `2*x` version.

How can we account for correlated values that are combined in complex ways? Stay
tuned for the next part of the
[series](https://blog.jle.im/entries/series/+uncertain.html)![^3]

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

[^1]: You can simulate noisy data by using uniform noise distributions, Gaussian
    distributions, or however manner you like that has a given expected value
    (mean) and "spread". Verify by checking the [standard
    deviation](https://en.wikipedia.org/wiki/Standard_deviation) of the sums!

[^2]: This law actually comes from the mathematical *definition* of variance, so
    does not assume anything about the underlying distribution of the sampling
    --- just that they are independent, and that they have defined variances.

[^3]: Or just look at my
    [package](https://hackage.haskell.org/package/uncertain) :)
