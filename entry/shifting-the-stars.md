Shifting the Stars: Advent of Code with Galilean Optimization
=============================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/shifting-the-stars.html)

Another short Advent of Code post! [Advent of Code
2018](https://adventofcode.com/2018) is in full swing; we're 40% of the way
through. Every once in a while, if I find a fun way to solve a problem, I'll
make a short post about it. You can check out my other ones [here on the series
page](https://blog.jle.im/entries/series/+advent-of-code.html), and you can also
find my [daily
reflections](https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md)
here, as well. And, again, if you're following along in Haskell, why not hop on
[glguy's](https://twitter.com/glguy) semi-official [Haskell
Leaderboard](https://adventofcode.com/2018/leaderboard/private) (join code
`43100-84040706`)! There are also Haskellers on freenode \#\#adventofcode, and
also \#adventofcode on the Functional Programming slack. You might also find my
[advent of code api](https://hackage.haskell.org/package/advent-of-code-api)
haskell bindings helpful too!

Today, we're going to be using linear algebra, calculus, and galilian
transformations to solve the *Day 10* challenge. (That's right, this isn't just
a Haskell blog, I do have
[math](https://blog.jle.im/entries/category/@math.html) posts on occasion too :)
)

Part 1
------

> It's no use; your navigation system simply isn't capable of providing walking
> directions in the arctic circle, and certainly not in 1018.
>
> The Elves suggest an alternative. In times like these, North Pole rescue
> operations will arrange points of light in the sky to guide missing Elves back
> to base. Unfortunately, the message is easy to miss: the points move slowly
> enough that it takes hours to align them, but have so much momentum that they
> only stay aligned for a second. If you blink at the wrong time, it might be
> hours before another message appears.
>
> You can see these points of light floating in the distance, and record their
> position in the sky and their velocity, the relative change in position per
> second (your puzzle input). The coordinates are all given from your
> perspective; given enough time, those positions and velocities will move the
> points into a cohesive message!
>
> Rather than wait, you decide to fast-forward the process and calculate what
> the points will eventually spell.
>
> For example, suppose you note the following points:
>
>     position=< 9,  1> velocity=< 0,  2>
>     position=< 7,  0> velocity=<-1,  0>
>     position=< 3, -2> velocity=<-1,  1>
>     position=< 6, 10> velocity=<-2, -1>
>     position=< 2, -4> velocity=< 2,  2>
>     position=<-6, 10> velocity=< 2, -2>
>     position=< 1,  8> velocity=< 1, -1>
>     position=< 1,  7> velocity=< 1,  0>
>     position=<-3, 11> velocity=< 1, -2>
>     position=< 7,  6> velocity=<-1, -1>
>     position=<-2,  3> velocity=< 1,  0>
>     position=<-4,  3> velocity=< 2,  0>
>     position=<10, -3> velocity=<-1,  1>
>     position=< 5, 11> velocity=< 1, -2>
>     position=< 4,  7> velocity=< 0, -1>
>     position=< 8, -2> velocity=< 0,  1>
>     position=<15,  0> velocity=<-2,  0>
>     position=< 1,  6> velocity=< 1,  0>
>     position=< 8,  9> velocity=< 0, -1>
>     position=< 3,  3> velocity=<-1,  1>
>     position=< 0,  5> velocity=< 0, -1>
>     position=<-2,  2> velocity=< 2,  0>
>     position=< 5, -2> velocity=< 1,  2>
>     position=< 1,  4> velocity=< 2,  1>
>     position=<-2,  7> velocity=< 2, -2>
>     position=< 3,  6> velocity=<-1, -1>
>     position=< 5,  0> velocity=< 1,  0>
>     position=<-6,  0> velocity=< 2,  0>
>     position=< 5,  9> velocity=< 1, -2>
>     position=<14,  7> velocity=<-2,  0>
>     position=<-3,  6> velocity=< 2, -1>
>
> Each line represents one point. Positions are given as `<X, Y>` pairs: X
> represents how far left (negative) or right (positive) the point appears,
> while Y represents how far up (negative) or down (positive) the point appears.
>
> At `0` seconds, each point has the position given. Each second, each point's
> velocity is added to its position. So, a point with velocity `<1, -2>` is
> moving to the right, but is moving upward twice as quickly. If this point's
> initial position were `<3, 9>`, after `3` seconds, its position would become
> `<6, 3>`.
>
> Over time, the points listed above would move like this:
>
>     Initially:
>     ........#.............
>     ................#.....
>     .........#.#..#.......
>     ......................
>     #..........#.#.......#
>     ...............#......
>     ....#.................
>     ..#.#....#............
>     .......#..............
>     ......#...............
>     ...#...#.#...#........
>     ....#..#..#.........#.
>     .......#..............
>     ...........#..#.......
>     #...........#.........
>     ...#.......#..........
>
>     After 1 second:
>     ......................
>     ......................
>     ..........#....#......
>     ........#.....#.......
>     ..#.........#......#..
>     ......................
>     ......#...............
>     ....##.........#......
>     ......#.#.............
>     .....##.##..#.........
>     ........#.#...........
>     ........#...#.....#...
>     ..#...........#.......
>     ....#.....#.#.........
>     ......................
>     ......................
>
>     After 2 seconds:
>     ......................
>     ......................
>     ......................
>     ..............#.......
>     ....#..#...####..#....
>     ......................
>     ........#....#........
>     ......#.#.............
>     .......#...#..........
>     .......#..#..#.#......
>     ....#....#.#..........
>     .....#...#...##.#.....
>     ........#.............
>     ......................
>     ......................
>     ......................
>
>     After 3 seconds:
>     ......................
>     ......................
>     ......................
>     ......................
>     ......#...#..###......
>     ......#...#...#.......
>     ......#...#...#.......
>     ......#####...#.......
>     ......#...#...#.......
>     ......#...#...#.......
>     ......#...#...#.......
>     ......#...#..###......
>     ......................
>     ......................
>     ......................
>     ......................
>
>     After 4 seconds:
>     ......................
>     ......................
>     ......................
>     ............#.........
>     ........##...#.#......
>     ......#.....#..#......
>     .....#..##.##.#.......
>     .......##.#....#......
>     ...........#....#.....
>     ..............#.......
>     ....#......#...#......
>     .....#.....##.........
>     ...............#......
>     ...............#......
>     ......................
>     ......................
>
> After 3 seconds, the message appeared briefly: `HI`. Of course, your message
> will be much longer and will take many more seconds to appear.
>
> *What message will eventually appear in the sky?*

The problem tells us to talk about a system of N particles, each moving at
constant velocity. From this, we can see that the position of particle
![i](https://latex.codecogs.com/png.latex?i "i") at time
![t](https://latex.codecogs.com/png.latex?t "t") is
![\\mathbf{r}\_i + \\mathbf{v}\_i t](https://latex.codecogs.com/png.latex?%5Cmathbf%7Br%7D_i%20%2B%20%5Cmathbf%7Bv%7D_i%20t "\mathbf{r}_i + \mathbf{v}_i t"),
where
![\\mathbf{r}\_i](https://latex.codecogs.com/png.latex?%5Cmathbf%7Br%7D_i "\mathbf{r}_i")
is the *initial position* vector, and
![\\mathbf{v}\_i](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D_i "\mathbf{v}_i")
is the *velocity* vector.

More generally, we can express this in terms of matrices. If we talk talk about
![R](https://latex.codecogs.com/png.latex?R "R") as the
![n \\times 2](https://latex.codecogs.com/png.latex?n%20%5Ctimes%202 "n \times 2")
matrix of initial positions, and
![V](https://latex.codecogs.com/png.latex?V "V") as the
![n \\times 2](https://latex.codecogs.com/png.latex?n%20%5Ctimes%202 "n \times 2")
matrix of initial velocities:

\$\$ \\begin{aligned} R & =

\\begin{bmatrix} x\_0 & y\_0 \\ x\_1 & y\_1 \\ x\_2 & y\_2 \\ \vdots & \vdots
\\end{bmatrix} \\ V & = \\begin{bmatrix} v\_x0 & v\_y0 \\ v\_x1 & v\_y1 \\ v\_x2
& v\_y2 \\ \vdots & \vdots \\end{bmatrix} \\end{aligned} \$\$

Then we can say that the state of the total system at time
![t](https://latex.codecogs.com/png.latex?t "t") is given by
![R + V t](https://latex.codecogs.com/png.latex?R%20%2B%20V%20t "R + V t")

Now, how can we find the time when all of the letters are aligned?

For this, we can make a *somewhat justified guess*: looking at the input data,
we see that things start out as "scattered", and end up in a clean clustered
arrangement. We know that the ending arrangement must be clustered fairly close
together because we only have a few hundred points in the input data set,
whereas the start times are all in the thousands or higher. And, once things get
clustered, they will also get un-clustered right away, because of the randomness
of the directions of motion.

This gives us a clue: if we can find the
![t](https://latex.codecogs.com/png.latex?t "t") that will give us the
![R + V t](https://latex.codecogs.com/png.latex?R%20%2B%20V%20t "R + V t") with
the *least variance*, we are good to go!

All of a sudden, this is now an
[optimization](https://en.wikipedia.org/wiki/Mathematical_optimization) problem.
Find the ![t](https://latex.codecogs.com/png.latex?t "t") that minimizes the
variance of x plus the variance of y. This can be done by using some calculus:
we can compute the function producing the sum of variances as a function of
time, take the first derivative with respect to time, and solve for
![t](https://latex.codecogs.com/png.latex?t "t") that makes the first derivative
zero.

The typical formula for finding the sum of variances of a matrix
![M](https://latex.codecogs.com/png.latex?M "M") is to take the
[trace](https://en.wikipedia.org/wiki/Trace_(linear_algebra)) of the [covariance
matrix](https://en.wikipedia.org/wiki/Covariance_matrix),
![\\mathrm{Tr} \\left( \\left(M - \\mu\_M \\right)\^T \\left(M - \\mu\_M \\right) \\right)](https://latex.codecogs.com/png.latex?%5Cmathrm%7BTr%7D%20%5Cleft%28%20%5Cleft%28M%20-%20%5Cmu_M%20%5Cright%29%5ET%20%5Cleft%28M%20-%20%5Cmu_M%20%5Cright%29%20%5Cright%29 "\mathrm{Tr} \left( \left(M - \mu_M \right)^T \left(M - \mu_M \right) \right)").
However, in this form, it's not too fun to work with. That's because we have to
re-compute the mean of of the positions at every point, and things will get
messy before they get clean.

Conceptually, however, we have a powerful tool: the [Center of Mass
frame](https://en.wikipedia.org/wiki/Center-of-momentum_frame). Essentially,
because our system has no external forces (and no net acceleration), we can
*perform a Galilean transform* into a frame of reference where the center of
mass is *fixed at the origin*, and *never changes*. If we can do this, then we
only need to compute
![\\mathrm{Tr} \\left(M\^T M \\right)](https://latex.codecogs.com/png.latex?%5Cmathrm%7BTr%7D%20%5Cleft%28M%5ET%20M%20%5Cright%29 "\mathrm{Tr} \left(M^T M \right)")
(since we guarantee that the mean of
![M](https://latex.codecogs.com/png.latex?M "M") is 0), which is relatively easy
peasy. If we find the minimizing time in the CoM frame, then we can also use
that same time in our original frame, because Galilean transformations leave
time unchanged (unlike Lorentz transforms and other similar coordinate
transformations).

Because our system has points of all equal "mass", we can shift
![R](https://latex.codecogs.com/png.latex?R "R") into
![\\hat{R}](https://latex.codecogs.com/png.latex?%5Chat%7BR%7D "\hat{R}")
(![R](https://latex.codecogs.com/png.latex?R "R") shifted into the center of
mass frame) and ![V](https://latex.codecogs.com/png.latex?V "V") into
![\\hat{V}](https://latex.codecogs.com/png.latex?%5Chat%7BV%7D "\hat{V}") by
just subtracting by the *initial* mean:

![
\\begin{aligned}
\\hat{R} & = R - \\mu\_R \\\\
\\hat{V} & = V - \\mu\_V
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Chat%7BR%7D%20%26%20%3D%20R%20-%20%5Cmu_R%20%5C%5C%0A%5Chat%7BV%7D%20%26%20%3D%20V%20-%20%5Cmu_V%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\hat{R} & = R - \mu_R \\
\hat{V} & = V - \mu_V
\end{aligned}
")

This means that our formula for variance at time T is now simple to manipulate.
Because we now know that center of mass *is always zero*, we can compute the sum
of variance as:

![
\\lvert \\Sigma(t) \\rvert = \\mathrm{Tr} \\left( \\hat{R} + \\hat{V} t \\right)\^T \\left( \\hat{R} + \\hat{V} t \\right)
](https://latex.codecogs.com/png.latex?%0A%5Clvert%20%5CSigma%28t%29%20%5Crvert%20%3D%20%5Cmathrm%7BTr%7D%20%5Cleft%28%20%5Chat%7BR%7D%20%2B%20%5Chat%7BV%7D%20t%20%5Cright%29%5ET%20%5Cleft%28%20%5Chat%7BR%7D%20%2B%20%5Chat%7BV%7D%20t%20%5Cright%29%0A "
\lvert \Sigma(t) \rvert = \mathrm{Tr} \left( \hat{R} + \hat{V} t \right)^T \left( \hat{R} + \hat{V} t \right)
")

We can do some simplification, remembering that the trace distributes over
addition, and that
![\\mathrm{Tr} \\left( X\^T Y \\right) = \\mathrm{Tr} \\left (Y\^T \\right)](https://latex.codecogs.com/png.latex?%5Cmathrm%7BTr%7D%20%5Cleft%28%20X%5ET%20Y%20%5Cright%29%20%3D%20%5Cmathrm%7BTr%7D%20%5Cleft%20%28Y%5ET%20%5Cright%29 "\mathrm{Tr} \left( X^T Y \right) = \mathrm{Tr} \left (Y^T \right)"):

![
\\lvert \\Sigma(t) \\rvert = \\mathrm{Tr} (\\hat{R}\^T \\hat{R})
                   + 2 \\mathrm{Tr} (\\hat{R}\^T \\hat{V}) t
                   + \\mathrm{Tr} (\\hat{V}\^T \\hat{V}) t\^2
](https://latex.codecogs.com/png.latex?%0A%5Clvert%20%5CSigma%28t%29%20%5Crvert%20%3D%20%5Cmathrm%7BTr%7D%20%28%5Chat%7BR%7D%5ET%20%5Chat%7BR%7D%29%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%2B%202%20%5Cmathrm%7BTr%7D%20%28%5Chat%7BR%7D%5ET%20%5Chat%7BV%7D%29%20t%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%2B%20%5Cmathrm%7BTr%7D%20%28%5Chat%7BV%7D%5ET%20%5Chat%7BV%7D%29%20t%5E2%0A "
\lvert \Sigma(t) \rvert = \mathrm{Tr} (\hat{R}^T \hat{R})
                   + 2 \mathrm{Tr} (\hat{R}^T \hat{V}) t
                   + \mathrm{Tr} (\hat{V}^T \hat{V}) t^2
")

Now, we want to minimize the sum of variances. So to do that, we can take the
first derivative with respect to
![t](https://latex.codecogs.com/png.latex?t "t"), and set it to be zero:

![
\\begin{aligned}
\\frac{d}{d t} \\lvert \\Sigma(t) \\rvert & = 2 \\mathrm{Tr} (\\hat{R}\^T \\hat{V})
                                  + 2 \\mathrm{Tr} (\\hat{V}\^T \\hat{V}) t \\\\
0 & = 2 \\mathrm{Tr} (\\hat{R}\^T \\hat{V}) + 2 \\mathrm{Tr} (\\hat{V}\^T \\hat{V}) t\_f \\\\
t\_f & = - \\frac{\\mathrm{Tr} (\\hat{R}\^T \\hat{V})}{\\mathrm{Tr}(\\hat{V}\^T \\hat{V})}
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Cfrac%7Bd%7D%7Bd%20t%7D%20%5Clvert%20%5CSigma%28t%29%20%5Crvert%20%26%20%3D%202%20%5Cmathrm%7BTr%7D%20%28%5Chat%7BR%7D%5ET%20%5Chat%7BV%7D%29%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%2B%202%20%5Cmathrm%7BTr%7D%20%28%5Chat%7BV%7D%5ET%20%5Chat%7BV%7D%29%20t%20%5C%5C%0A0%20%26%20%3D%202%20%5Cmathrm%7BTr%7D%20%28%5Chat%7BR%7D%5ET%20%5Chat%7BV%7D%29%20%2B%202%20%5Cmathrm%7BTr%7D%20%28%5Chat%7BV%7D%5ET%20%5Chat%7BV%7D%29%20t_f%20%5C%5C%0At_f%20%26%20%3D%20-%20%5Cfrac%7B%5Cmathrm%7BTr%7D%20%28%5Chat%7BR%7D%5ET%20%5Chat%7BV%7D%29%7D%7B%5Cmathrm%7BTr%7D%28%5Chat%7BV%7D%5ET%20%5Chat%7BV%7D%29%7D%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\frac{d}{d t} \lvert \Sigma(t) \rvert & = 2 \mathrm{Tr} (\hat{R}^T \hat{V})
                                  + 2 \mathrm{Tr} (\hat{V}^T \hat{V}) t \\
0 & = 2 \mathrm{Tr} (\hat{R}^T \hat{V}) + 2 \mathrm{Tr} (\hat{V}^T \hat{V}) t_f \\
t_f & = - \frac{\mathrm{Tr} (\hat{R}^T \hat{V})}{\mathrm{Tr}(\hat{V}^T \hat{V})}
\end{aligned}
")

And just like that, we have a formula for
![t](https://latex.codecogs.com/png.latex?t "t")!

We can simplify this a little more by remembering that the trace of a matrix
multiplication is the sum of the dot product of the rows. That means we can
write:

![
t\_f = - \\frac{\\Sigma\_i \\hat{\\mathbf{r}}\_i \\cdot \\hat{\\mathbf{v}}\_i}{\\Sigma\_i \\hat{\\mathbf{v}}\_i \\cdot \\hat{\\mathbf{v}}\_i}
](https://latex.codecogs.com/png.latex?%0At_f%20%3D%20-%20%5Cfrac%7B%5CSigma_i%20%5Chat%7B%5Cmathbf%7Br%7D%7D_i%20%5Ccdot%20%5Chat%7B%5Cmathbf%7Bv%7D%7D_i%7D%7B%5CSigma_i%20%5Chat%7B%5Cmathbf%7Bv%7D%7D_i%20%5Ccdot%20%5Chat%7B%5Cmathbf%7Bv%7D%7D_i%7D%0A "
t_f = - \frac{\Sigma_i \hat{\mathbf{r}}_i \cdot \hat{\mathbf{v}}_i}{\Sigma_i \hat{\mathbf{v}}_i \cdot \hat{\mathbf{v}}_i}
")

Once we find this, we can plug into our original form, to find that our final
points are, in our un-shifted coordinates,
![R + V t](https://latex.codecogs.com/png.latex?R%20%2B%20V%20t "R + V t") (we
can also use the shifted coordinates as well).

We can write this as a Haskell function, assuming we take in a list of
`V2 Double` for velocities and `V2 Double` for positions, from the
*[linear](https://en.wikipedia.org/wiki/Center-of-momentum_frame)* library:

``` {.haskell}
-- | Shift so that centroid is at zero
centralize :: [V2 Double] -> [V2 Double]
centralize ps = map (subtract mean) ps
  where
    mean = (/ len) <$> tot
    (Sum tot, Sum len) = foldMap (\x -> (Sum x, Sum 1)) ps

-- | Sum of dot products
sumOfDots :: [Point] -> [Point] -> Double
sumOfDots xs ys = sum $ zipWith L.dot xs ys

findWord
    :: [V2 Double]              -- ^ velocities
    -> [V2 Double]              -- ^ initial positions
    -> ([V2 Double], Double)    -- ^ points in word, and final time t
findWord (centralize->vs) (centralize->xs) = (final, t)
  where
    t     = negate (sumOfDots xs vs / sumOfDots vs vs)
    final = zipWith (\v x -> x + t L.*^ v) vs xs
```

To answer the actual problem, we do need to `round` the time (and the final
points) to display them on the screen.

Part 2
------

> Good thing you didn't have to wait, because that would have taken a long time
> - much longer than the `3` seconds in the example above.
>
> Impressed by your sub-hour communication capabilities, the Elves are curious:
> *exactly how many seconds would they have needed to wait for that message to
> appear?*

This one is just ![t](https://latex.codecogs.com/png.latex?t "t"), which we
solved for in the last part!

Message in the Stars
--------------------

Optimization by finding the first derivative is a common tool in math that is
definitely under-utilized! In practice, unless we have a really clean system, we
won't be able to analytically "solve for zero" in most situations. However, this
system shows all of the signs of being well-behaved: the thing we are minimizing
is quadratic on our variable, so the first derivative will be linear on our
variable, making "solving for zero" very simple.

The first step was looking at our system as a matrix formula in the first place.
This gave us key insights from linear algebra that we could exploit. However,
even though things are quadratic, they might have been mentally complex because
we have to re-evaluate the mean of all of the points at all points in time in
order to compute the sum of variances. To get around this, we borrow from a
common tool used in classical mechanics that allows us not only to *fix the
mean* over the entire time span, but also *set it to the origin*, so we don't
even have to worry about it ever coming up at any point in time. This made our
symbolic manipulation easy enough to do on a few lines of notebook paper. And
isn't it cute that we use the *Galilean* transformed, named after someone who is
famous for having studied the motion of astronomical bodies? Maybe that was a
subtle hint from the author of the challenges ;)

Anyway, I thought this was a fun twist on the typical Advent of Code challenges.
It's always fun when something that you might think can only be solved by
simulation turns out to have a closed-form solution...but even more fun when the
closed-form solution turns out to just be simple linear algebra:

![
t\_f = - \\frac{\\Sigma\_i \\hat{\\mathbf{r}}\_i \\cdot \\hat{\\mathbf{v}}\_i}{\\Sigma\_i \\hat{\\mathbf{v}}\_i \\cdot \\hat{\\mathbf{v}}\_i}
](https://latex.codecogs.com/png.latex?%0At_f%20%3D%20-%20%5Cfrac%7B%5CSigma_i%20%5Chat%7B%5Cmathbf%7Br%7D%7D_i%20%5Ccdot%20%5Chat%7B%5Cmathbf%7Bv%7D%7D_i%7D%7B%5CSigma_i%20%5Chat%7B%5Cmathbf%7Bv%7D%7D_i%20%5Ccdot%20%5Chat%7B%5Cmathbf%7Bv%7D%7D_i%7D%0A "
t_f = - \frac{\Sigma_i \hat{\mathbf{r}}_i \cdot \hat{\mathbf{v}}_i}{\Sigma_i \hat{\mathbf{v}}_i \cdot \hat{\mathbf{v}}_i}
")

"It's just dot products all the way down."

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
