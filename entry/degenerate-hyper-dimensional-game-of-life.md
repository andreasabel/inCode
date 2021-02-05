Degenerate Hyper-Dimensional Game of Life
=========================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/degenerate-hyper-dimensional-game-of-life.html)

tldr: Over the course of a month, we were able to successive new mathematical
properties of a "degenerate" hyper-dimensional game of life\" to take a "10
dimensions may just barely be possible on a supercomputer" to "10 dimensions is
easy enough to be run on any modern browser, and 40 dimensions can be reached
with a compiled language". Includes interactive visualizations and simulations!

This is a story about breaking the degenerate hyper-dimensional game of life by
exploratory visualizations and math! Let's travel back in time: t'was the night
before December 17, 2020, The release of ["Conway
Cubes"](https://adventofcode.com/2020/day/17), day 17 of the "Advent of Code"
(fun little coding puzzles building up to Christmas). One part about Advent of
Code I've always found especially fun is that, because the problems are so
self-contained and tidy, they are often *open-ended* in the interesting ways you
can solve them or expand them.

On the surface, Day 17 seemed to essentially be a straightforward extension of
[Conway's Game Of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
("GoL"). GoL is a simulation played out on a 2d grid, where cells are "on" and
"off", and at each step of the simulation, the on/off cells spread and propagate
in fascinating ways based on the state of their neighbors.

The twist of the Advent of Code puzzle is it asks what would happen if we played
out the rules of GoL in 3d, and then 4d! The "starting conditions" are a 8x8 2D
grid picked out for each participant, and the puzzle solution is the number of
live cells after six steps. My personal starting conditions were:

    #####..#
    #..###.#
    ###.....
    .#.#.#..
    ##.#..#.
    ######..
    .##..###
    ###.####

I submitted my answer with a direct implementation (scoring the 66th spot on the
leader board for that day)...and that was that for the "competitive" part. But
the real fun always starts after! When discussing with some friends, we started
talking about the trade-offs of different implementations and realized that the
extra dimensionality was no joke...as you upped the number of dimensions, the
number of points you have to consider grow as
![O((2t+6)\^d)](https://latex.codecogs.com/png.latex?O%28%282t%2B6%29%5Ed%29 "O((2t+6)^d)"),
and the number of neighbors of each point to check grows as
![O(3\^d)](https://latex.codecogs.com/png.latex?O%283%5Ed%29 "O(3^d)"). So for
4D it's definitely possible to solve naively...but anything higher is going to
strain. My naive solution on 6D took three minutes, and 7D in a reasonable
amount of time (612,220,032 points with 2,186 neighbors each) seemed
*impossible* on commercial consumer hardware because of the sheer number of
points in 7D space. But I thought...what if a breakthrough in optimization was
possible? I set my goal as 10D (3,570,467,226,624 points with 59,048 neighbors
each), not knowing if it was possible.

And soon...a breakthrough did come! Someone brought up that if we look at the 3d
version, we see there's actually a *mirror symmetry*! That is, because
everything starts off on the xy plane, with z=0, the resulting progression must
be symmetrical on both sides (positive and negative z).

![d=3 animation by
[u/ZuBsPaCe](https://www.reddit.com/r/adventofcode/comments/kfa3nr/2020_day_17_godot_cubes_i_think_i_went_a_bit_too/)](/img/entries/advent-gol/life3d.gif "d=3 animation u/ZuBsPaCe")

In the end that means we only have to simulate one of the "halves"/"quadrants"
of the higher-dimensional space, since all "quadrants" are identical! This saves
down the number of points by a factor of two for each extra dimension
(![O(2\^{d-2})](https://latex.codecogs.com/png.latex?O%282%5E%7Bd-2%7D%29 "O(2^{d-2})")).
My 7D implementation completed in 6 minutes! 8D still hung forever, though.

Well, it didn't get us to d=10...but this discovery completely changed how we
saw this puzzle. With one breakthrough down, we began to believe that there
would be more just around the corner, made possible by our problem's special
degeneracy (that is, that we start on a 2d slice).

Such a dream (as posed in [this reddit thread I
started](https://www.reddit.com/r/adventofcode/comments/kfb6zx/day_17_getting_to_t6_at_for_higher_spoilerss/))
turned into a month-long quest of breakthrough after breakthrough, exploiting
different aspects of this degeneracy! It was a long, harrowing journey full of
sudden twists and turns and bursts of excitement when new innovations came. And
in the end, the hopeful question "What if d=10 was possible?" turned into "d=10
in 100ms, d=40 in eight minutes." I even got d=10 fast enough to run on easily
any modern browser --- this post includes those simulations! Furthermore, the
whole journey became an adventure in the power of visualization combined with
abstract thinking.

So, let's take a deep dive --- deeper than you probably ever expected to dive
into any particular degenerate starting conditions of a hyper-dimensional game
of life :D

Starting Off
------------

First of all, let's meet our friend for the rest of this journey. In the drawer
below, you can draw (with your mouse) the 8x8 grid you want to simulate for the
rest of this post. As you draw, the rest of the visualizations will update to
use this as their initial conditions.

::: {#golDrawer}
Please enable Javascript
:::

And for fun, here's a 2D vanilla game of life implementation (for six time
steps) to test out your creation. I recommend trying out some of the
[interesting well-known
patterns](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Examples_of_patterns)!

::: {#gol2D}
Please enable Javascript
:::

Now that that's there, let's start at the beginning: what's the naive, baseline
solution?

A reasonable initial thought would be:

1.  Keep a 2D (or 3D, or 4D, etc.) array of booleans.
2.  At each step:
    a.  Make a fresh copy of the entire space
        (![O(n\^d)](https://latex.codecogs.com/png.latex?O%28n%5Ed%29 "O(n^d)")).
    b.  Loop over each item in your array
        (![O(n\^d)](https://latex.codecogs.com/png.latex?O%28n%5Ed%29 "O(n^d)")).
        Count all of the neighbors
        (![O(3\^d)](https://latex.codecogs.com/png.latex?O%283%5Ed%29 "O(3^d)"))
        that are `true` ("alive"), and write to the new array based on the rules
        table of GoL (2 or 3 neighbors for a live cell stays alive, 3 neighbors
        for a dead cell turns alive).
3.  You have a new array! Loop again six times.

Sounds reasonable enough! And this does work for the 2D case pretty well (like
in the [Day 11 puzzle](https://adventofcode.com/2020/day/11)). However, there
are some clear issues when moving into higher dimensions. The size of your array
grows exponentially on your dimension, and so does the number of neighbors you'd
have to check. And the [curse of
dimensionality](https://en.wikipedia.org/wiki/Curse_of_dimensionality) assures
us that more and more of that array would become wasted as the proportion of
"on" points shrinks to zero for higher dimensions.

Oh, but what's that? The percentage of "on" points shrinks to zero for higher
dimensions? That actually sounds like something we can use to our advantage!
The...*blessing of dimensionality*, I daresay? Because we know the vast majority
of our points will be "off", there's another method.

1.  Keep a *set* of points that are "on".
2.  At each step:
    a.  Initialize a dynamic map (key-value store) of points to integers. This
        map associates each point to the number of live neighbors it has.

    b.  For each step, iterate over each of your "on" points, expand all of
        their neighbors ![n\_i](https://latex.codecogs.com/png.latex?n_i "n_i")
        (![(O(3\^d))](https://latex.codecogs.com/png.latex?%28O%283%5Ed%29%29 "(O(3^d))")),
        and increment the value associated with
        ![n\_i](https://latex.codecogs.com/png.latex?n_i "n_i") in your dynamic
        map.

        For example, if the point `<2,3>` is in your set of live points, you
        would add increment the map's values at keys `<1,2>`, `<2,2>`, `<3,2>`,
        etc.: all 8 neighbors of `<2,3>`.

    c.  Collect your new set of on points: keep all of the keys in your dynamic
        map corresponding to live points if their integers are 2 or 3, and keep
        all of the keys in your dynamic map corresponding to dead points if
        their integers are 3.

3.  You have a new set! Loop again six times!

I discuss this algorithm much more deeply with actual code in [my solutions
write-up in my Advent of Code reflections
journal](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md).

This method nets us a huge advantage because we now only have to loop over the
number of items that we know are alive! Any points far away from our set of
alive points can be properly ignored. This narrows down our huge iteration
space, and the benefits compound with every dimension due to the blessing of
dimensionality!\[\^bittweak\]

The nice thing about this method is that it's easy enough to generalize to any
dimension: instead of, say, keeping `[x,y]` in your set for 2D, just keep
`[x,y,z]` for 3D, or any length array of coordinates. One minor trick you need
to think through is generating all
![3\^d-1](https://latex.codecogs.com/png.latex?3%5Ed-1 "3^d-1") neighbors, but
but that's going to come down to a d-ary [cartesian
product](https://observablehq.com/@d3/d3-cross) of `[-1,0,1]` to
itself.\[\^crosstrick\]

Here's a version of the set-based implementation, using a nice trick I learned
from [phaazon](https://twitter.com/phaazon_) to get the right neighbors by doing
a cartesian product against `[0,-1,1]`, which leaves the first item as the
`<0,0>` "original point" we want to exclude:

``` {.python}
from itertools import islice, product
from collections import Counter

def mk_neighbs(point):
    """Return neighboring points, each equally weighted

    (1,2)
    => [(1, 1), (1, 3), (0, 2), (0, 1), (0, 3), (2, 2), (2, 1), (2, 3)]
    """
    gen = product(*[[x, x-1, x+1] for x in point])
    # skip the first item, the original point
    next(gen)
    return gen

def step(pts):
    """Takes a set of points (tuples) and steps them in the simulation
    """
    neighbs = Counter()
    for point in pts:
        neighbs += Counter(mk_neighbs(point))

    def validate(point, ncount):
        if point in pts:
            return ncount == 2 or ncount == 3
        else:
            return ncount == 3

    return [p for p, n in neighbs.items() if validate(p, n)]
```

And...there's actually a neat optimization we can use (brought to our attention
by [Peter
Tseng](https://www.reddit.com/r/adventofcode/comments/kfb6zx/day_17_getting_to_t6_at_for_higher_spoilerss/ghmllf8))
to avoid the check of the original set in step 2c above: when you iterate over
each point, increment the eight neighbors' map values by *2*, and then increment
the point itself by 1. Then in the final integer under each key, `n / 2` or
`n >> 1` gives you the number of neighbors and `n % 2` (modulo) gives you
whether or not that cell was alive.

``` {.python}
from itertools import islice, product, chain,repeat
from collections import Counter

def mk_neighbs(point):
    """Return neighboring points, with special optimization trick weights

    (1,2)
    => [((1, 2), 1), ((1, 1), 2), ((1, 3), 2), ((0, 2), 2), ((0, 1), 2),
        ((0, 3), 2), ((2, 2), 2), ((2, 1), 2), ((2, 3), 2)
       ]
    """
    neighb_pts = product(*[[x, x-1, x+1] for x in point])
    # associate the original point with +1, and the neighbors with +2
    return zip(neighb_pts, chain([1], repeat(2)))

def step(pts):
    """Takes a set of points (tuples) and steps them in the simulation
    """
    neighbs = Counter()
    for point in pts:
        neighbs += Counter(dict(mk_neighbs(point)))

    def validate(val):
        # the true neighbor count, since we inserted +2 for neighbors
        ncount = val // 2
        # was originally alive if odd, since we inserted +1 for self
        if val % 2 == 1:
            return ncount == 2 or ncount == 3
        else:
            return ncount == 3

    return [p for p, q in neighbs.items() if validate(q)]
```

Three Dimensions
----------------

Let's see how this looks for the 3D case! To make things easier to see, we can
render things in "slices" in 3D space: each grid represents a slice at a
different z level (ie, the z=0 square represents all squares
![\<x,y,0\>](https://latex.codecogs.com/png.latex?%3Cx%2Cy%2C0%3E "<x,y,0>")).
Press "Play" to have the simulation cycle through 6 time steps!

::: {#gol3D}
Please enable Javascript
:::

In "reality", each of those 13 slices above are stacked on top of each other in
3D space. You'll see that most initial conditions will spread out from the
center z=0 point, which means they are actually spreading "up and down" the z
axis.

If you mouse over (or tap) any individual tiny `<x,y>` cell, you'll see the all
of the 26 (![3\^d-1](https://latex.codecogs.com/png.latex?3%5Ed-1 "3^d-1"))
`<x,y,z>` 3D neighbors of the point you're hovering over highlighted in blue ---
these 26 points form a 3D cube around your mouse once everything is stacked
correctly. You can use this cube to help see how the simulation progresses. If
your mouse is hovering over a live cell, and there are 2 or 3 live cells
highlighted in your cube, it'll stay alive in the next time step. If your mouse
is hovering over a dead cell and there are exactly 3 live cells highlighted in
your cube, it will come alive in the next step.

### Axis Reflection Symmetry

Try playing around with different initial conditions to see how they evolve! See
any patterns?

Well, the yellow highlight might have given given things away, but...note that
the entire thing has reflection symmetry across z=0! z=1 is always the same as
z=-1, z=2 is always the same as z=-2, etc. Fundamentally, this is because our
starting solution has z-plane symmetry: the initial 2D slice is symmetric with
reflections across z, because z=0. This is the first "degeneracy" that this blog
post's title is referring to. The negative and positive directions are
interchangeable! This is reflected in the yellow highlight on hover: when you
mouse-over a z square, its corresponding reflected twin is highlighted, and will
always be identical.

This means that we actually only need to simulate *positive* z's...and for our
final answer we just "un-reflect" to get the total number.

This is exactly what freenode IRC user sim642 noticed late into the night of
December 16th:

> I wanted to ask this before but forgot: did anyone try to take advantage of
> the symmetry, e.g. in z axis in part 1? Should halve the amount of
> calculations you have to do.
>
> Only some extra work at the end to differentiate z=0 and z\>0 positions to
> know which to count twice And in part 2 I feel like you could also exploit the
> symmetry in w axis simultaneously

Wow, let's do this! Apparently the picture is slightly more complicated than
simply halving the points. We also need to change how to distribute neighbors.
That's because, once we commit to only keeping the positive z's, some cells need
to be double-counted as neighbors. In particular, any `z=0` cell would
previously had a neighbor at both `z=-1` and `z=1`...but now if we only keep the
positive z's, it would have `z=1` as a neighbor *twice*.

The following interactive element lets you explore what this looks like:

::: {#golSyms3DForward}
Please enable Javascript
:::

Each square represents an entire "slice" of z. When you mouse-over or tap a
z-cell, its z-neighbors are highlighted with how many times that neighbor has to
be counted, and the green bar tells you from what direction that neighborship
arose from. For example, mousing over z=3, z=2 and z=4 get highlighted with the
values "1" because they are neighbors of 3, on the left and right side
(respectively). Note that one neat property for all squares (except for z=6,
which goes off the scale) is that the "total" higher-dimensional neighbors is
always 2
(![3\^(d-2)-1](https://latex.codecogs.com/png.latex?3%5E%28d-2%29-1 "3^(d-2)-1")),
it's just that where those neighbors fall is re-arranged slightly.

The tricky square is now z=0: if you mouse-over it, you'll see that it has a
single neighbor z=1 that is counted twice, as a neighbor from both the left and
right side.

We can compute the above diagram by expanding z=0 to its neighbors (z=-1, and
z=1), applying the absolute value function, and seeing how points double-up.
This gives us the "forward neighbors", and we can directly use it for the
original "keep the full array" GoL implementation.

However, for the "keep active points and expand their neighbors" GoL
implementation...we have to find the opposite. Remember that to build our
"neighbors map" (the map of points to how many active neighbors they have), we
have each cell "pro-actively" add its contributions to all of its neighbors.
`<1,2,3>` is a neighbor to `<1,3,4>` once, so when we expand `<1,2,3>` we would
increment the value in the map at `<1,3,4>` by 1 because `<1,2,3>` is a neighbor
of `<1,3,4>` once.

Now, how do we count `<1,3,1>` expanding into `<1,3,0>`? Well, normally,
`<1,3,1>` is a neighbor of `<1,3,0>` once. However, if we only keep the
normalized z values, `<1,3,1>` is a neighbor of `<1,3,0>`...twice! To compute
the total neighbor count of `<1,3,0>`, we have to count the contribution from
`<1,3,1>` twice (once for `<1,3,1>` and once for `<1,3,-1>`, which was
normalized away).

That means we have to follow the rules in the previous element *backwards*,
like:

::: {#golSyms3DReverse}
Please enable Javascript
:::

These are the "reverse neighbors": how much times a given point counts as a
neighbor for its surrounding points. Here, mousing over z=1 shows that it counts
as a neighbor for z=0 twice, from both the left and the right. It also counts as
a neighbor for z=2 once (from the left side).

We can account for this by hard-coding the rules into our step algorithm: if our
z goes from `1` to `0`, increment its value twice in the neighbor map.
Otherwise, simply increment by 1 as normal.

This rule is relatively easy to implement, and as a result we now halved our
total number of points we need to keep and check for 3D! It's also simple enough
to generalize (just do the `1 -> 0` check for every "higher dimension" and
double its contribution for each `1 -> 0` transition is seen)...and that means
we reduce the number of 4D points we need to track by a factor of four, the
number of 5D points by a factor of eight, the number of 6D points by a factor of
16... now our total points to check only grows as
![O(n\^d / 2\^{d-2})](https://latex.codecogs.com/png.latex?O%28n%5Ed%20%2F%202%5E%7Bd-2%7D%29 "O(n^d / 2^{d-2})")
instead of
![O(n\^d)](https://latex.codecogs.com/png.latex?O%28n%5Ed%29 "O(n^d)")!

This discovery late in the night of December 16th was what inspired us to
believe and dream that more breakthroughs might be possible to bring things down
even further.

And those breakthroughs soon came...

Four Dimensions
---------------

Let's look at how the 4 dimensions works! We can visualize this by taking "z-w"
slices at different x-y planes as well. The labels in these boxes are the
`<z,w>` of each slice. The very center is `<z,w> = <0,0>` the row in the middle
from the top is `w=0`, and the column in the very middle from the left is `z=0`.
It's basically taking the 3D visualization above and expanding it in an extra
dimension. Press "Play" to run your initial conditions!

::: {#gol4D}
Please enable Javascript
:::

We get something interesting as well: most initial conditions will spread out
from the center `<z,w> = <0,0>` point radially, spreading outwards into positive
and negative z and w. Mouse-over or tap any individual tiny `<x.y>` cell and
you'll see each of its 80
(![3\^d-1](https://latex.codecogs.com/png.latex?3%5Ed-1 "3^d-1")) `<x,y,z,w>` 4D
neighbors highlighted in blue, forming a little 3x3x3 "tesseract" (4D cube, or
hypercube). Like in the 3D case, you can use this little hypercube to track how
the simulation progresses: if your mouse if hovering over a live cell with 2 or
3 live cells in its hypercube, it'll stay alive in the next step, if it's
hovering over a dead cell with 3 live cells in its hypercube, it'll come alive
in the next step.

### Diagonal Reflection Symmetry

Play around and explore how simulations evolve! You will notice that the axis
reflection symmetry is still preserved, but four ways (the slice at
`<z,w> = <3,4>` is always going to be identical to the slice at `<-3,4>`,
`<3,-4>`, and `<-3,-4>`). These are reflected in the "deep yellow" highlights
above when you mouse over a zw square. (Ignore the lighter yellow highlights for
now!)

And now, for the next big breakthrough. I think this situation shows the power
of visualization well, because this exact visualization was what reddit user
*u/cetttbycett* was looking at when [they made this
post](https://www.reddit.com/r/adventofcode/comments/kfjhwh/year_2020_day_17_part_2_using_symmetry_in_4d_space/)...and
everything changed forever.

> I noticed that the expansion of active cubes for part 2 is symmetric with
> respect to two hyperplanes in 4d space: These hyperplanes can be described by
> w = 0 and w-z = 0.
>
> Using these symmetries could make the code nearly eight times as fast.I was
> wondering if anyone tried that.

What *u/cetttbycettt* saw is what you can see now in the element above: it's all
of the *light yellow* highlighted squares when you mouse-over. In addition to
the z=0 and w=0 lines (the two lines down the middle, up-down and left-right),
we also have another line of symmetry: z=w and w=z, the diagonal lines!

That's right, a zw slice at `<z,w>=<3,4>` is *identical* to the one at `<4,3>`,
and so also `<-3,4>`, `<3,-4>`, `<-3,-4>`, `<-4,3>`, `<4,-3>`, and `<-4,-3>`!
Each slice is potentially repeated *eight* times! The exceptions are the points
on the lines of symmetry themselves, which are each repeated four times, and
also `<z,w>=<0,0>`, which is in its own class.

So, our first breakthrough meant that we only have to simulate *positive*
coordinates (a single quadrant)...our next breakthrough means that we only have
to simulate coordinates on a single "wedge" half-quadrant...and then duplicate
those eight times at the end.

Arbitrarily, let's say we only simulate the points `<z,w>` where both components
are positive and in non-decreasing order. So, `<4,-3>` gets "normalized" to
`<3,4>`.

Okay, so we found a new symmetry...but we ran into the same issue as before. How
do we propagate neighbors? To help us, see what's going on, let's look at the
map of neighbors between different `<z,w>` squares, for the single zw wedge we
are simulating.

::: {#golSyms4DForward}
Please enable Javascript
:::

These are the "forward neighbors"; we can compute them by expanding a point to
its neighbors, and then normalizing our points and seeing how they double (or
quadruple) up. For example, mouse over `<z,w>=<3,3>` and see it has eight total
higher-dimensional neighbors (like all points should, though this visualization
leaves out points at w\>6). It's *supposed* to have a neighbor at `<4,3>`, but
that gets reflected back onto `<3,4>` during our normalization process, so you
see that the point `<3,3>` has a neighbor at `<3,4>` "double-counted". The green
squares (in the north and west positions) at `<3,4>` when you hover over `<3,3>`
show that `<3,4>` is a neighbor of `<3,3>` both to its north and to its west.

Also, we have something really odd show up for the first time. Mouse over a
point like `<z,w>=<2,3>` and see that it has a neighbor in...itself? What's
going on here? Well, it is *supposed* to have a neighbor at `<3,2>` but that
gets normalized/reflected back onto `<2,3>` --- it reflects onto itself! The
green square in the Southeast means that `<2,3>`'s southeast neighbor
is...itself!

The "forward neighbors" are useful for understanding what's going on, but to
actually run our simulation we again need to find the "reverse neighbors": from
a given point A, how many times is that point a neighbor of another point B?

We can compute this in brute-force using a cache: iterate over each point,
expand all its neighbors
![a\_i](https://latex.codecogs.com/png.latex?a_i "a_i"), normalize that
neighbor, and then set ![a\_i](https://latex.codecogs.com/png.latex?a_i "a_i")
in the cache to the multiplicity after normalization. But this is pretty
expensive to do in the general case, so we'd like to maybe find a formula to be
able to do this using mathematical operations. So, let's explore!

::: {#golSyms4DReverse}
Please enable Javascript
:::

After exploring this interactively, we can maybe think of some rules we can
apply.

1.  If we have a point `<z,z>` directly on the z=w diagonal, just use its five
    normal left/up neighbors with weight 1 each.
2.  If we have a point `<z,z+1>` on the "inner-er" diagonal, use its five normal
    left/up neighbors with weight 1, but its south and west neighbors have
    weight 2, and the point reflects onto *itself* with weight 1.
3.  If we're on `z=1` and we move into `z=0`, double that count (phew, the same
    rule as in the 3D case earlier)
4.  If we're on w=1 and we move into w=0, double that count (same as before)
5.  And...I guess `<0,1>` reflects onto itself *twice*? I guess that technically
    falls under a combination of rule 2 and rule 4, but we don't directly
    observe the motion into w=0 before it gets reflected so it has to be
    special-cased.

Okay, those rules are *sliiightly* more complicated than our 3D rules ("if we go
from z=1 to z=0, double-count it")...but they're at least mechanical enough to
code in, even if not beautiful. You can probably foresee that it might be tough
to generalize, but...we'll tackle that when we get there :)

For now, we have a super-fast implementation of 4D GoL with our special
degeneracy! The runtime gets reduced by a factor of 8!

Now, onward to 5D!

Five Dimensions
---------------

::: {#golSyms5D}
Please enable Javascript
:::

Tackling the Neighbor Problem
-----------------------------

::: {#golTreeForward}
Please enable Javascript
:::

::: {#golTreeReverse}
Please enable Javascript
:::

Arbitrary Dimensions
--------------------

::: {#golFlat}
Please enable Javascript
:::

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
