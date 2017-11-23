Hamiltonian Dynamics in Haskell
===============================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/hamiltonian-dynamics-in-haskell.html)

As promised in my [*hamilton* introduction
post](https://blog.jle.im/entry/introducing-the-hamilton-library.html), I’m
going to go over implementing of the
*[hamilton](http://hackage.haskell.org/package/hamilton)* library using
*[ad](http://hackage.haskell.org/package/ad)* and dependent types.

This post will be a bit heavy in some mathematics and Haskell concepts. The
expected audience is intermediate Haskell programmers, and no previous knowledge
of dependent types is expected.

The mathematics and physics are “extra” flavor text and could potentially be
skipped, but you’ll get the most out of this article if you have basic
familiarity with:

1.  Basic concepts of multivariable calculus (like partial and total
    derivatives).
2.  Concepts of linear algebra (like dot products, matrix multiplication, and
    matrix inverses)

No physics knowledge is assumed, but knowing a little bit of first semester
physics would help you gain a bit more of an appreciation for the end result!

The Goal
--------

At the end of this, we should be able to have Haskell *automatically generate*
**equations of motions** for any arbitrary system described in arbitrary
coordinate systems, and simulate that system.

Normally, we’d describe a system using particles’ x and y coordinates, but our
goal is to be able to describe our particles’ positions using any coordinate
system we want (polar, distance-along-a-curved-rail, pendulum-angles, etc.) and
have Haskell automatically generate equations of motions and time progressions
of those coordinates!

Read [my hamilton library
introduction](https://blog.jle.im/entry/introducing-the-hamilton-library.html)
for more information and examples!

All of the code in this project can be found [hosted
here](https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs),
and executable as a stack script (just exec `./Hamilton.hs`!). It was tested on
the [Stackage LTS-9.14](https://www.stackage.org/lts-9.14) resolver.

Hamiltonian Mechanics
---------------------

As mentioned in the previous post, Hamiltonian mechanics is a re-imagining of
dynamics and mechanics (think “the world
post-![F = m a](https://latex.codecogs.com/png.latex?F%20%3D%20m%20a "F = m a")”)
that not only opened up new doors to solving problems in classical, but also
ended up being the right angle of viewing the world to unlock statistical
mechanics and thermodynamics, and later even quantum mechanics.

Hamiltonian mechanics lets you parameterize your system’s “position” in
arbitrary ways (like the angle of rotation, for pendulum problems) and then
posits that the full state of the system exists in something called *phase
space*, and that the system’s dynamics is its motion through phase space that is
dictated by the geometry of the *Hamiltonian* of that phase space.

The system’s *Hamiltonian* is a
![\\mathbb{R}\^{2n} \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5E%7B2n%7D%20%5Crightarrow%20%5Cmathbb%7BR%7D "\mathbb{R}^{2n} \rightarrow \mathbb{R}")
function on phase space (where ![n](https://latex.codecogs.com/png.latex?n "n")
is the number of coordinates parameterizing your system) to
![\\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D "\mathbb{R}").
For a time-independent system, the picture of the dynamics is pretty simple: the
system moves along the *contour lines* of the *Hamiltonian* – the lines of equal
“height”.

![Example of contour lines of a
![\\mathbb{R}\^2 \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5E2%20%5Crightarrow%20%5Cmathbb%7BR%7D "\mathbb{R}^2 \rightarrow \mathbb{R}")
function – the elevation of land. From the [Ordinace
Survey](https://www.ordnancesurvey.co.uk/blog/2015/11/map-reading-skills-making-sense-of-contour-lines/)
website.](/img/entries/hamilton/contour-lines.jpg "Contour lines")

In the example above, if we imagine that phase space is the 2D location, then
the *Hamiltonian* is the mountain. And for a system dropped anywhere on the
mountain, its motion would be along the contour lines. For example, if a system
started somewhere along the 10 contour line, it would begin to oscillate the
entire phase space along the 10 contour line.[^1]

*Every* [smooth](https://www.youtube.com/watch?v=izGwDsrQ1eQ)
![\\mathbb{R}\^{2n} \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5E%7B2n%7D%20%5Crightarrow%20%5Cmathbb%7BR%7D "\mathbb{R}^{2n} \rightarrow \mathbb{R}")
function on phase space can be used as a Hamiltonian to describe the physics of
some system. So, given any “mountain range” on phase space, any “elevation map”
or real-valued function on phase space, you can treat it as a description of the
dynamics of some physical system.

The *trick*, then, to using Hamiltonian dynamics to model your system, is:

1.  Finding the phase space to describe your system. This can be done based on
    any continuous parameterization of your system (“generalized coordinates”),
    like angles of pendulums and so on.

2.  Finding the Hamiltonian on that phase space to describe your system.

And then Hamilton’s dynamics will give you the rest! All you do is “follow the
contour lines” on that Hamiltonian!

### Phase Space

The only thing I’ve really said in detail about phase space is that if your
system’s state has ![n](https://latex.codecogs.com/png.latex?n "n") parameters,
then the corresponding phase space is
![2n](https://latex.codecogs.com/png.latex?2n "2n")-dimensional (and that
Hamiltonian mechanics is somehow about systems moving around in phase space).
Let me clear it up now: *Phase space* is a
![2n](https://latex.codecogs.com/png.latex?2n "2n")-dimensional space
parameterized by:

1.  All of the current values of the
    ![n](https://latex.codecogs.com/png.latex?n "n") parameters (“generalized
    coordinates”)
2.  All of the current “generalized momenta” of those
    ![n](https://latex.codecogs.com/png.latex?n "n") parameters

So if you were parameterizing your pendulum system by, say, the angle of the
pendulum, the phase space would be the current angle of the pendulum along with
the current “generalized momentum” associated with the angle of the pendulum.
What exactly *is* generalized momentum? We’ll go over calculating it eventually,
but what does it represent…*physically*?

I could give you some spiel here about the underlying Lie algebra of the Lie
group associated with the generalized coordinates, but that would make this a
completely different post. What I *can* say is that the generalized momenta
associated with (“conjugate to”) certain sets of familiar coordinates yield
things that we typically call “momenta”:

1.  The momentum conjugate to normal Cartesian coordinates is just our normal
    run-of-the-mill *linear momentum* (in the
    ![\\mathbf{p} = m \\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D%20%3D%20m%20%5Cmathbf%7Bv%7D "\mathbf{p} = m \mathbf{v}"))
    from first semester physics.

2.  The momentum conjugate to the angle
    ![\\theta](https://latex.codecogs.com/png.latex?%5Ctheta "\theta") in polar
    coordinates is *angular momentum*
    (![\\mathbf{L} = m \\mathbf{r} \\times \\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7BL%7D%20%3D%20m%20%5Cmathbf%7Br%7D%20%5Ctimes%20%5Cmathbf%7Bv%7D "\mathbf{L} = m \mathbf{r} \times \mathbf{v}"),
    or
    ![L = m r\^2 \\dot{\\theta}](https://latex.codecogs.com/png.latex?L%20%3D%20m%20r%5E2%20%5Cdot%7B%5Ctheta%7D "L = m r^2 \dot{\theta}"))
    from first semester physics.

3.  The momentum conjugate to the radial coordinate
    ![r](https://latex.codecogs.com/png.latex?r "r") in polar coordinates is
    also just boring old linear momentum
    ![p\_r = m \\dot{r}](https://latex.codecogs.com/png.latex?p_r%20%3D%20m%20%5Cdot%7Br%7D "p_r = m \dot{r}").

So, it’s our normal momentum (for linear and polar coordinates) *generalized* to
arbitrary coordinates.

### Hamiltonian Dynamics

I’ve explained Hamiltonian dynamics for time-independent Hamiltonians as “follow
the contour lines”. If you remember your basic multi-variable calculus course,
you’ll know that the line of “steepest ascent” is the gradient. If we call the
Hamiltonian
![\\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "\mathcal{H}(\mathbf{q},\mathbf{p})")
(where
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
is the vector of positions and
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}")
is the vector of momenta), then the direction of steepest ascent is

![
\\left \\langle \\frac{\\partial}{\\partial \\mathbf{q}}
\\mathcal{H}(\\mathbf{q},\\mathbf{p}), \\frac{\\partial}{\\partial \\mathbf{p}}
\\mathcal{H}(\\mathbf{q},\\mathbf{p}) \\right \\rangle
](https://latex.codecogs.com/png.latex?%0A%5Cleft%20%5Clangle%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cmathbf%7Bq%7D%7D%0A%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%2C%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cmathbf%7Bp%7D%7D%0A%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%5Cright%20%5Crangle%0A "
\left \langle \frac{\partial}{\partial \mathbf{q}}
\mathcal{H}(\mathbf{q},\mathbf{p}), \frac{\partial}{\partial \mathbf{p}}
\mathcal{H}(\mathbf{q},\mathbf{p}) \right \rangle
")

But we want to move along the *contour lines*…and these are the lines
*perpendicular* to the direction of steepest descent. The vector perpendicular
to
![\\langle x, y \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20x%2C%20y%20%5Crangle "\langle x, y \rangle")
is
![\\langle y, -x \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20y%2C%20-x%20%5Crangle "\langle y, -x \rangle"),[^2]
so we just derived the actual Hamiltonian equations of motion: just move in the
direction perpendicular to the steepest ascent!

![
\\begin{aligned}
\\dot{q} & = \\frac{\\partial}{\\partial p\_q} \\mathcal{H}(\\mathbf{q},\\mathbf{p}) \\\\
\\dot{p}\_q & = - \\frac{\\partial}{\\partial q} \\mathcal{H}(\\mathbf{q},\\mathbf{p})
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Cdot%7Bq%7D%20%26%20%3D%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20p_q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%5C%5C%0A%5Cdot%7Bp%7D_q%20%26%20%3D%20-%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\dot{q} & = \frac{\partial}{\partial p_q} \mathcal{H}(\mathbf{q},\mathbf{p}) \\
\dot{p}_q & = - \frac{\partial}{\partial q} \mathcal{H}(\mathbf{q},\mathbf{p})
\end{aligned}
")

Which holds for every generalized coordinate
![q](https://latex.codecogs.com/png.latex?q "q"), where
![p\_q](https://latex.codecogs.com/png.latex?p_q "p_q") is the momentum
conjugate to that coordinate. (For the rest of this post,
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
refers to the vector of coordinates,
![q](https://latex.codecogs.com/png.latex?q "q") refers to a single specific
coordinate, and ![p\_q](https://latex.codecogs.com/png.latex?p_q "p_q") refers
to the momentum conjugate to that coordinate).

Essentially, these give you “updating functions” for
![q](https://latex.codecogs.com/png.latex?q "q") and
![p\_q](https://latex.codecogs.com/png.latex?p_q "p_q") – given
![\\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "\mathcal{H}(\mathbf{q},\mathbf{p})"),
you have a way to “update” the particle’s position in phase space. Just take the
partial derivatives of
![\\mathcal{H}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D "\mathcal{H}")
at every step in time! To update
![q](https://latex.codecogs.com/png.latex?q "q"), nudge it by
![\\frac{\\partial}{\\partial p\_q} \\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20p_q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "\frac{\partial}{\partial p_q} \mathcal{H}(\mathbf{q},\mathbf{p})").
To update ![p\_q](https://latex.codecogs.com/png.latex?p_q "p_q"), nudge it by
![-\\frac{\\partial}{\\partial q} \\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?-%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "-\frac{\partial}{\partial q} \mathcal{H}(\mathbf{q},\mathbf{p})")!

This picture is appealing to me in a visceral way because it sort of seems like
the system is “surfing” along the Hamiltonian’s contour lines. It’s being
“pushed” *faster* when the Hamiltonian is steeper, and slower when it’s more
shallow. I can apply all my intuition as a surfer[^3] to Hamiltonian mechanics!

Hamiltonian Dynamics and Physical Systems
-----------------------------------------

Earlier I mentioned that the two steps for applying Hamiltonian mechanics to
your system was figuring out your system’s conjugate momenta and the appropriate
Hamiltonian. To explain this, I’m going to make a couple of simplifying
assumptions that make the job easier for the purposes of this article:

1.  Your coordinates and potential energy are time-independent.
2.  Your potential energy function only depends on *positions*, and not
    *velocities*. (So nothing like friction or wind resistance or magnetic field
    vector potentials)

With these assumptions, I’m going to skip over discussing the
[Lagrangian](https://en.wikipedia.org/wiki/Lagrangian_mechanics) of the system,
which is the traditional way to do this. You can think of this section as me
presenting derived conclusions and skipping the derivations.

### Conjugate Momenta

For systems with velocity-independent potential energies, it can be shown that
the momentum conjugate to coordinate
![q](https://latex.codecogs.com/png.latex?q "q") is

![
p\_q = \\frac{\\partial}{\\partial \\dot{q}} KE(\\mathbf{q}, \\dot{\\mathbf{q}})
](https://latex.codecogs.com/png.latex?%0Ap_q%20%3D%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cdot%7Bq%7D%7D%20KE%28%5Cmathbf%7Bq%7D%2C%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%29%0A "
p_q = \frac{\partial}{\partial \dot{q}} KE(\mathbf{q}, \dot{\mathbf{q}})
")

Where
![KE(\\mathbf{q},\\dot{\\mathbf{q}})](https://latex.codecogs.com/png.latex?KE%28%5Cmathbf%7Bq%7D%2C%5Cdot%7B%5Cmathbf%7Bq%7D%7D%29 "KE(\mathbf{q},\dot{\mathbf{q}})")
is the kinetic energy of the system, which is a function on the coordinates
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
and their rates of change,
![\\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\dot{\mathbf{q}}").
For example, for normal Cartesian coordinates in one dimension,
![KE(x, \\dot{x}) = \\frac{1}{2} m \\dot{x}\^2](https://latex.codecogs.com/png.latex?KE%28x%2C%20%5Cdot%7Bx%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20m%20%5Cdot%7Bx%7D%5E2 "KE(x, \dot{x}) = \frac{1}{2} m \dot{x}^2").
So the momentum conjugate to ![x](https://latex.codecogs.com/png.latex?x "x")
is:

![
p\_x = \\frac{\\partial}{\\partial \\dot{x}} \\left\[ \\frac{1}{2} m \\dot{x}\^2 \\right\] = m \\dot{x}
](https://latex.codecogs.com/png.latex?%0Ap_x%20%3D%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cdot%7Bx%7D%7D%20%5Cleft%5B%20%5Cfrac%7B1%7D%7B2%7D%20m%20%5Cdot%7Bx%7D%5E2%20%5Cright%5D%20%3D%20m%20%5Cdot%7Bx%7D%0A "
p_x = \frac{\partial}{\partial \dot{x}} \left[ \frac{1}{2} m \dot{x}^2 \right] = m \dot{x}
")

Just linear momentum, like I claimed before.

Alright, now let’s generalize this to arbitrary coordinates. In general, for
*Cartesian* coordinates, the kinetic energy will always be

![
KE(\\mathbf{x}, \\dot{\\mathbf{x}}) = \\frac{1}{2} \\left\[ m\_1 \\dot{x}\_1\^2 + m\_2 \\dot{x}\_2\^2 + m\_3 \\dot{x}\_3\^2 + \\dots \\right\]
](https://latex.codecogs.com/png.latex?%0AKE%28%5Cmathbf%7Bx%7D%2C%20%5Cdot%7B%5Cmathbf%7Bx%7D%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cleft%5B%20m_1%20%5Cdot%7Bx%7D_1%5E2%20%2B%20m_2%20%5Cdot%7Bx%7D_2%5E2%20%2B%20m_3%20%5Cdot%7Bx%7D_3%5E2%20%2B%20%5Cdots%20%5Cright%5D%0A "
KE(\mathbf{x}, \dot{\mathbf{x}}) = \frac{1}{2} \left[ m_1 \dot{x}_1^2 + m_2 \dot{x}_2^2 + m_3 \dot{x}_3^2 + \dots \right]
")

Where ![m](https://latex.codecogs.com/png.latex?m "m") is the inertia associated
with each coordinate…for example, if
![\\langle x\_1, x\_2 \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20x_1%2C%20x_2%20%5Crangle "\langle x_1, x_2 \rangle")
describes the location of an object of mass
![m](https://latex.codecogs.com/png.latex?m "m"), then
![m\_1 = m\_2 = m](https://latex.codecogs.com/png.latex?m_1%20%3D%20m_2%20%3D%20m "m_1 = m_2 = m").

To make things more convenient, we’ll treat this as a quadratic form over an
inertia matrix:

![
KE(\\dot{\\mathbf{x}}) = \\frac{1}{2} \\dot{\\mathbf{x}}\^T \\hat{M} \\dot{\\mathbf{x}}
](https://latex.codecogs.com/png.latex?%0AKE%28%5Cdot%7B%5Cmathbf%7Bx%7D%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bx%7D%7D%5ET%20%5Chat%7BM%7D%20%5Cdot%7B%5Cmathbf%7Bx%7D%7D%0A "
KE(\dot{\mathbf{x}}) = \frac{1}{2} \dot{\mathbf{x}}^T \hat{M} \dot{\mathbf{x}}
")

Where ![\\hat{M}](https://latex.codecogs.com/png.latex?%5Chat%7BM%7D "\hat{M}")
is the [diagonal matrix](https://en.wikipedia.org/wiki/Diagonal_matrix) whose
entries are the masses of each coordinate, and
![\\dot{\\mathbf{x}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bx%7D%7D "\dot{\mathbf{x}}")
is the column vector of all of the (Cartesian) coordinates,
![\\left\[ \\dot{x}\_1\\, \\dot{x}\_2\\, \\dot{x}\_3\\, \\dots \\right\]\^T](https://latex.codecogs.com/png.latex?%5Cleft%5B%20%5Cdot%7Bx%7D_1%5C%2C%20%5Cdot%7Bx%7D_2%5C%2C%20%5Cdot%7Bx%7D_3%5C%2C%20%5Cdots%20%5Cright%5D%5ET "\left[ \dot{x}_1\, \dot{x}_2\, \dot{x}_3\, \dots \right]^T").

Now! How to generalize this to arbitrary coordinates? Well, if we have
![n](https://latex.codecogs.com/png.latex?n "n") generalized coordinates
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
mapping to ![m](https://latex.codecogs.com/png.latex?m "m")-dimensional
Cartesian coordinates, we can specify them as
![\\mathbf{x} = f(\\mathbf{q})](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bx%7D%20%3D%20f%28%5Cmathbf%7Bq%7D%29 "\mathbf{x} = f(\mathbf{q})"),
where
![f : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^m](https://latex.codecogs.com/png.latex?f%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5Em "f : \mathbb{R}^n \rightarrow \mathbb{R}^m"),
taking the vector of generalized coordinates and returning a vector for the
position in Cartesian space. For example, for polar coordinates,
![f(r, \\theta) = \\left \\langle r \\cos(\\theta), r \\sin(\\theta) \\right \\rangle](https://latex.codecogs.com/png.latex?f%28r%2C%20%5Ctheta%29%20%3D%20%5Cleft%20%5Clangle%20r%20%5Ccos%28%5Ctheta%29%2C%20r%20%5Csin%28%5Ctheta%29%20%5Cright%20%5Crangle "f(r, \theta) = \left \langle r \cos(\theta), r \sin(\theta) \right \rangle"),
because, for polar coordinates,
![x = r \\cos(\\theta)](https://latex.codecogs.com/png.latex?x%20%3D%20r%20%5Ccos%28%5Ctheta%29 "x = r \cos(\theta)")
and
![y = r \\sin(\\theta)](https://latex.codecogs.com/png.latex?y%20%3D%20r%20%5Csin%28%5Ctheta%29 "y = r \sin(\theta)").

So we can get
![\\mathbf{x}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bx%7D "\mathbf{x}")
from
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
with ![f](https://latex.codecogs.com/png.latex?f "f"), but how can we get
![\\dot{\\mathbf{x}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bx%7D%7D "\dot{\mathbf{x}}"),
the vector of rate of changes? Well, if
![x\_1 = f\_1(q\_1, q\_2, q\_3 \\dots)](https://latex.codecogs.com/png.latex?x_1%20%3D%20f_1%28q_1%2C%20q_2%2C%20q_3%20%5Cdots%29 "x_1 = f_1(q_1, q_2, q_3 \dots)"),
then the
![\\dot{x}\_1](https://latex.codecogs.com/png.latex?%5Cdot%7Bx%7D_1 "\dot{x}_1")
is the [total derivative](https://en.wikipedia.org/wiki/Total_derivative) of
![x\_1](https://latex.codecogs.com/png.latex?x_1 "x_1") with respect to time:

![
\\dot{x}\_1 = \\frac{\\partial f\_1}{\\partial q\_1} \\dot{q}\_1 +
    \\frac{\\partial f\_1}{\\partial q\_2} \\dot{q}\_2 +
    \\frac{\\partial f\_1}{\\partial q\_3} \\dot{q}\_3 + \\dots
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7Bx%7D_1%20%3D%20%5Cfrac%7B%5Cpartial%20f_1%7D%7B%5Cpartial%20q_1%7D%20%5Cdot%7Bq%7D_1%20%2B%0A%20%20%20%20%5Cfrac%7B%5Cpartial%20f_1%7D%7B%5Cpartial%20q_2%7D%20%5Cdot%7Bq%7D_2%20%2B%0A%20%20%20%20%5Cfrac%7B%5Cpartial%20f_1%7D%7B%5Cpartial%20q_3%7D%20%5Cdot%7Bq%7D_3%20%2B%20%5Cdots%0A "
\dot{x}_1 = \frac{\partial f_1}{\partial q_1} \dot{q}_1 +
    \frac{\partial f_1}{\partial q_2} \dot{q}_2 +
    \frac{\partial f_1}{\partial q_3} \dot{q}_3 + \dots
")

Or, in short:

![
\\dot{x}\_i = \\sum\_{j = 1}\^n \\frac{\\partial f\_i}{\\partial q\_j} \\dot{q}\_j
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7Bx%7D_i%20%3D%20%5Csum_%7Bj%20%3D%201%7D%5En%20%5Cfrac%7B%5Cpartial%20f_i%7D%7B%5Cpartial%20q_j%7D%20%5Cdot%7Bq%7D_j%0A "
\dot{x}_i = \sum_{j = 1}^n \frac{\partial f_i}{\partial q_j} \dot{q}_j
")

But, hey, this looks a lot like a matrix multiplication. If we call
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f")
the [Jacobian
matrix](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant), the
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrix of partial derivatives of
![f](https://latex.codecogs.com/png.latex?f "f")
(![\\hat{J}\_{fij} = \\frac{\\partial f\_i}{\\partial q\_j}](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_%7Bfij%7D%20%3D%20%5Cfrac%7B%5Cpartial%20f_i%7D%7B%5Cpartial%20q_j%7D "\hat{J}_{fij} = \frac{\partial f_i}{\partial q_j}"))
at a given point, then we have a nice expression for
![\\dot{\\mathbf{x}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bx%7D%7D "\dot{\mathbf{x}}"):

![
\\dot{\\mathbf{x}} = \\hat{J}\_f \\dot{\\mathbf{q}}
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7B%5Cmathbf%7Bx%7D%7D%20%3D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A "
\dot{\mathbf{x}} = \hat{J}_f \dot{\mathbf{q}}
")

And we can plug it in (remembering that
![(A B)\^T = B\^T A\^T](https://latex.codecogs.com/png.latex?%28A%20B%29%5ET%20%3D%20B%5ET%20A%5ET "(A B)^T = B^T A^T"))
to our kinetic energy equation to get:

![
KE(\\mathbf{q},\\dot{\\mathbf{q}}) = \\frac{1}{2} \\dot{\\mathbf{q}}\^T \\hat{J}\_f\^T
    \\hat{M} \\hat{J}\_f \\dot{\\mathbf{q}}
](https://latex.codecogs.com/png.latex?%0AKE%28%5Cmathbf%7Bq%7D%2C%5Cdot%7B%5Cmathbf%7Bq%7D%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%5ET%20%5Chat%7BJ%7D_f%5ET%0A%20%20%20%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A "
KE(\mathbf{q},\dot{\mathbf{q}}) = \frac{1}{2} \dot{\mathbf{q}}^T \hat{J}_f^T
    \hat{M} \hat{J}_f \dot{\mathbf{q}}
")

And for the final step, we differentiate with respect to the
![\\dot{q}](https://latex.codecogs.com/png.latex?%5Cdot%7Bq%7D "\dot{q}")s
(which is just the gradient
![\\nabla\_{\\dot{\\mathbf{q}}}](https://latex.codecogs.com/png.latex?%5Cnabla_%7B%5Cdot%7B%5Cmathbf%7Bq%7D%7D%7D "\nabla_{\dot{\mathbf{q}}}"))
to get
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}"),
the vector of conjugate momenta:

![
\\mathbf{p} = \\nabla\_{\\dot{\\mathbf{q}}} \\left\[
    \\frac{1}{2} \\dot{\\mathbf{q}}\^T \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\dot{\\mathbf{q}}
  \\right\]
  = \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\dot{\\mathbf{q}}
](https://latex.codecogs.com/png.latex?%0A%5Cmathbf%7Bp%7D%20%3D%20%5Cnabla_%7B%5Cdot%7B%5Cmathbf%7Bq%7D%7D%7D%20%5Cleft%5B%0A%20%20%20%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%5ET%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A%20%20%5Cright%5D%0A%20%20%3D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A "
\mathbf{p} = \nabla_{\dot{\mathbf{q}}} \left[
    \frac{1}{2} \dot{\mathbf{q}}^T \hat{J}_f^T \hat{M} \hat{J}_f \dot{\mathbf{q}}
  \right]
  = \hat{J}_f^T \hat{M} \hat{J}_f \dot{\mathbf{q}}
")

Now, we’re going to be using
![\\hat{J}\_f\^T \\hat{M} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f "\hat{J}_f^T \hat{M} \hat{J}_f")
a lot, so let’s give it a name,
![\\hat{K}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D "\hat{K}"). If
the masses are all positive and
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f")
is full-rank[^4], then
![\\hat{K}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D "\hat{K}") is a
symmetric, positive-definite, invertible matrix (by construction). It’s
important to also remember that it’s an explicit function of
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}"),
because
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f")
is a matrix of partial derivatives at a given
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}").
We now have a simple expression for the vector of conjugate momenta
(![\\mathbf{p} = \\hat{K} \\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D%20%3D%20%5Chat%7BK%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\mathbf{p} = \hat{K} \dot{\mathbf{q}}")),
and also for kinetic energy
(![KE = \\frac{1}{2} \\dot{\\mathbf{q}}\^T \\hat{K} \\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?KE%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%5ET%20%5Chat%7BK%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D "KE = \frac{1}{2} \dot{\mathbf{q}}^T \hat{K} \dot{\mathbf{q}}")).

It’s going to be important for us to also be able to go backwards (to get
![\\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\dot{\mathbf{q}}")
from
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}")).
Luckily, because we wrote the whole thing as a matrix operation, going backwards
is easy – just take the matrix inverse, which we know exists!

![
\\dot{\\mathbf{q}} = \\hat{K}\^{-1} \\mathbf{p}
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7B%5Cmathbf%7Bq%7D%7D%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A "
\dot{\mathbf{q}} = \hat{K}^{-1} \mathbf{p}
")

The power of linear algebra!

### Hamiltonians of Physical Systems

Ok, that’s step one. How about step two – finding the Hamiltonian for your
system?

The *real* Hamiltonian is actually the [Poisson
bracket](https://en.wikipedia.org/wiki/Poisson_bracket) of the system’s
[Lagrangian](https://en.wikipedia.org/wiki/Lagrangian_mechanics), but I did some
of the work for you for the case of time-independent coordinates where the
potential energy depends *only* on positions (so, no friction, wind resistance,
etc.), the Hamiltonian of a system is precisely the system’s total [mechanical
energy](https://en.wikipedia.org/wiki/Mechanical_energy), or its kinetic energy
plus the potential energy:

![
\\mathcal{H}(\\mathbf{q},\\mathbf{p}) = KE(\\mathbf{q},\\mathbf{p}) + PE(\\mathbf{q})
](https://latex.codecogs.com/png.latex?%0A%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%3D%20KE%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%2B%20PE%28%5Cmathbf%7Bq%7D%29%0A "
\mathcal{H}(\mathbf{q},\mathbf{p}) = KE(\mathbf{q},\mathbf{p}) + PE(\mathbf{q})
")

Which makes a lot of intuitive sense, because you might recall that total
mechanical energy is always conserved for certain types of systems.
Incidentally, Hamiltonian dynamics makes sure that the value of the system’s
Hamiltonian stays the same (because it moves along contour lines). So, the
system’s Hamiltonian always stays the same, and so its total mechanical energy
stays the same, as well! Energy is conserved because the Hamiltonian stays the
same!

Anyway, we want to build our system’s Hamiltonian from properties of the
coordinate system, so plugging in our expression for
![KE](https://latex.codecogs.com/png.latex?KE "KE"), we get
![\\mathcal{H}(\\mathbf{q},\\dot{\\mathbf{q}}) = \\frac{1}{2} \\dot{\\mathbf{q}}\^T \\hat{K} \\dot{\\mathbf{q}} + PE(\\mathbf{q})](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cdot%7B%5Cmathbf%7Bq%7D%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%5ET%20%5Chat%7BK%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%20%2B%20PE%28%5Cmathbf%7Bq%7D%29 "\mathcal{H}(\mathbf{q},\dot{\mathbf{q}}) = \frac{1}{2} \dot{\mathbf{q}}^T \hat{K} \dot{\mathbf{q}} + PE(\mathbf{q})").

Oh, but oops, the Hamiltonian has to be a function of
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}"),
not of
![\\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\dot{\mathbf{q}}").
Let’s remember that
![\\dot{\\mathbf{q}} = \\hat{K}\^{-1} \\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D "\dot{\mathbf{q}} = \hat{K}^{-1} \mathbf{p}")
and find the final form of our Hamiltonian (after a bit of simplification,
remembering that the inverse of a symmetric matrix is also symmetric):

![
\\mathcal{H}(\\mathbf{q},\\mathbf{p}) = \\frac{1}{2} \\mathbf{p}\^T \\hat{K}\^{-1} \\mathbf{p} + PE(\\mathbf{q})
](https://latex.codecogs.com/png.latex?%0A%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%20%2B%20PE%28%5Cmathbf%7Bq%7D%29%0A "
\mathcal{H}(\mathbf{q},\mathbf{p}) = \frac{1}{2} \mathbf{p}^T \hat{K}^{-1} \mathbf{p} + PE(\mathbf{q})
")

### Hamiltonian Equations

We got our Hamiltonian! Now just to find our updating functions (the partial
derivatives of the Hamiltonian), and we’re done with the math.

Because we are assuming the case (with loss of generality)
![PE](https://latex.codecogs.com/png.latex?PE "PE") doesn’t depend on
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}"),
the partial derivatives of
![\\mathcal{H}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D "\mathcal{H}")
with respect to
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}")
is:

![
\\nabla\_{\\mathbf{p}} \\mathcal{H}(\\mathbf{q},\\mathbf{p}) = \\hat{K}\^{-1} \\mathbf{p}
](https://latex.codecogs.com/png.latex?%0A%5Cnabla_%7B%5Cmathbf%7Bp%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A "
\nabla_{\mathbf{p}} \mathcal{H}(\mathbf{q},\mathbf{p}) = \hat{K}^{-1} \mathbf{p}
")

We already can calculate
![\\hat{K}\^{-1}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D%5E%7B-1%7D "\hat{K}^{-1}"),
so this wound up being easy peasy. But finding the partial derivatives with
respect to
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
is a little trickier. The gradient is a linear operator, so we can break that
down to just finding the gradient of the
![KE](https://latex.codecogs.com/png.latex?KE "KE") term
![\\frac{1}{2} \\mathbf{p}\^T \\hat{K}\^{-1} \\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cfrac%7B1%7D%7B2%7D%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D "\frac{1}{2} \mathbf{p}^T \hat{K}^{-1} \mathbf{p}").
Because
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}")
is an independent input to
![\\mathcal{H}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D "\mathcal{H}"),
we can just look at the gradient of
![\\hat{K}\^{-1}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D%5E%7B-1%7D "\hat{K}^{-1}").
We can simplify that even more by realizing that for any invertible matrix
![A](https://latex.codecogs.com/png.latex?A "A"),
![\\frac{\\partial}{\\partial q} A\^{-1} = - A\^{-1} \\left\[ \\frac{\\partial}{\\partial q} A \\right\] A\^{-1}](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20A%5E%7B-1%7D%20%3D%20-%20A%5E%7B-1%7D%20%5Cleft%5B%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20A%20%5Cright%5D%20A%5E%7B-1%7D "\frac{\partial}{\partial q} A^{-1} = - A^{-1} \left[ \frac{\partial}{\partial q} A \right] A^{-1}"),
so now we just need to find the partial derivatives of
![\\hat{K}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D "\hat{K}"), or
![\\hat{J}\_f\^T \\hat{M} \\hat{J}\_f}](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%7D "\hat{J}_f^T \hat{M} \hat{J}_f}").
![\\hat{M}](https://latex.codecogs.com/png.latex?%5Chat%7BM%7D "\hat{M}") is a
constant term, so, using the good ol’ product rule over
![\\hat{J}\_f\^T](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f%5ET "\hat{J}_f^T")
and
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f"),
we see that, after some simplification:

![
\\frac{\\partial}{\\partial q\_i} \\left\[ \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\right\] =
    2 \\hat{J}\_f\^T \\hat{M} \\left\[ \\frac{\\partial}{\\partial q\_i} \\hat{J}\_f \\right\]
](https://latex.codecogs.com/png.latex?%0A%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Cleft%5B%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%3D%0A%20%20%20%202%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Cleft%5B%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%0A "
\frac{\partial}{\partial q_i} \left[ \hat{J}_f^T \hat{M} \hat{J}_f \right] =
    2 \hat{J}_f^T \hat{M} \left[ \frac{\partial}{\partial q_i} \hat{J}_f \right]
")

![\\frac{\\partial}{\\partial q\_i} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Chat%7BJ%7D_f "\frac{\partial}{\partial q_i} \hat{J}_f")
(an
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrix, like
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f"))
represents the *second derivatives* of
![f](https://latex.codecogs.com/png.latex?f "f") – the derivative with respect
to ![q\_i](https://latex.codecogs.com/png.latex?q_i "q_i") of the derivatives.

We can write this in a more general way (using the gradient operator
![\\nabla](https://latex.codecogs.com/png.latex?%5Cnabla "\nabla")) as:

![
\\nabla\_{\\mathbf{q}} \\left\[ \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\right\] =
    2 \\hat{J}\_f\^T \\hat{M} \\left\[ \\nabla\_{\\mathbf{q}} \\hat{J}\_f \\right\]
](https://latex.codecogs.com/png.latex?%0A%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Cleft%5B%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%3D%0A%20%20%20%202%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Cleft%5B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%0A "
\nabla_{\mathbf{q}} \left[ \hat{J}_f^T \hat{M} \hat{J}_f \right] =
    2 \hat{J}_f^T \hat{M} \left[ \nabla_{\mathbf{q}} \hat{J}_f \right]
")

If we define
![\\nabla\_{\\mathbf{q}} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f "\nabla_{\mathbf{q}} \hat{J}_f")
as an
![m \\times n \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n%20%5Ctimes%20n "m \times n \times n")
tensor, whose ![n](https://latex.codecogs.com/png.latex?n "n") components are
the each the
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrices corresponding to
![\\frac{\\partial}{\\partial q\_i} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Chat%7BJ%7D_f "\frac{\partial}{\partial q_i} \hat{J}_f")

And with that, we have our final expression for
![\\nabla\_{\\mathbf{q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "\nabla_{\mathbf{q}} \mathcal{H}(\mathbf{q},\mathbf{p})"):

![
\\nabla\_{\\mathbf{q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p}) =
    - \\mathbf{p}\^T \\hat{K}\^{-1} \\hat{J}\_f\^T \\hat{M}
        \\left\[ \\nabla\_{\\mathbf{q}} \\hat{J}\_f \\right\] \\hat{K}\^{-1} \\mathbf{p}
    + \\nabla\_{\\mathbf{q}} PE(\\mathbf{q})
](https://latex.codecogs.com/png.latex?%0A%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%3D%0A%20%20%20%20-%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%0A%20%20%20%20%20%20%20%20%5Cleft%5B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A%20%20%20%20%2B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20PE%28%5Cmathbf%7Bq%7D%29%0A "
\nabla_{\mathbf{q}} \mathcal{H}(\mathbf{q},\mathbf{p}) =
    - \mathbf{p}^T \hat{K}^{-1} \hat{J}_f^T \hat{M}
        \left[ \nabla_{\mathbf{q}} \hat{J}_f \right] \hat{K}^{-1} \mathbf{p}
    + \nabla_{\mathbf{q}} PE(\mathbf{q})
")

And, finally, we have everything we need — we can now construct our equations of
motion! To progress through phase space
(![\\langle \\mathbf{q}, \\mathbf{p}\\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20%5Cmathbf%7Bq%7D%2C%20%5Cmathbf%7Bp%7D%5Crangle "\langle \mathbf{q}, \mathbf{p}\rangle")):

![
\\begin{aligned}
\\dot{q} & = \\nabla\_{\\mathbf{p\_q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p})
  && = \\hat{K}\^{-1} \\mathbf{p} \\\\
\\dot{p}\_q & = - \\nabla\_{\\mathbf{q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p})
  && = \\mathbf{p}\^T \\hat{K}\^{-1} \\hat{J}\_f\^T \\hat{M}
        \\left\[ \\nabla\_{\\mathbf{q}} \\hat{J}\_f \\right\] \\hat{K}\^{-1} \\mathbf{p}
    - \\nabla\_{\\mathbf{q}} PE(\\mathbf{q})
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Cdot%7Bq%7D%20%26%20%3D%20%5Cnabla_%7B%5Cmathbf%7Bp_q%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%0A%20%20%26%26%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%20%5C%5C%0A%5Cdot%7Bp%7D_q%20%26%20%3D%20-%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%0A%20%20%26%26%20%3D%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%0A%20%20%20%20%20%20%20%20%5Cleft%5B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A%20%20%20%20-%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20PE%28%5Cmathbf%7Bq%7D%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\dot{q} & = \nabla_{\mathbf{p_q}} \mathcal{H}(\mathbf{q},\mathbf{p})
  && = \hat{K}^{-1} \mathbf{p} \\
\dot{p}_q & = - \nabla_{\mathbf{q}} \mathcal{H}(\mathbf{q},\mathbf{p})
  && = \mathbf{p}^T \hat{K}^{-1} \hat{J}_f^T \hat{M}
        \left[ \nabla_{\mathbf{q}} \hat{J}_f \right] \hat{K}^{-1} \mathbf{p}
    - \nabla_{\mathbf{q}} PE(\mathbf{q})
\end{aligned}
")

That’s it. We’re done. Have a nice day, thanks for reading!

The Haskell
-----------

Just kidding, now it’s time for the fun stuff :)

We’re going to be using the sized-typed vectors from the
*[vector-sized](http://hackage.haskell.org/package/vector-sized)* package, from
the
[Data.Vector.Sized](http://hackage.haskell.org/package/vector-sized/docs/Data-Vector-Sized.html)
module. This package is really nice because it exports the same interface as the
classic *vector* package, except with the size of the vector in the type. A
`Vector n a` is a vector of length `n` containing values of type `a`.

Our final goal is to be able to simulate a *system of discrete particles*
through *arbitrary generalized coordinates*.

To simplify the math, we always assume that, whatever generalized coordinates
you are using
(![\\mathbb{R}\^n](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5En "\mathbb{R}^n")),
your system “actually” exists in some real flat Cartesian coordinate system
(![\\mathbb{R}\^m](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5Em "\mathbb{R}^m")).
This allows us to take advantage of all of that math we derived in the previous
section.

So, in order to fully describe the system, we need:

1.  Each of their masses (or inertias) in their underlying
    ![m](https://latex.codecogs.com/png.latex?m "m") Cartesian coordinates,
    which we’ll call
    ![\\mathbf{m}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bm%7D "\mathbf{m}").
2.  A function
    ![f : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^m](https://latex.codecogs.com/png.latex?f%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5Em "f : \mathbb{R}^n \rightarrow \mathbb{R}^m")
    to convert the generalized coordinates
    (![\\mathbb{R\^n}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%5En%7D "\mathbb{R^n}"))
    to Cartesian coordinates
    (![\\mathbb{R}\^m](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5Em "\mathbb{R}^m"))
3.  The potential energy function
    ![U : \\mathbb{R}\^n \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?U%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D "U : \mathbb{R}^n \rightarrow \mathbb{R}")
    in the generalized coordinates
    (![\\mathbb{R\^n}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%5En%7D "\mathbb{R^n}"))

From these alone, we can derive the equations of motion for the particles in
phase space as a system of first-order ODEs using the process described above.
Then, given an initial phase space position, we can do a straightforward
first-order ODE integration to simulate our system’s motion through phase space.
That is, to “surf the Hamiltonian waves in phase space”, so to speak.

But, to be explicit, we also are going to need some derivatives for these
functions/vectors, too. If you’ve been following along, the full enumeration of
functions and vectors we need is:

![
\\begin{aligned}
\\mathbf{m} & : \\mathbb{R}\^m \\\\
f & : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^m \\\\
\\hat{J}\_f & : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^{m \\times n} \\\\
\\nabla\_{\\mathbf{q}} \\hat{J}\_f & : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^{m \\times n \\times n} \\\\
U & : \\mathbb{R}\^n \\rightarrow \\mathbb{R} \\\\
\\nabla\_{\\mathbf{q}} U & : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^n
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Cmathbf%7Bm%7D%20%26%20%3A%20%5Cmathbb%7BR%7D%5Em%20%5C%5C%0Af%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5Em%20%5C%5C%0A%5Chat%7BJ%7D_f%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5E%7Bm%20%5Ctimes%20n%7D%20%5C%5C%0A%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5E%7Bm%20%5Ctimes%20n%20%5Ctimes%20n%7D%20%5C%5C%0AU%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%20%5C%5C%0A%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20U%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5En%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\mathbf{m} & : \mathbb{R}^m \\
f & : \mathbb{R}^n \rightarrow \mathbb{R}^m \\
\hat{J}_f & : \mathbb{R}^n \rightarrow \mathbb{R}^{m \times n} \\
\nabla_{\mathbf{q}} \hat{J}_f & : \mathbb{R}^n \rightarrow \mathbb{R}^{m \times n \times n} \\
U & : \mathbb{R}^n \rightarrow \mathbb{R} \\
\nabla_{\mathbf{q}} U & : \mathbb{R}^n \rightarrow \mathbb{R}^n
\end{aligned}
")

But, as we’ll see, with libraries like
*[ad](http://hackage.haskell.org/package/ad)* in Haskell, we can really just ask
the user for
![\\mathbf{m}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bm%7D "\mathbf{m}"),
![f](https://latex.codecogs.com/png.latex?f "f"), and
![U](https://latex.codecogs.com/png.latex?U "U") – all of the derivatives can be
computed automatically!

### Our Data Structures

We can couple together all of these functions in a data type that fully
describes the physics of our systems (the “shape” of the Hamiltonian):

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L19-26
data System m n = System
    { sysInertia       :: R m                         -- ^ 'm' vector
    , sysCoords        :: R n -> R m                  -- ^ f
    , sysJacobian      :: R n -> L m n                -- ^ J_f
    , sysJacobian2     :: R n -> V.Vector n (L m n)   -- ^ grad (J_f)
    , sysPotential     :: R n -> Double               -- ^ U
    , sysPotentialGrad :: R n -> R n                  -- ^ grad U
    }
```

`R n` and `L m n` are from the
*[hmatrix](http://hackage.haskell.org/package/hmatrix)* library; an `R n`
represents an n-vector (That is, an `R 4` is a 4-vector), and an `L m n`
represents an `m x n` matrix (That is, an `L 5 3` is a 5x3 matrix).

A `System m n` will describe a system parameterized by `n` generalized
coordinates, taking place in an underlying `m`-dimensional Cartesian space.

It’ll also be convenient to have a nice data type to describe the state of our
system in terms of its generalized positions
(![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}"))
and generalized velocities (the rates of changes of these positions,
![\\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\dot{\mathbf{q}}")),
which is sometimes called “configuration space”:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L28-32
data Config n = Config
    { confPositions  :: R n
    , confVelocities :: R n
    }
  deriving Show
```

And, more importantly, remember that Hamiltonian dynamics is all about surfing
around on that phase space (generalized positions
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
and their conjugate momenta,
![\\mathbf{p\_q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp_q%7D "\mathbf{p_q}")).
So let’s make a type to describe the state of our system in phase space:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L34-38
data Phase n = Phase
    { phasePositions :: R n
    , phaseMomenta   :: R n
    }
  deriving Show
```

### Getting comfortable with our data types

First of all, assuming we can construct a `System` in a sound way, let’s imagine
some useful functions.

We can write a function `underlyingPosition`, which allows you to give a
position in generalized coordinates, and returns the position in the “underlying
coordinate system”:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L40-41
underlyingPosition :: System m n -> R n -> R m
underlyingPosition = sysCoords
```

Note that the types in our function helps us know exactly what the function is
doing — and also helps us implement it correctly. If we have a `System` in `n`
dimensions, over an underlying `m`-dimensional Cartesian space, then we would
need to convert an `R n` (an n-dimensional vector of all of the positions) into
an `R m` (a vector in the underlying Cartesian space).

Simple enough, but let’s maybe try to calculate something more complicated: the
*momenta* of a system, given its positions and velocities (configuration).

We remember that we have a nice formula for that, up above:

![
\\mathbf{p} = \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\dot{\\mathbf{q}}
](https://latex.codecogs.com/png.latex?%0A%5Cmathbf%7Bp%7D%20%3D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A "
\mathbf{p} = \hat{J}_f^T \hat{M} \hat{J}_f \dot{\mathbf{q}}
")

We can translate that directly into Haskell code:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L40-41
underlyingPosition :: System m n -> R n -> R m
underlyingPosition = sysCoords
```

Note that, because our vectors have their size indexed in their type, this is
pretty simple to write and ensure that the shapes “line up”. In fact, GHC can
even help you write this function by telling you what values can go in what
locations. Being able to get rid of a large class of bugs and clean up your
implementation space is nice, too!

(Note that *hmatrix* requires a `KnownNat` constraint on the size parameters of
our vectors for some functions, so we add this as a constraint on our end.)

With this, we can write a function to convert any state in configuration space
to its coordinates in phase space:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L49-50
toPhase :: (KnownNat n, KnownNat m) => System m n -> Config n -> Phase n
toPhase s c = Phase (confPositions c) (momenta s c)
```

This function is important, because “configuration space” is how we actually
directly observe our system – in terms of positions and velocities, and not in
terms of positions and momenta (and sometimes conjugate momenta might not even
have a meaningful physical interpretation). So, having `toPhase` lets us
“initialize” our system in terms of direct observables, and then convert it to
its phase space representation, which is something that Hamiltonian mechanics
can work with.

### Automatic Differentiation

Now, creating a `System` “from scratch” is not much fun, because you would have
to manually differentiate your coordinate systems and potentials to generate
your Jacobians and gradients.

Here’s where the magic comes in – we can have Haskell generate our Jacobians and
gradients *automatically*, using the amazing
[ad](http://hackage.haskell.org/package/ad) library! We can just use the
appropriately named `grad` and `jacobian` functions!

#### Quick Intro to AD

At the simplest level, if we have a function from some number to some other
number, we can use `diff` to get its derivative:

``` {.haskell}
myFunc      :: RealFloat a => a -> a
diff myFunc :: RealFloat a => a -> a
```

If we have a function a function from a sized vector to a scalar, we can use
`grad` to get its gradient:

``` {.haskell}
myFunc      :: RealFloat a => V.Vector n a -> a
grad myFunc :: RealFloat a => V.Vector n a -> V.Vector n a
```

Where each of the components in the resulting vector corresponds to the rate of
change of the output according to variations in that component.

We’re using **statically sized vector** type from the
[vector-sized](http://hackage.haskell.org/package/vector-sized) package, where
`V.Vector n a` is a `n`-vector of `a`s – that is, a `V.Vector 3 Double` is a
vector of 3 `Double`s.

We have to use `Vector` (instead of `R`, from *hmatrix*) because automatic
differentiation for gradients requires *some Functor* to work. An `R 5` is
essentially a `V.Vector 5 Double`, except the latter can contain other,
non-Double things – and therefore can be used by *ad* to do its magic.

If we have a function from a sized vector to a (differently) sized vector, we
can use the `jacobian` function to get its jacobian!

``` {.haskell}
myFunc          :: RealFloat a => V.Vector n a -> V.Vector m a
jacobian myFunc :: RealFloat a => V.Vector n a -> V.Vector m (V.Vector n a)
```

Again note the usage of sized vector types, and the fact that our
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrix is represented by a `m`-vector of `n`-vectors.

Finally, we can get a “2nd-order Jacobian” by using `jacobians`, which gives you
a lazily linked chain of successive order Jacobians (stored in successive cells
of a `Cofree`). We only need the second order jacobian, so let’s define a quick
utility function that only gives us the second-order Jacobian:

``` {.haskell}
-- import qualified Control.Comonad        as C
-- import qualified Control.Comonad.Cofree as C

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L52-60
jacobian2
    :: (Traversable f, Functor g, RealFloat a)
    => (forall b. RealFloat b => f b -> g b)
    -> f a
    -> g (f (f a))
jacobian2 f = (fmap . fmap . fmap) C.extract  -- ^ take 2nd deriv
            . (fmap . fmap) C.unwrap          -- ^ drap 1st deriv
            . fmap C.unwrap                   -- ^ drop 0th deriv
            . jacobians f                     -- ^ create list of jacobians
```

If you think of `Cofree` as an infinite linked list (of nested Functors),
`jacobians` returns a linked list of derivative tensors. The first item is the
0th derivative (the actual function value), so we drop it with `unwrap` (like
`tail`) for lists). The second item is the 1st derivative, so we drop it again
using `unwrap`. And finally, we only want the third item (the 2nd derivatives)
so we `extract` it (like `head` for lists).

Finally, we can achieve our goal:

``` {.haskell}
myFunc
    :: RealFloat a => V.Vector n a -> V.Vector m a
jacobian2 myFunc
    :: RealFloat a => V.Vector n a -> V.Vector m (V.Vector n (V.Vector n a))
```

#### Conversion between vector-sized and hmatrix

So some ugly things – we need to write some functions to convert between
*vector-sized* sized vectors and *hmatrix* vectors and matrices:

``` {.haskell}
-- import qualified Data.Vector.Generic.Sized as VG

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L62-69
vec2r :: KnownNat n => V.Vector n Double -> R n
vec2r = fromJust . create . VG.fromSized . VG.convert

r2vec :: KnownNat n => R n -> V.Vector n Double
r2vec = VG.convert . fromJust . VG.toSized . extract

vec2l :: (KnownNat m, KnownNat n) => V.Vector m (V.Vector n Double) -> L m n
vec2l = fromJust . (\rs -> withRows rs exactDims) . toList . fmap vec2r
```

Remember that it is necessary to switch because *ad* requires our vectors to be
*Functors*, but `R` and `L` from *hmatrix* are not your typical Hask Functors.
One nice thing is that because they both use TypeLits to get their sized
parameters, we can get type-safe conversions that preserve their size
information!

Also, even though *ad* gives our second-order Jacobian as an
![m \\times n \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n%20%5Ctimes%20n "m \times n \times n")
tensor, we really want it as a `n`-vector of
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrices – that’s how we interpreted it in our original math. So we just need to
write an function to convert what *ad* gives us to the form we expect:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L71-71
rejacobi :: (KnownNat m, KnownNat n) => V.Vector m (L n n) -> V.Vector n (L m n)
```

#### Using AD to Auto-Derive Systems

Now to make a `System` using just the mass vector, the coordinate conversion
function, and the potential energy function:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L76-90
mkSystem
    :: (KnownNat m, KnownNat n)
    => R m
    -> (forall a. RealFloat a => V.Vector n a -> V.Vector m a)
    -> (forall a. RealFloat a => V.Vector n a -> a)
    -> System m n
mkSystem m f u = System
    { sysInertia       =                        m
    , sysCoords        =      vec2r .           f . r2vec
    , sysJacobian      =      vec2l . jacobian  f . r2vec
    , sysJacobian2     = rejacobi
                       . fmap vec2l . jacobian2 f . r2vec
    , sysPotential     =                        u . r2vec
    , sysPotentialGrad =      vec2r .      grad u . r2vec
    }
```

Now, I hesitate to call this “trivial”…but, it really is a straightforward
direct translation of the definitions, minus some ugly conversions back and
forth using `r2vec`, `vec2r`, and `vec2l`!

1.  The vector of masses is just `m`
2.  The coordinate function is just `f`
3.  The jacobian of the coordinate function is just `jacobian f`
4.  The second-order jacobian of the coordinate function is just `jacobian2 f`
5.  The potential energy function is just `u`
6.  The gradient of the potential energy function is just `grad u`

Take a moment to marvel at the fact that the *ad* library automatically
generated all of these for us and created a perfectly well-formed `System` with
all of its gradients and Jacobians by giving only the coordinate function and
the potential energy function, and in such a clean and concise way!

### Equations of Motion

At this point, we’re ready to write our final equations of motion, which we
found to be given by:

![
\\begin{aligned}
\\dot{q} & = \\hat{K}\^{-1} \\mathbf{p} \\\\
\\dot{p}\_q & = \\mathbf{p}\^T \\hat{K}\^{-1} \\hat{J}\_f\^T \\hat{M}
        \\left\[ \\nabla\_{\\mathbf{q}} \\hat{J}\_f \\right\] \\hat{K}\^{-1} \\mathbf{p}
    - \\nabla\_{\\mathbf{q}} PE(\\mathbf{q})
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Cdot%7Bq%7D%20%26%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%20%5C%5C%0A%5Cdot%7Bp%7D_q%20%26%20%3D%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%0A%20%20%20%20%20%20%20%20%5Cleft%5B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A%20%20%20%20-%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20PE%28%5Cmathbf%7Bq%7D%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\dot{q} & = \hat{K}^{-1} \mathbf{p} \\
\dot{p}_q & = \mathbf{p}^T \hat{K}^{-1} \hat{J}_f^T \hat{M}
        \left[ \nabla_{\mathbf{q}} \hat{J}_f \right] \hat{K}^{-1} \mathbf{p}
    - \nabla_{\mathbf{q}} PE(\mathbf{q})
\end{aligned}
")

This equation isn’t particularly beautiful, but it’s straightforward to
translate it into Haskell (using
![\\hat{K} = \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D%20%3D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f "\hat{K} = \hat{J}_f^T \hat{M} \hat{J}_f")):

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L92-109
hamilEqns
    :: (KnownNat n, KnownNat m)
    => System m n
    -> Phase n
    -> (R n, R n)       -- dq/dt and dp/dt
hamilEqns s (Phase q p) = (dqdt, dpdt)
  where
    j       = sysJacobian s q
    trj     = tr j
    mHat    = diag (sysInertia s)
    kHat    = trj <> mHat <> j
    kHatInv = inv kHat
    dqdt    = kHatInv #> p
    dpdt    = vec2r bigUglyThing - sysPotentialGrad s q
      where
        bigUglyThing =
          fmap (\j2 -> -p <.> kHatInv #> trj #> mHat #> j2 #> kHatInv #> p)
               (sysJacobian2 s q)
```

Of course, there is no way to get around the big ugly math term in
![\\dot{p}\_q](https://latex.codecogs.com/png.latex?%5Cdot%7Bp%7D_q "\dot{p}_q"),
but at least it is a direct reading of the math!

The result of `hamilEqns` gives the rate of change of the components of our
`Phase n`.

The rest of the processes then is just to “step” `Phase n`. Gradually update it,
following these rate of changes!

Numerical Integration
---------------------

This process is known as \[Numerical Integration\]\[integration\], and the
“best” way to do it is quite a big field, so for this article we’re going to be
using the extremely extremely simple [Euler
method](https://en.wikipedia.org/wiki/Euler_method) to progress our system
through time.

Disclaimer – The Euler method is typically a **very very bad** choice for
numerical integration (even though, as popularized in the movie *Hidden
Figures*, it was good enough to [send humans to
space?](http://www.latimes.com/science/sciencenow/la-sci-sn-hidden-figures-katherine-johnson-20170109-story.html)).
We are just choosing it for this article because it’s very simple, conceptually!

The basic idea is that you pick a time-step,
![\\Delta t](https://latex.codecogs.com/png.latex?%5CDelta%20t "\Delta t"), and
update each coordinate as:

![
x(t + \\Delta t) = x(t) + \\dot{x} \\Delta t
](https://latex.codecogs.com/png.latex?%0Ax%28t%20%2B%20%5CDelta%20t%29%20%3D%20x%28t%29%20%2B%20%5Cdot%7Bx%7D%20%5CDelta%20t%0A "
x(t + \Delta t) = x(t) + \dot{x} \Delta t
")

Which makes sense visually if we imagine
![\\dot{x}](https://latex.codecogs.com/png.latex?%5Cdot%7Bx%7D "\dot{x}") as the
“slope” of ![x](https://latex.codecogs.com/png.latex?x "x") – it just means to
follow the slope another
![\\Delta t](https://latex.codecogs.com/png.latex?%5CDelta%20t "\Delta t")
steps. If the slope stays constant, this method is perfectly accurate. The
inaccuracy, of course, happens when the slope changes drastically within that
![\\Delta t](https://latex.codecogs.com/png.latex?%5CDelta%20t "\Delta t") (and
also from the fact that small errors cause errors in the new calculations of
![\\dot{x}](https://latex.codecogs.com/png.latex?%5Cdot%7Bx%7D "\dot{x}"), and
so compound over time)

You can understand this symbolically, as well, by remembering that the
derivative can be approximated by
![\\dot{x} \\approx \\frac{x(t + \\Delta t) - x(t)}{\\Delta t}](https://latex.codecogs.com/png.latex?%5Cdot%7Bx%7D%20%5Capprox%20%5Cfrac%7Bx%28t%20%2B%20%5CDelta%20t%29%20-%20x%28t%29%7D%7B%5CDelta%20t%7D "\dot{x} \approx \frac{x(t + \Delta t) - x(t)}{\Delta t}")
for small
![\\Delta t](https://latex.codecogs.com/png.latex?%5CDelta%20t "\Delta t"), and
so we can do a little bit of symbolic manipulation to get
![x(t + \\Delta t) \\approx \\dot{x} \\Delta t + x(t)](https://latex.codecogs.com/png.latex?x%28t%20%2B%20%5CDelta%20t%29%20%5Capprox%20%5Cdot%7Bx%7D%20%5CDelta%20t%20%2B%20x%28t%29 "x(t + \Delta t) \approx \dot{x} \Delta t + x(t)").

We can directly translate this into Haskell:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L111-119
stepEuler
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ q(t) and p(t)
    -> Phase n          -- ^ q(t + dt) and p(t + dt)
stepEuler s dt ph@(Phase q p) = Phase (q + konst dt * dq) (p + konst dt * dp)
  where
    (dq, dp) = hamilEqns s ph
```

And repeatedly evolve this system as a lazy list:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L121-129
runSystem
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ initial phase
    -> [Phase n]        -- ^ progression of the system using Euler integration
runSystem s dt = go
  where
    go p0 = p0 : go (stepEuler s dt p0)
```

The Tests
=========

And…that’s it! Granted, in real life, we would be using a less naive integration
method, but this is essentially the entire process!

Let’s generate the boring system, a 5kg particle in 2D Cartesian Coordinates
under gravity –

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L131-136
simpleSystem :: System 2 2
simpleSystem = mkSystem (vec2 5 5) id pots
  where
    -- U(x,y) = 9.8 * y
    pots :: RealFloat a => V.Vector 2 a -> a
    pots xy = 9.8 * (xy `V.index` 1)
```

If we initialize the particle at position
![\\mathbf{q}\_0 = \\langle 0, 0 \\rangle](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D_0%20%3D%20%5Clangle%200%2C%200%20%5Crangle "\mathbf{q}_0 = \langle 0, 0 \rangle")
and velocity
![\\mathbf{v}\_0 = \\langle 1, 3 \\rangle](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D_0%20%3D%20%5Clangle%201%2C%203%20%5Crangle "\mathbf{v}_0 = \langle 1, 3 \rangle")
(that is,
![v\_x = 1](https://latex.codecogs.com/png.latex?v_x%20%3D%201 "v_x = 1") and
![v\_y = 3](https://latex.codecogs.com/png.latex?v_y%20%3D%203 "v_y = 3")), we
should see something that travels at a constant velocity in x and something that
starts moving “upwards” (in positive y) and eventually reaches a peak and moves
downwards.

We can make our initial configuration:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L138-141
simpleConfig0 :: Config 2
simpleConfig0 = Config { confPositions  = vec2 0 0
                       , confVelocities = vec2 1 3
                       }
```

And then…let it run!

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L143-147
simpleMain :: IO ()
simpleMain =
    mapM_ (disp 2 . phasePositions)  -- position with 2 digits of precision
  . take 25                          -- 25 steps
  $ runSystem simpleSystem 0.1 (toPhase simpleSystem simpleConfig0)
```

We get:

``` {.haskell}
ghci> :l Hamilton.hs
ghci> simpleMain
0     0
0.10  0.30
0.20  0.58
0.30  0.84
0.40  1.08
0.50  1.30
0.60  1.51
0.70  1.69
0.80  1.85
0.90  1.99
1.00  2.12
1.10  2.22
1.20  2.31
1.30  2.37
1.40  2.42
1.50  2.44
1.60  2.45
1.70  2.43
1.80  2.40
1.90  2.35
2.00  2.28
2.10  2.18
2.20  2.07
2.30  1.94
2.40  1.79
```

Exactly what we’d expect! The `x` positions increase steadily, and the `y`
positions increase, slow down, and start decreasing.

We can try a slightly more complicated example that validates all of the work
we’ve done – let’s simulate a simple pendulum. The state of a pendulum is
characterized by one coordinate –
![\\theta](https://latex.codecogs.com/png.latex?%5Ctheta "\theta"), which refers
to the angular (clockwise) from the equilibrium “hanging straight down”
position.
![\\theta = 0](https://latex.codecogs.com/png.latex?%5Ctheta%20%3D%200 "\theta = 0")
corresponds to 6 o’ clock,
![\\theta = \\pi/2](https://latex.codecogs.com/png.latex?%5Ctheta%20%3D%20%5Cpi%2F2 "\theta = \pi/2")
corresponds to 9 o’ clock,
![\\theta = - \\pi / 2](https://latex.codecogs.com/png.latex?%5Ctheta%20%3D%20-%20%5Cpi%20%2F%202 "\theta = - \pi / 2")
corresponds to 3 o’ clock, etc. For a pendulum of length
![l](https://latex.codecogs.com/png.latex?l "l"), we can translate that as
![\\langle x, y \\rangle = \\langle - l sin(\\theta), - l cos(\\theta) \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20x%2C%20y%20%5Crangle%20%3D%20%5Clangle%20-%20l%20sin%28%5Ctheta%29%2C%20-%20l%20cos%28%5Ctheta%29%20%5Crangle "\langle x, y \rangle = \langle - l sin(\theta), - l cos(\theta) \rangle").

Let’s set up that system! We’ll put it under normal gravity potential, again
(![U(x,y) = 9.8 y](https://latex.codecogs.com/png.latex?U%28x%2Cy%29%20%3D%209.8%20y "U(x,y) = 9.8 y")).
Our initial position
![\\theta\_0](https://latex.codecogs.com/png.latex?%5Ctheta_0 "\theta_0") will
be at equilibrium, and our initial angular velocity
![v\_{\\theta 0}](https://latex.codecogs.com/png.latex?v_%7B%5Ctheta%200%7D "v_{\theta 0}")
will be 0.1 radians/sec (clockwise), as we try to induce harmonic motion:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L150-172
pendulum :: System 2 1
pendulum = mkSystem (vec2 5 5) coords pots      -- 5kg particle
  where
    -- <x,y> = <-0.5 sin(theta), -0.5 cos(theta)>
    -- pendulum of length 0.25
    coords :: RealFloat a => V.Vector 1 a -> V.Vector 2 a
    coords (V.head->theta) = fromJust
                           . V.fromList
                           $ [- 0.25 * sin theta, - 0.25 * cos theta]
    -- U(x,y) = 9.8 * y
    pots :: RealFloat a => V.Vector 1 a -> a
    pots q = 9.8 * (coords q `V.index` 1)

pendulumConfig0 :: Config 1
pendulumConfig0 = Config { confPositions  = 0
                         , confVelocities = 0.1
                         }

pendulumMain :: IO ()
pendulumMain =
    mapM_ (disp 3 . phasePositions)  -- position with 2 digits of precision
  . take 25                          -- 25 steps
  $ runSystem pendulum 0.1 (toPhase pendulum pendulumConfig0)
```

This pendulum should wobble back and forth, ever so slightly, around
equilibrium.

``` {.haskell}
ghci> :l Hamilton.hs
ghci> pendulumMain
0
0.010
0.020
0.029
0.037
0.042
0.045
0.044
0.040
0.032
0.021
0.007
-0.008
-0.023
-0.038
-0.051
-0.061
-0.068
-0.069
-0.065
-0.056
-0.041
-0.022
-0.000
0.023
```

We see our ![\\theta](https://latex.codecogs.com/png.latex?%5Ctheta "\theta")
coordinate increasing, then turning around and decreasing, swinging the other
way past equilibrium, and then turning around and heading back!

Clearly our system is gaining some sort of phantom energy, since it rises up to
0.045 on the left, and then all the way up to -0.69 on the right. Rest assured
that this is simply from the inaccuracies in Euler’s Method, which we used to
integrate!

But, conceptually, this is a success! We *automatically generated equations of
motion for a pendulum*. Sweet!

Wrap-Up
=======

We traveled through the world of physics, math, Haskell, and back again to
achieve something that would have initially seemed like a crazy thought
experiment. But, utilizing Hamiltonian mechanics, we have a system that can
automatically generate equations of motion given your coordinate system and a
potential energy function. See my [previous
post](https://blog.jle.im/entry/introducing-the-hamilton-library.html) for even
crazier examples – involving multiple objects, double pendulums, and more!
[ad](http://hackage.haskell.org/package/ad):
http://hackage.haskell.org/package/ad

This post will be a bit heavy in some mathematics and Haskell concepts. The
expected audience is intermediate Haskell programmers, and no previous knowledge
of dependent types is expected.

The mathematics and physics are “extra” flavor text and could potentially be
skipped, but you’ll get the most out of this article if you have basic
familiarity with:

1.  Basic concepts of multivariable calculus (like partial and total
    derivatives).
2.  Concepts of linear algebra (like dot products, matrix multiplication, and
    matrix inverses)

No physics knowledge is assumed, but knowing a little bit of first semester
physics would help you gain a bit more of an appreciation for the end result!

The Goal
--------

At the end of this, we should be able to have Haskell *automatically generate*
**equations of motions** for any arbitrary system described in arbitrary
coordinate systems, and simulate that system.

Normally, we’d describe a system using particles’ x and y coordinates, but our
goal is to be able to describe our particles’ positions using any coordinate
system we want (polar, distance-along-a-curved-rail, pendulum-angles, etc.) and
have Haskell automatically generate equations of motions and time progressions
of those coordinates!

Read [my hamilton library
introduction](https://blog.jle.im/entry/introducing-the-hamilton-library.html)
for more information and examples!

Hamiltonian Mechanics
=====================

As mentioned in the previous post, Hamiltonian mechanics is a re-imagining of
dynamics and mechanics (think “the world
post-![F = m a](https://latex.codecogs.com/png.latex?F%20%3D%20m%20a "F = m a")”)
that not only opened up new doors to solving problems in classical, but also
ended up being the right angle of viewing the world to unlock statistical
mechanics and thermodynamics, and later even quantum mechanics.

Hamiltonian mechanics lets you parameterize your system’s “position” in
arbitrary ways (like the angle of rotation, for pendulum problems) and then
posits that the full state of the system exists in something called *phase
space*, and that the system’s dynamics is its motion through phase space that is
dictated by the geometry of the *Hamiltonian* of that phase space.

The system’s *Hamiltonian* is a
![\\mathbb{R}\^{2n} \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5E%7B2n%7D%20%5Crightarrow%20%5Cmathbb%7BR%7D "\mathbb{R}^{2n} \rightarrow \mathbb{R}")
function on phase space (where ![n](https://latex.codecogs.com/png.latex?n "n")
is the number of coordinates parameterizing your system) to
![\\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D "\mathbb{R}").
For a time-independent system, the picture of the dynamics is pretty simple: the
system moves along the *contour lines* of the *Hamiltonian* – the lines of equal
“height”.

![Example of contour lines of a
![\\mathbb{R}\^2 \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5E2%20%5Crightarrow%20%5Cmathbb%7BR%7D "\mathbb{R}^2 \rightarrow \mathbb{R}")
function – the elevation of land. From the [Ordinace
Survey](https://www.ordnancesurvey.co.uk/blog/2015/11/map-reading-skills-making-sense-of-contour-lines/)
website.](/img/entries/hamilton/contour-lines.jpg "Contour lines")

In the example above, if we imagine that phase space is the 2D location, then
the *Hamiltonian* is the mountain. And for a system dropped anywhere on the
mountain, its motion would be along the contour lines. For example, if a system
started somewhere along the 10 contour line, it would begin to oscillate the
entire phase space along the 10 contour line.[^5]

*Every* [smooth](https://www.youtube.com/watch?v=izGwDsrQ1eQ)
![\\mathbb{R}\^{2n} \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5E%7B2n%7D%20%5Crightarrow%20%5Cmathbb%7BR%7D "\mathbb{R}^{2n} \rightarrow \mathbb{R}")
function on phase space can be used as a Hamiltonian to describe the physics of
some system. So, given any “mountain range” on phase space, any “elevation map”
or real-valued function on phase space, you can treat it as a description of the
dynamics of some physical system.

The *trick*, then, to using Hamiltonian dynamics to model your system, is:

1.  Finding the phase space to describe your system. This can be done based on
    any continuous parameterization of your system (“generalized coordinates”),
    like angles of pendulums and so on.

2.  Finding the Hamiltonian on that phase space to describe your system.

And then Hamilton’s dynamics will give you the rest! All you do is “follow the
contour lines” on that Hamiltonian!

### Phase Space

The only thing I’ve really said in detail about phase space is that if your
system’s state has ![n](https://latex.codecogs.com/png.latex?n "n") parameters,
then the corresponding phase space is
![2n](https://latex.codecogs.com/png.latex?2n "2n")-dimensional (and that
Hamiltonian mechanics is somehow about systems moving around in phase space).
Let me clear it up now: *Phase space* is a
![2n](https://latex.codecogs.com/png.latex?2n "2n")-dimensional space
parameterized by:

1.  All of the current values of the
    ![n](https://latex.codecogs.com/png.latex?n "n") parameters (“generalized
    coordinates”)
2.  All of the current “generalized momenta” of those
    ![n](https://latex.codecogs.com/png.latex?n "n") parameters

So if you were parameterizing your pendulum system by, say, the angle of the
pendulum, the phase space would be the current angle of the pendulum along with
the current “generalized momentum” associated with the angle of the pendulum.
What exactly *is* generalized momentum? We’ll go over calculating it eventually,
but what does it represent…*physically*?

I could give you some spiel here about the underlying Lie algebra of the Lie
group associated with the generalized coordinates, but that would make this a
completely different post. What I *can* say is that the generalized momenta
associated with (“conjugate to”) certain sets of familiar coordinates yield
things that we typically call “momenta”:

1.  The momentum conjugate to normal Cartesian coordinates is just our normal
    run-of-the-mill *linear momentum* (in the
    ![\\mathbf{p} = m \\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D%20%3D%20m%20%5Cmathbf%7Bv%7D "\mathbf{p} = m \mathbf{v}"))
    from first semester physics.

2.  The momentum conjugate to the angle
    ![\\theta](https://latex.codecogs.com/png.latex?%5Ctheta "\theta") in polar
    coordinates is *angular momentum*
    (![\\mathbf{L} = m \\mathbf{r} \\times \\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7BL%7D%20%3D%20m%20%5Cmathbf%7Br%7D%20%5Ctimes%20%5Cmathbf%7Bv%7D "\mathbf{L} = m \mathbf{r} \times \mathbf{v}"),
    or
    ![L = m r\^2 \\dot{\\theta}](https://latex.codecogs.com/png.latex?L%20%3D%20m%20r%5E2%20%5Cdot%7B%5Ctheta%7D "L = m r^2 \dot{\theta}"))
    from first semester physics.

3.  The momentum conjugate to the radial coordinate
    ![r](https://latex.codecogs.com/png.latex?r "r") in polar coordinates is
    also just boring old linear momentum
    ![p\_r = m \\dot{r}](https://latex.codecogs.com/png.latex?p_r%20%3D%20m%20%5Cdot%7Br%7D "p_r = m \dot{r}").

So, it’s our normal momentum (for linear and polar coordinates) *generalized* to
arbitrary coordinates.

### Hamiltonian Dynamics

I’ve explained Hamiltonian dynamics for time-independent Hamiltonians as “follow
the contour lines”. If you remember your basic multi-variable calculus course,
you’ll know that the line of “steepest ascent” is the gradient. If we call the
Hamiltonian
![\\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "\mathcal{H}(\mathbf{q},\mathbf{p})")
(where
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
is the vector of positions and
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}")
is the vector of momenta), then the direction of steepest ascent is

![
\\left \\langle \\frac{\\partial}{\\partial \\mathbf{q}}
\\mathcal{H}(\\mathbf{q},\\mathbf{p}), \\frac{\\partial}{\\partial \\mathbf{p}}
\\mathcal{H}(\\mathbf{q},\\mathbf{p}) \\right \\rangle
](https://latex.codecogs.com/png.latex?%0A%5Cleft%20%5Clangle%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cmathbf%7Bq%7D%7D%0A%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%2C%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cmathbf%7Bp%7D%7D%0A%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%5Cright%20%5Crangle%0A "
\left \langle \frac{\partial}{\partial \mathbf{q}}
\mathcal{H}(\mathbf{q},\mathbf{p}), \frac{\partial}{\partial \mathbf{p}}
\mathcal{H}(\mathbf{q},\mathbf{p}) \right \rangle
")

But we want to move along the *contour lines*…and these are the lines
*perpendicular* to the direction of steepest descent. The vector perpendicular
to
![\\langle x, y \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20x%2C%20y%20%5Crangle "\langle x, y \rangle")
is
![\\langle y, -x \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20y%2C%20-x%20%5Crangle "\langle y, -x \rangle"),[^6]
so we just derived the actual Hamiltonian equations of motion: just move in the
direction perpendicular to the steepest ascent!

![
\\begin{aligned}
\\dot{q} & = \\frac{\\partial}{\\partial p\_q} \\mathcal{H}(\\mathbf{q},\\mathbf{p}) \\\\
\\dot{p}\_q & = - \\frac{\\partial}{\\partial q} \\mathcal{H}(\\mathbf{q},\\mathbf{p})
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Cdot%7Bq%7D%20%26%20%3D%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20p_q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%5C%5C%0A%5Cdot%7Bp%7D_q%20%26%20%3D%20-%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\dot{q} & = \frac{\partial}{\partial p_q} \mathcal{H}(\mathbf{q},\mathbf{p}) \\
\dot{p}_q & = - \frac{\partial}{\partial q} \mathcal{H}(\mathbf{q},\mathbf{p})
\end{aligned}
")

Which holds for every generalized coordinate
![q](https://latex.codecogs.com/png.latex?q "q"), where
![p\_q](https://latex.codecogs.com/png.latex?p_q "p_q") is the momentum
conjugate to that coordinate. (For the rest of this post,
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
refers to the vector of coordinates,
![q](https://latex.codecogs.com/png.latex?q "q") refers to a single specific
coordinate, and ![p\_q](https://latex.codecogs.com/png.latex?p_q "p_q") refers
to the momentum conjugate to that coordinate).

Essentially, these give you “updating functions” for
![q](https://latex.codecogs.com/png.latex?q "q") and
![p\_q](https://latex.codecogs.com/png.latex?p_q "p_q") – given
![\\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "\mathcal{H}(\mathbf{q},\mathbf{p})"),
you have a way to “update” the particle’s position in phase space. Just take the
partial derivatives of
![\\mathcal{H}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D "\mathcal{H}")
at every step in time! To update
![q](https://latex.codecogs.com/png.latex?q "q"), nudge it by
![\\frac{\\partial}{\\partial p\_q} \\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20p_q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "\frac{\partial}{\partial p_q} \mathcal{H}(\mathbf{q},\mathbf{p})").
To update ![p\_q](https://latex.codecogs.com/png.latex?p_q "p_q"), nudge it by
![-\\frac{\\partial}{\\partial q} \\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?-%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "-\frac{\partial}{\partial q} \mathcal{H}(\mathbf{q},\mathbf{p})")!

This picture is appealing to me in a visceral way because it sort of seems like
the system is “surfing” along the Hamiltonian’s contour lines. It’s being
“pushed” *faster* when the Hamiltonian is steeper, and slower when it’s more
shallow. I can apply all my intuition as a surfer[^7] to Hamiltonian mechanics!

Hamiltonian Dynamics and Physical Systems
-----------------------------------------

Earlier I mentioned that the two steps for applying Hamiltonian mechanics to
your system was figuring out your system’s conjugate momenta and the appropriate
Hamiltonian. To explain this, I’m going to make a couple of simplifying
assumptions that make the job easier for the purposes of this article:

1.  Your coordinates and potential energy are time-independent.
2.  Your potential energy function only depends on *positions*, and not
    *velocities*. (So nothing like friction or wind resistance or magnetic field
    vector potentials)

With these assumptions, I’m going to skip over discussing the
[Lagrangian](https://en.wikipedia.org/wiki/Lagrangian_mechanics) of the system,
which is the traditional way to do this. You can think of this section as me
presenting derived conclusions and skipping the derivations.

### Conjugate Momenta

For systems with velocity-independent potential energies, it can be shown that
the momentum conjugate to coordinate
![q](https://latex.codecogs.com/png.latex?q "q") is

![
p\_q = \\frac{\\partial}{\\partial \\dot{q}} KE(\\mathbf{q}, \\dot{\\mathbf{q}})
](https://latex.codecogs.com/png.latex?%0Ap_q%20%3D%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cdot%7Bq%7D%7D%20KE%28%5Cmathbf%7Bq%7D%2C%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%29%0A "
p_q = \frac{\partial}{\partial \dot{q}} KE(\mathbf{q}, \dot{\mathbf{q}})
")

Where
![KE(\\mathbf{q},\\dot{\\mathbf{q}})](https://latex.codecogs.com/png.latex?KE%28%5Cmathbf%7Bq%7D%2C%5Cdot%7B%5Cmathbf%7Bq%7D%7D%29 "KE(\mathbf{q},\dot{\mathbf{q}})")
is the kinetic energy of the system, which is a function on the coordinates
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
and their rates of change,
![\\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\dot{\mathbf{q}}").
For example, for normal Cartesian coordinates in one dimension,
![KE(x, \\dot{x}) = \\frac{1}{2} m \\dot{x}\^2](https://latex.codecogs.com/png.latex?KE%28x%2C%20%5Cdot%7Bx%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20m%20%5Cdot%7Bx%7D%5E2 "KE(x, \dot{x}) = \frac{1}{2} m \dot{x}^2").
So the momentum conjugate to ![x](https://latex.codecogs.com/png.latex?x "x")
is:

![
p\_x = \\frac{\\partial}{\\partial \\dot{x}} \\left\[ \\frac{1}{2} m \\dot{x}\^2 \\right\] = m \\dot{x}
](https://latex.codecogs.com/png.latex?%0Ap_x%20%3D%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cdot%7Bx%7D%7D%20%5Cleft%5B%20%5Cfrac%7B1%7D%7B2%7D%20m%20%5Cdot%7Bx%7D%5E2%20%5Cright%5D%20%3D%20m%20%5Cdot%7Bx%7D%0A "
p_x = \frac{\partial}{\partial \dot{x}} \left[ \frac{1}{2} m \dot{x}^2 \right] = m \dot{x}
")

Just linear momentum, like I claimed before.

Alright, now let’s generalize this to arbitrary coordinates. In general, for
*Cartesian* coordinates, the kinetic energy will always be

![
KE(\\mathbf{x}, \\dot{\\mathbf{x}}) = \\frac{1}{2} \\left\[ m\_1 \\dot{x}\_1\^2 + m\_2 \\dot{x}\_2\^2 + m\_3 \\dot{x}\_3\^2 + \\dots \\right\]
](https://latex.codecogs.com/png.latex?%0AKE%28%5Cmathbf%7Bx%7D%2C%20%5Cdot%7B%5Cmathbf%7Bx%7D%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cleft%5B%20m_1%20%5Cdot%7Bx%7D_1%5E2%20%2B%20m_2%20%5Cdot%7Bx%7D_2%5E2%20%2B%20m_3%20%5Cdot%7Bx%7D_3%5E2%20%2B%20%5Cdots%20%5Cright%5D%0A "
KE(\mathbf{x}, \dot{\mathbf{x}}) = \frac{1}{2} \left[ m_1 \dot{x}_1^2 + m_2 \dot{x}_2^2 + m_3 \dot{x}_3^2 + \dots \right]
")

Where ![m](https://latex.codecogs.com/png.latex?m "m") is the inertia associated
with each coordinate…for example, if
![\\langle x\_1, x\_2 \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20x_1%2C%20x_2%20%5Crangle "\langle x_1, x_2 \rangle")
describes the location of an object of mass
![m](https://latex.codecogs.com/png.latex?m "m"), then
![m\_1 = m\_2 = m](https://latex.codecogs.com/png.latex?m_1%20%3D%20m_2%20%3D%20m "m_1 = m_2 = m").

To make things more convenient, we’ll treat this as a quadratic form over an
inertia matrix:

![
KE(\\dot{\\mathbf{x}}) = \\frac{1}{2} \\dot{\\mathbf{x}}\^T \\hat{M} \\dot{\\mathbf{x}}
](https://latex.codecogs.com/png.latex?%0AKE%28%5Cdot%7B%5Cmathbf%7Bx%7D%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bx%7D%7D%5ET%20%5Chat%7BM%7D%20%5Cdot%7B%5Cmathbf%7Bx%7D%7D%0A "
KE(\dot{\mathbf{x}}) = \frac{1}{2} \dot{\mathbf{x}}^T \hat{M} \dot{\mathbf{x}}
")

Where ![\\hat{M}](https://latex.codecogs.com/png.latex?%5Chat%7BM%7D "\hat{M}")
is the [diagonal matrix](https://en.wikipedia.org/wiki/Diagonal_matrix) whose
entries are the masses of each coordinate, and
![\\dot{\\mathbf{x}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bx%7D%7D "\dot{\mathbf{x}}")
is the column vector of all of the (Cartesian) coordinates,
![\\left\[ \\dot{x}\_1\\, \\dot{x}\_2\\, \\dot{x}\_3\\, \\dots \\right\]\^T](https://latex.codecogs.com/png.latex?%5Cleft%5B%20%5Cdot%7Bx%7D_1%5C%2C%20%5Cdot%7Bx%7D_2%5C%2C%20%5Cdot%7Bx%7D_3%5C%2C%20%5Cdots%20%5Cright%5D%5ET "\left[ \dot{x}_1\, \dot{x}_2\, \dot{x}_3\, \dots \right]^T").

Now! How to generalize this to arbitrary coordinates? Well, if we have
![n](https://latex.codecogs.com/png.latex?n "n") generalized coordinates
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
mapping to ![m](https://latex.codecogs.com/png.latex?m "m")-dimensional
Cartesian coordinates, we can specify them as
![\\mathbf{x} = f(\\mathbf{q})](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bx%7D%20%3D%20f%28%5Cmathbf%7Bq%7D%29 "\mathbf{x} = f(\mathbf{q})"),
where
![f : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^m](https://latex.codecogs.com/png.latex?f%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5Em "f : \mathbb{R}^n \rightarrow \mathbb{R}^m"),
taking the vector of generalized coordinates and returning a vector for the
position in Cartesian space. For example, for polar coordinates,
![f(r, \\theta) = \\left \\langle r \\cos(\\theta), r \\sin(\\theta) \\right \\rangle](https://latex.codecogs.com/png.latex?f%28r%2C%20%5Ctheta%29%20%3D%20%5Cleft%20%5Clangle%20r%20%5Ccos%28%5Ctheta%29%2C%20r%20%5Csin%28%5Ctheta%29%20%5Cright%20%5Crangle "f(r, \theta) = \left \langle r \cos(\theta), r \sin(\theta) \right \rangle"),
because, for polar coordinates,
![x = r \\cos(\\theta)](https://latex.codecogs.com/png.latex?x%20%3D%20r%20%5Ccos%28%5Ctheta%29 "x = r \cos(\theta)")
and
![y = r \\sin(\\theta)](https://latex.codecogs.com/png.latex?y%20%3D%20r%20%5Csin%28%5Ctheta%29 "y = r \sin(\theta)").

So we can get
![\\mathbf{x}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bx%7D "\mathbf{x}")
from
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
with ![f](https://latex.codecogs.com/png.latex?f "f"), but how can we get
![\\dot{\\mathbf{x}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bx%7D%7D "\dot{\mathbf{x}}"),
the vector of rate of changes? Well, if
![x\_1 = f\_1(q\_1, q\_2, q\_3 \\dots)](https://latex.codecogs.com/png.latex?x_1%20%3D%20f_1%28q_1%2C%20q_2%2C%20q_3%20%5Cdots%29 "x_1 = f_1(q_1, q_2, q_3 \dots)"),
then the
![\\dot{x}\_1](https://latex.codecogs.com/png.latex?%5Cdot%7Bx%7D_1 "\dot{x}_1")
is the [total derivative](https://en.wikipedia.org/wiki/Total_derivative) of
![x\_1](https://latex.codecogs.com/png.latex?x_1 "x_1") with respect to time:

![
\\dot{x}\_1 = \\frac{\\partial f\_1}{\\partial q\_1} \\dot{q}\_1 +
    \\frac{\\partial f\_1}{\\partial q\_2} \\dot{q}\_2 +
    \\frac{\\partial f\_1}{\\partial q\_3} \\dot{q}\_3 + \\dots
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7Bx%7D_1%20%3D%20%5Cfrac%7B%5Cpartial%20f_1%7D%7B%5Cpartial%20q_1%7D%20%5Cdot%7Bq%7D_1%20%2B%0A%20%20%20%20%5Cfrac%7B%5Cpartial%20f_1%7D%7B%5Cpartial%20q_2%7D%20%5Cdot%7Bq%7D_2%20%2B%0A%20%20%20%20%5Cfrac%7B%5Cpartial%20f_1%7D%7B%5Cpartial%20q_3%7D%20%5Cdot%7Bq%7D_3%20%2B%20%5Cdots%0A "
\dot{x}_1 = \frac{\partial f_1}{\partial q_1} \dot{q}_1 +
    \frac{\partial f_1}{\partial q_2} \dot{q}_2 +
    \frac{\partial f_1}{\partial q_3} \dot{q}_3 + \dots
")

Or, in short:

![
\\dot{x}\_i = \\sum\_{j = 1}\^n \\frac{\\partial f\_i}{\\partial q\_j} \\dot{q}\_j
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7Bx%7D_i%20%3D%20%5Csum_%7Bj%20%3D%201%7D%5En%20%5Cfrac%7B%5Cpartial%20f_i%7D%7B%5Cpartial%20q_j%7D%20%5Cdot%7Bq%7D_j%0A "
\dot{x}_i = \sum_{j = 1}^n \frac{\partial f_i}{\partial q_j} \dot{q}_j
")

But, hey, this looks a lot like a matrix multiplication. If we call
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f")
the [Jacobian
matrix](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant), the
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrix of partial derivatives of
![f](https://latex.codecogs.com/png.latex?f "f")
(![\\hat{J}\_{fij} = \\frac{\\partial f\_i}{\\partial q\_j}](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_%7Bfij%7D%20%3D%20%5Cfrac%7B%5Cpartial%20f_i%7D%7B%5Cpartial%20q_j%7D "\hat{J}_{fij} = \frac{\partial f_i}{\partial q_j}"))
at a given point, then we have a nice expression for
![\\dot{\\mathbf{x}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bx%7D%7D "\dot{\mathbf{x}}"):

![
\\dot{\\mathbf{x}} = \\hat{J}\_f \\dot{\\mathbf{q}}
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7B%5Cmathbf%7Bx%7D%7D%20%3D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A "
\dot{\mathbf{x}} = \hat{J}_f \dot{\mathbf{q}}
")

And we can plug it in (remembering that
![(A B)\^T = B\^T A\^T](https://latex.codecogs.com/png.latex?%28A%20B%29%5ET%20%3D%20B%5ET%20A%5ET "(A B)^T = B^T A^T"))
to our kinetic energy equation to get:

![
KE(\\mathbf{q},\\dot{\\mathbf{q}}) = \\frac{1}{2} \\dot{\\mathbf{q}}\^T \\hat{J}\_f\^T
    \\hat{M} \\hat{J}\_f \\dot{\\mathbf{q}}
](https://latex.codecogs.com/png.latex?%0AKE%28%5Cmathbf%7Bq%7D%2C%5Cdot%7B%5Cmathbf%7Bq%7D%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%5ET%20%5Chat%7BJ%7D_f%5ET%0A%20%20%20%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A "
KE(\mathbf{q},\dot{\mathbf{q}}) = \frac{1}{2} \dot{\mathbf{q}}^T \hat{J}_f^T
    \hat{M} \hat{J}_f \dot{\mathbf{q}}
")

And for the final step, we differentiate with respect to the
![\\dot{q}](https://latex.codecogs.com/png.latex?%5Cdot%7Bq%7D "\dot{q}")s
(which is just the gradient
![\\nabla\_{\\dot{\\mathbf{q}}}](https://latex.codecogs.com/png.latex?%5Cnabla_%7B%5Cdot%7B%5Cmathbf%7Bq%7D%7D%7D "\nabla_{\dot{\mathbf{q}}}"))
to get
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}"),
the vector of conjugate momenta:

![
\\mathbf{p} = \\nabla\_{\\dot{\\mathbf{q}}} \\left\[
    \\frac{1}{2} \\dot{\\mathbf{q}}\^T \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\dot{\\mathbf{q}}
  \\right\]
  = \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\dot{\\mathbf{q}}
](https://latex.codecogs.com/png.latex?%0A%5Cmathbf%7Bp%7D%20%3D%20%5Cnabla_%7B%5Cdot%7B%5Cmathbf%7Bq%7D%7D%7D%20%5Cleft%5B%0A%20%20%20%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%5ET%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A%20%20%5Cright%5D%0A%20%20%3D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A "
\mathbf{p} = \nabla_{\dot{\mathbf{q}}} \left[
    \frac{1}{2} \dot{\mathbf{q}}^T \hat{J}_f^T \hat{M} \hat{J}_f \dot{\mathbf{q}}
  \right]
  = \hat{J}_f^T \hat{M} \hat{J}_f \dot{\mathbf{q}}
")

Now, we’re going to be using
![\\hat{J}\_f\^T \\hat{M} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f "\hat{J}_f^T \hat{M} \hat{J}_f")
a lot, so let’s give it a name,
![\\hat{K}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D "\hat{K}"). If
the masses are all positive and
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f")
is full-rank[^8], then
![\\hat{K}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D "\hat{K}") is a
symmetric, positive-definite, invertible matrix (by construction). It’s
important to also remember that it’s an explicit function of
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}"),
because
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f")
is a matrix of partial derivatives at a given
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}").
We now have a simple expression for the vector of conjugate momenta
(![\\mathbf{p} = \\hat{K} \\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D%20%3D%20%5Chat%7BK%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\mathbf{p} = \hat{K} \dot{\mathbf{q}}")),
and also for kinetic energy
(![KE = \\frac{1}{2} \\dot{\\mathbf{q}}\^T \\hat{K} \\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?KE%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%5ET%20%5Chat%7BK%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D "KE = \frac{1}{2} \dot{\mathbf{q}}^T \hat{K} \dot{\mathbf{q}}")).

It’s going to be important for us to also be able to go backwards (to get
![\\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\dot{\mathbf{q}}")
from
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}")).
Luckily, because we wrote the whole thing as a matrix operation, going backwards
is easy – just take the matrix inverse, which we know exists!

![
\\dot{\\mathbf{q}} = \\hat{K}\^{-1} \\mathbf{p}
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7B%5Cmathbf%7Bq%7D%7D%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A "
\dot{\mathbf{q}} = \hat{K}^{-1} \mathbf{p}
")

The power of linear algebra!

### Hamiltonians of Physical Systems

Ok, that’s step one. How about step two – finding the Hamiltonian for your
system?

The *real* Hamiltonian is actually the [Poisson
bracket](https://en.wikipedia.org/wiki/Poisson_bracket) of the system’s
[Lagrangian](https://en.wikipedia.org/wiki/Lagrangian_mechanics), but I did some
of the work for you for the case of time-independent coordinates where the
potential energy depends *only* on positions (so, no friction, wind resistance,
etc.), the Hamiltonian of a system is precisely the system’s total [mechanical
energy](https://en.wikipedia.org/wiki/Mechanical_energy), or its kinetic energy
plus the potential energy:

![
\\mathcal{H}(\\mathbf{q},\\mathbf{p}) = KE(\\mathbf{q},\\mathbf{p}) + PE(\\mathbf{q})
](https://latex.codecogs.com/png.latex?%0A%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%3D%20KE%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%2B%20PE%28%5Cmathbf%7Bq%7D%29%0A "
\mathcal{H}(\mathbf{q},\mathbf{p}) = KE(\mathbf{q},\mathbf{p}) + PE(\mathbf{q})
")

Which makes a lot of intuitive sense, because you might recall that total
mechanical energy is always conserved for certain types of systems.
Incidentally, Hamiltonian dynamics makes sure that the value of the system’s
Hamiltonian stays the same (because it moves along contour lines). So, the
system’s Hamiltonian always stays the same, and so its total mechanical energy
stays the same, as well! Energy is conserved because the Hamiltonian stays the
same!

Anyway, we want to build our system’s Hamiltonian from properties of the
coordinate system, so plugging in our expression for
![KE](https://latex.codecogs.com/png.latex?KE "KE"), we get
![\\mathcal{H}(\\mathbf{q},\\dot{\\mathbf{q}}) = \\frac{1}{2} \\dot{\\mathbf{q}}\^T \\hat{K} \\dot{\\mathbf{q}} + PE(\\mathbf{q})](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cdot%7B%5Cmathbf%7Bq%7D%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%5ET%20%5Chat%7BK%7D%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%20%2B%20PE%28%5Cmathbf%7Bq%7D%29 "\mathcal{H}(\mathbf{q},\dot{\mathbf{q}}) = \frac{1}{2} \dot{\mathbf{q}}^T \hat{K} \dot{\mathbf{q}} + PE(\mathbf{q})").

Oh, but oops, the Hamiltonian has to be a function of
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}"),
not of
![\\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\dot{\mathbf{q}}").
Let’s remember that
![\\dot{\\mathbf{q}} = \\hat{K}\^{-1} \\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D "\dot{\mathbf{q}} = \hat{K}^{-1} \mathbf{p}")
and find the final form of our Hamiltonian (after a bit of simplification,
remembering that the inverse of a symmetric matrix is also symmetric):

![
\\mathcal{H}(\\mathbf{q},\\mathbf{p}) = \\frac{1}{2} \\mathbf{p}\^T \\hat{K}\^{-1} \\mathbf{p} + PE(\\mathbf{q})
](https://latex.codecogs.com/png.latex?%0A%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%20%2B%20PE%28%5Cmathbf%7Bq%7D%29%0A "
\mathcal{H}(\mathbf{q},\mathbf{p}) = \frac{1}{2} \mathbf{p}^T \hat{K}^{-1} \mathbf{p} + PE(\mathbf{q})
")

### Hamiltonian Equations

We got our Hamiltonian! Now just to find our updating functions (the partial
derivatives of the Hamiltonian), and we’re done with the math.

Because we are assuming the case (with loss of generality)
![PE](https://latex.codecogs.com/png.latex?PE "PE") doesn’t depend on
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}"),
the partial derivatives of
![\\mathcal{H}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D "\mathcal{H}")
with respect to
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}")
is:

![
\\nabla\_{\\mathbf{p}} \\mathcal{H}(\\mathbf{q},\\mathbf{p}) = \\hat{K}\^{-1} \\mathbf{p}
](https://latex.codecogs.com/png.latex?%0A%5Cnabla_%7B%5Cmathbf%7Bp%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A "
\nabla_{\mathbf{p}} \mathcal{H}(\mathbf{q},\mathbf{p}) = \hat{K}^{-1} \mathbf{p}
")

We already can calculate
![\\hat{K}\^{-1}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D%5E%7B-1%7D "\hat{K}^{-1}"),
so this wound up being easy peasy. But finding the partial derivatives with
respect to
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
is a little trickier. The gradient is a linear operator, so we can break that
down to just finding the gradient of the
![KE](https://latex.codecogs.com/png.latex?KE "KE") term
![\\frac{1}{2} \\mathbf{p}\^T \\hat{K}\^{-1} \\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cfrac%7B1%7D%7B2%7D%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D "\frac{1}{2} \mathbf{p}^T \hat{K}^{-1} \mathbf{p}").
Because
![\\mathbf{p}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp%7D "\mathbf{p}")
is an independent input to
![\\mathcal{H}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BH%7D "\mathcal{H}"),
we can just look at the gradient of
![\\hat{K}\^{-1}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D%5E%7B-1%7D "\hat{K}^{-1}").
We can simplify that even more by realizing that for any invertible matrix
![A](https://latex.codecogs.com/png.latex?A "A"),
![\\frac{\\partial}{\\partial q} A\^{-1} = - A\^{-1} \\left\[ \\frac{\\partial}{\\partial q} A \\right\] A\^{-1}](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20A%5E%7B-1%7D%20%3D%20-%20A%5E%7B-1%7D%20%5Cleft%5B%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20A%20%5Cright%5D%20A%5E%7B-1%7D "\frac{\partial}{\partial q} A^{-1} = - A^{-1} \left[ \frac{\partial}{\partial q} A \right] A^{-1}"),
so now we just need to find the partial derivatives of
![\\hat{K}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D "\hat{K}"), or
![\\hat{J}\_f\^T \\hat{M} \\hat{J}\_f}](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%7D "\hat{J}_f^T \hat{M} \hat{J}_f}").
![\\hat{M}](https://latex.codecogs.com/png.latex?%5Chat%7BM%7D "\hat{M}") is a
constant term, so, using the good ol’ product rule over
![\\hat{J}\_f\^T](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f%5ET "\hat{J}_f^T")
and
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f"),
we see that, after some simplification:

![
\\frac{\\partial}{\\partial q\_i} \\left\[ \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\right\] =
    2 \\hat{J}\_f\^T \\hat{M} \\left\[ \\frac{\\partial}{\\partial q\_i} \\hat{J}\_f \\right\]
](https://latex.codecogs.com/png.latex?%0A%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Cleft%5B%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%3D%0A%20%20%20%202%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Cleft%5B%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%0A "
\frac{\partial}{\partial q_i} \left[ \hat{J}_f^T \hat{M} \hat{J}_f \right] =
    2 \hat{J}_f^T \hat{M} \left[ \frac{\partial}{\partial q_i} \hat{J}_f \right]
")

![\\frac{\\partial}{\\partial q\_i} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Chat%7BJ%7D_f "\frac{\partial}{\partial q_i} \hat{J}_f")
(an
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrix, like
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f"))
represents the *second derivatives* of
![f](https://latex.codecogs.com/png.latex?f "f") – the derivative with respect
to ![q\_i](https://latex.codecogs.com/png.latex?q_i "q_i") of the derivatives.

We can write this in a more general way (using the gradient operator
![\\nabla](https://latex.codecogs.com/png.latex?%5Cnabla "\nabla")) as:

![
\\nabla\_{\\mathbf{q}} \\left\[ \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\right\] =
    2 \\hat{J}\_f\^T \\hat{M} \\left\[ \\nabla\_{\\mathbf{q}} \\hat{J}\_f \\right\]
](https://latex.codecogs.com/png.latex?%0A%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Cleft%5B%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%3D%0A%20%20%20%202%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Cleft%5B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%0A "
\nabla_{\mathbf{q}} \left[ \hat{J}_f^T \hat{M} \hat{J}_f \right] =
    2 \hat{J}_f^T \hat{M} \left[ \nabla_{\mathbf{q}} \hat{J}_f \right]
")

If we define
![\\nabla\_{\\mathbf{q}} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f "\nabla_{\mathbf{q}} \hat{J}_f")
as an
![m \\times n \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n%20%5Ctimes%20n "m \times n \times n")
tensor, whose ![n](https://latex.codecogs.com/png.latex?n "n") components are
the each the
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrices corresponding to
![\\frac{\\partial}{\\partial q\_i} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Chat%7BJ%7D_f "\frac{\partial}{\partial q_i} \hat{J}_f")

And with that, we have our final expression for
![\\nabla\_{\\mathbf{q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "\nabla_{\mathbf{q}} \mathcal{H}(\mathbf{q},\mathbf{p})"):

![
\\nabla\_{\\mathbf{q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p}) =
    - \\mathbf{p}\^T \\hat{K}\^{-1} \\hat{J}\_f\^T \\hat{M}
        \\left\[ \\nabla\_{\\mathbf{q}} \\hat{J}\_f \\right\] \\hat{K}\^{-1} \\mathbf{p}
    + \\nabla\_{\\mathbf{q}} PE(\\mathbf{q})
](https://latex.codecogs.com/png.latex?%0A%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%3D%0A%20%20%20%20-%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%0A%20%20%20%20%20%20%20%20%5Cleft%5B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A%20%20%20%20%2B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20PE%28%5Cmathbf%7Bq%7D%29%0A "
\nabla_{\mathbf{q}} \mathcal{H}(\mathbf{q},\mathbf{p}) =
    - \mathbf{p}^T \hat{K}^{-1} \hat{J}_f^T \hat{M}
        \left[ \nabla_{\mathbf{q}} \hat{J}_f \right] \hat{K}^{-1} \mathbf{p}
    + \nabla_{\mathbf{q}} PE(\mathbf{q})
")

And, finally, we have everything we need — we can now construct our equations of
motion! To progress through phase space
(![\\langle \\mathbf{q}, \\mathbf{p}\\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20%5Cmathbf%7Bq%7D%2C%20%5Cmathbf%7Bp%7D%5Crangle "\langle \mathbf{q}, \mathbf{p}\rangle")):

![
\\begin{aligned}
\\dot{q} & = \\nabla\_{\\mathbf{p\_q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p})
  && = \\hat{K}\^{-1} \\mathbf{p} \\\\
\\dot{p}\_q & = - \\nabla\_{\\mathbf{q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p})
  && = \\mathbf{p}\^T \\hat{K}\^{-1} \\hat{J}\_f\^T \\hat{M}
        \\left\[ \\nabla\_{\\mathbf{q}} \\hat{J}\_f \\right\] \\hat{K}\^{-1} \\mathbf{p}
    - \\nabla\_{\\mathbf{q}} PE(\\mathbf{q})
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Cdot%7Bq%7D%20%26%20%3D%20%5Cnabla_%7B%5Cmathbf%7Bp_q%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%0A%20%20%26%26%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%20%5C%5C%0A%5Cdot%7Bp%7D_q%20%26%20%3D%20-%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%0A%20%20%26%26%20%3D%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%0A%20%20%20%20%20%20%20%20%5Cleft%5B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A%20%20%20%20-%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20PE%28%5Cmathbf%7Bq%7D%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\dot{q} & = \nabla_{\mathbf{p_q}} \mathcal{H}(\mathbf{q},\mathbf{p})
  && = \hat{K}^{-1} \mathbf{p} \\
\dot{p}_q & = - \nabla_{\mathbf{q}} \mathcal{H}(\mathbf{q},\mathbf{p})
  && = \mathbf{p}^T \hat{K}^{-1} \hat{J}_f^T \hat{M}
        \left[ \nabla_{\mathbf{q}} \hat{J}_f \right] \hat{K}^{-1} \mathbf{p}
    - \nabla_{\mathbf{q}} PE(\mathbf{q})
\end{aligned}
")

That’s it. We’re done. Have a nice day, thanks for reading!

The Haskell
-----------

Just kidding, now it’s time for the fun stuff :)

We’re going to be using the sized-typed vectors from the
*[vector-sized](http://hackage.haskell.org/package/vector-sized)* package, from
the
[Data.Vector.Sized](http://hackage.haskell.org/package/vector-sized/docs/Data-Vector-Sized.html)
module. This package is really nice because it exports the same interface as the
classic *vector* package, except with the size of the vector in the type. A
`Vector n a` is a vector of length `n` containing values of type `a`.

Our final goal is to be able to simulate a *system of discrete particles*
through *arbitrary generalized coordinates*.

To simplify the math, we always assume that, whatever generalized coordinates
you are using
(![\\mathbb{R}\^n](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5En "\mathbb{R}^n")),
your system “actually” exists in some real flat Cartesian coordinate system
(![\\mathbb{R}\^m](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5Em "\mathbb{R}^m")).
This allows us to take advantage of all of that math we derived in the previous
section.

So, in order to fully describe the system, we need:

1.  Each of their masses (or inertias) in their underlying
    ![m](https://latex.codecogs.com/png.latex?m "m") Cartesian coordinates,
    which we’ll call
    ![\\mathbf{m}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bm%7D "\mathbf{m}").
2.  A function
    ![f : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^m](https://latex.codecogs.com/png.latex?f%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5Em "f : \mathbb{R}^n \rightarrow \mathbb{R}^m")
    to convert the generalized coordinates
    (![\\mathbb{R\^n}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%5En%7D "\mathbb{R^n}"))
    to Cartesian coordinates
    (![\\mathbb{R}\^m](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5Em "\mathbb{R}^m"))
3.  The potential energy function
    ![U : \\mathbb{R}\^n \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?U%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D "U : \mathbb{R}^n \rightarrow \mathbb{R}")
    in the generalized coordinates
    (![\\mathbb{R\^n}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%5En%7D "\mathbb{R^n}"))

From these alone, we can derive the equations of motion for the particles in
phase space as a system of first-order ODEs using the process described above.
Then, given an initial phase space position, we can do a straightforward
first-order ODE integration to simulate our system’s motion through phase space.
That is, to “surf the Hamiltonian waves in phase space”, so to speak.

But, to be explicit, we also are going to need some derivatives for these
functions/vectors, too. If you’ve been following along, the full enumeration of
functions and vectors we need is:

![
\\begin{aligned}
\\mathbf{m} & : \\mathbb{R}\^m \\\\
f & : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^m \\\\
\\hat{J}\_f & : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^{m \\times n} \\\\
\\nabla\_{\\mathbf{q}} \\hat{J}\_f & : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^{m \\times n \\times n} \\\\
U & : \\mathbb{R}\^n \\rightarrow \\mathbb{R} \\\\
\\nabla\_{\\mathbf{q}} U & : \\mathbb{R}\^n \\rightarrow \\mathbb{R}\^n
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Cmathbf%7Bm%7D%20%26%20%3A%20%5Cmathbb%7BR%7D%5Em%20%5C%5C%0Af%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5Em%20%5C%5C%0A%5Chat%7BJ%7D_f%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5E%7Bm%20%5Ctimes%20n%7D%20%5C%5C%0A%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5E%7Bm%20%5Ctimes%20n%20%5Ctimes%20n%7D%20%5C%5C%0AU%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%20%5C%5C%0A%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20U%20%26%20%3A%20%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5En%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\mathbf{m} & : \mathbb{R}^m \\
f & : \mathbb{R}^n \rightarrow \mathbb{R}^m \\
\hat{J}_f & : \mathbb{R}^n \rightarrow \mathbb{R}^{m \times n} \\
\nabla_{\mathbf{q}} \hat{J}_f & : \mathbb{R}^n \rightarrow \mathbb{R}^{m \times n \times n} \\
U & : \mathbb{R}^n \rightarrow \mathbb{R} \\
\nabla_{\mathbf{q}} U & : \mathbb{R}^n \rightarrow \mathbb{R}^n
\end{aligned}
")

But, as we’ll see, with libraries like
*[ad](http://hackage.haskell.org/package/ad)* in Haskell, we can really just ask
the user for
![\\mathbf{m}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bm%7D "\mathbf{m}"),
![f](https://latex.codecogs.com/png.latex?f "f"), and
![U](https://latex.codecogs.com/png.latex?U "U") – all of the derivatives can be
computed automatically!

### Our Data Structures

We can couple together all of these functions in a data type that fully
describes the physics of our systems (the “shape” of the Hamiltonian):

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L19-26
data System m n = System
    { sysInertia       :: R m                         -- ^ 'm' vector
    , sysCoords        :: R n -> R m                  -- ^ f
    , sysJacobian      :: R n -> L m n                -- ^ J_f
    , sysJacobian2     :: R n -> V.Vector n (L m n)   -- ^ grad (J_f)
    , sysPotential     :: R n -> Double               -- ^ U
    , sysPotentialGrad :: R n -> R n                  -- ^ grad U
    }
```

`R n` and `L m n` are from the
*[hmatrix](http://hackage.haskell.org/package/hmatrix)* library; an `R n`
represents an n-vector (That is, an `R 4` is a 4-vector), and an `L m n`
represents an `m x n` matrix (That is, an `L 5 3` is a 5x3 matrix).

A `System m n` will describe a system parameterized by `n` generalized
coordinates, taking place in an underlying `m`-dimensional Cartesian space.

It’ll also be convenient to have a nice data type to describe the state of our
system in terms of its generalized positions
(![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}"))
and generalized velocities (the rates of changes of these positions,
![\\dot{\\mathbf{q}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bq%7D%7D "\dot{\mathbf{q}}")),
which is sometimes called “configuration space”:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L28-32
data Config n = Config
    { confPositions  :: R n
    , confVelocities :: R n
    }
  deriving Show
```

And, more importantly, remember that Hamiltonian dynamics is all about surfing
around on that phase space (generalized positions
![\\mathbf{q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D "\mathbf{q}")
and their conjugate momenta,
![\\mathbf{p\_q}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bp_q%7D "\mathbf{p_q}")).
So let’s make a type to describe the state of our system in phase space:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L34-38
data Phase n = Phase
    { phasePositions :: R n
    , phaseMomenta   :: R n
    }
  deriving Show
```

### Getting comfortable with our data types

First of all, assuming we can construct a `System` in a sound way, let’s imagine
some useful functions.

We can write a function `underlyingPosition`, which allows you to give a
position in generalized coordinates, and returns the position in the “underlying
coordinate system”:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L40-41
underlyingPosition :: System m n -> R n -> R m
underlyingPosition = sysCoords
```

Note that the types in our function helps us know exactly what the function is
doing — and also helps us implement it correctly. If we have a `System` in `n`
dimensions, over an underlying `m`-dimensional Cartesian space, then we would
need to convert an `R n` (an n-dimensional vector of all of the positions) into
an `R m` (a vector in the underlying Cartesian space).

Simple enough, but let’s maybe try to calculate something more complicated: the
*momenta* of a system, given its positions and velocities (configuration).

We remember that we have a nice formula for that, up above:

![
\\mathbf{p} = \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\dot{\\mathbf{q}}
](https://latex.codecogs.com/png.latex?%0A%5Cmathbf%7Bp%7D%20%3D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cdot%7B%5Cmathbf%7Bq%7D%7D%0A "
\mathbf{p} = \hat{J}_f^T \hat{M} \hat{J}_f \dot{\mathbf{q}}
")

We can translate that directly into Haskell code:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L40-41
underlyingPosition :: System m n -> R n -> R m
underlyingPosition = sysCoords
```

Note that, because our vectors have their size indexed in their type, this is
pretty simple to write and ensure that the shapes “line up”. In fact, GHC can
even help you write this function by telling you what values can go in what
locations. Being able to get rid of a large class of bugs and clean up your
implementation space is nice, too!

(Note that *hmatrix* requires a `KnownNat` constraint on the size parameters of
our vectors for some functions, so we add this as a constraint on our end.)

With this, we can write a function to convert any state in configuration space
to its coordinates in phase space:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L49-50
toPhase :: (KnownNat n, KnownNat m) => System m n -> Config n -> Phase n
toPhase s c = Phase (confPositions c) (momenta s c)
```

This function is important, because “configuration space” is how we actually
directly observe our system – in terms of positions and velocities, and not in
terms of positions and momenta (and sometimes conjugate momenta might not even
have a meaningful physical interpretation). So, having `toPhase` lets us
“initialize” our system in terms of direct observables, and then convert it to
its phase space representation, which is something that Hamiltonian mechanics
can work with.

### Automatic Differentiation

Now, creating a `System` “from scratch” is not much fun, because you would have
to manually differentiate your coordinate systems and potentials to generate
your Jacobians and gradients.

Here’s where the magic comes in – we can have Haskell generate our Jacobians and
gradients *automatically*, using the amazing
[ad](http://hackage.haskell.org/package/ad) library! We can just use the
appropriately named `grad` and `jacobian` functions!

#### Quick Intro to AD

At the simplest level, if we have a function from some number to some other
number, we can use `diff` to get its derivative:

``` {.haskell}
myFunc      :: RealFloat a => a -> a
diff myFunc :: RealFloat a => a -> a
```

If we have a function a function from a sized vector to a scalar, we can use
`grad` to get its gradient:

``` {.haskell}
myFunc      :: RealFloat a => V.Vector n a -> a
grad myFunc :: RealFloat a => V.Vector n a -> V.Vector n a
```

Where each of the components in the resulting vector corresponds to the rate of
change of the output according to variations in that component.

We’re using **statically sized vector** type from the
[vector-sized](http://hackage.haskell.org/package/vector-sized) package, where
`V.Vector n a` is a `n`-vector of `a`s – that is, a `V.Vector 3 Double` is a
vector of 3 `Double`s.

We have to use `Vector` (instead of `R`, from *hmatrix*) because automatic
differentiation for gradients requires *some Functor* to work. An `R 5` is
essentially a `V.Vector 5 Double`, except the latter can contain other,
non-Double things – and therefore can be used by *ad* to do its magic.

If we have a function from a sized vector to a (differently) sized vector, we
can use the `jacobian` function to get its jacobian!

``` {.haskell}
myFunc          :: RealFloat a => V.Vector n a -> V.Vector m a
jacobian myFunc :: RealFloat a => V.Vector n a -> V.Vector m (V.Vector n a)
```

Again note the usage of sized vector types, and the fact that our
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrix is represented by a `m`-vector of `n`-vectors.

Finally, we can get a “2nd-order Jacobian” by using `jacobians`, which gives you
a lazily linked chain of successive order Jacobians (stored in successive cells
of a `Cofree`). We only need the second order jacobian, so let’s define a quick
utility function that only gives us the second-order Jacobian:

``` {.haskell}
-- import qualified Control.Comonad        as C
-- import qualified Control.Comonad.Cofree as C

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L52-60
jacobian2
    :: (Traversable f, Functor g, RealFloat a)
    => (forall b. RealFloat b => f b -> g b)
    -> f a
    -> g (f (f a))
jacobian2 f = (fmap . fmap . fmap) C.extract  -- ^ take 2nd deriv
            . (fmap . fmap) C.unwrap          -- ^ drap 1st deriv
            . fmap C.unwrap                   -- ^ drop 0th deriv
            . jacobians f                     -- ^ create list of jacobians
```

If you think of `Cofree` as an infinite linked list (of nested Functors),
`jacobians` returns a linked list of derivative tensors. The first item is the
0th derivative (the actual function value), so we drop it with `unwrap` (like
`tail`) for lists). The second item is the 1st derivative, so we drop it again
using `unwrap`. And finally, we only want the third item (the 2nd derivatives)
so we `extract` it (like `head` for lists).

Finally, we can achieve our goal:

``` {.haskell}
myFunc
    :: RealFloat a => V.Vector n a -> V.Vector m a
jacobian2 myFunc
    :: RealFloat a => V.Vector n a -> V.Vector m (V.Vector n (V.Vector n a))
```

#### Conversion between vector-sized and hmatrix

So some ugly things – we need to write some functions to convert between
*vector-sized* sized vectors and *hmatrix* vectors and matrices:

``` {.haskell}
-- import qualified Data.Vector.Generic.Sized as VG

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L62-69
vec2r :: KnownNat n => V.Vector n Double -> R n
vec2r = fromJust . create . VG.fromSized . VG.convert

r2vec :: KnownNat n => R n -> V.Vector n Double
r2vec = VG.convert . fromJust . VG.toSized . extract

vec2l :: (KnownNat m, KnownNat n) => V.Vector m (V.Vector n Double) -> L m n
vec2l = fromJust . (\rs -> withRows rs exactDims) . toList . fmap vec2r
```

Remember that it is necessary to switch because *ad* requires our vectors to be
*Functors*, but `R` and `L` from *hmatrix* are not your typical Hask Functors.
One nice thing is that because they both use TypeLits to get their sized
parameters, we can get type-safe conversions that preserve their size
information!

Also, even though *ad* gives our second-order Jacobian as an
![m \\times n \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n%20%5Ctimes%20n "m \times n \times n")
tensor, we really want it as a `n`-vector of
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrices – that’s how we interpreted it in our original math. So we just need to
write an function to convert what *ad* gives us to the form we expect:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L71-71
rejacobi :: (KnownNat m, KnownNat n) => V.Vector m (L n n) -> V.Vector n (L m n)
```

#### Using AD to Auto-Derive Systems

Now to make a `System` using just the mass vector, the coordinate conversion
function, and the potential energy function:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L76-90
mkSystem
    :: (KnownNat m, KnownNat n)
    => R m
    -> (forall a. RealFloat a => V.Vector n a -> V.Vector m a)
    -> (forall a. RealFloat a => V.Vector n a -> a)
    -> System m n
mkSystem m f u = System
    { sysInertia       =                        m
    , sysCoords        =      vec2r .           f . r2vec
    , sysJacobian      =      vec2l . jacobian  f . r2vec
    , sysJacobian2     = rejacobi
                       . fmap vec2l . jacobian2 f . r2vec
    , sysPotential     =                        u . r2vec
    , sysPotentialGrad =      vec2r .      grad u . r2vec
    }
```

Now, I hesitate to call this “trivial”…but, it really is a straightforward
direct translation of the definitions, minus some ugly conversions back and
forth using `r2vec`, `vec2r`, and `vec2l`!

1.  The vector of masses is just `m`
2.  The coordinate function is just `f`
3.  The jacobian of the coordinate function is just `jacobian f`
4.  The second-order jacobian of the coordinate function is just `jacobian2 f`
5.  The potential energy function is just `u`
6.  The gradient of the potential energy function is just `grad u`

Take a moment to marvel at the fact that the *ad* library automatically
generated all of these for us and created a perfectly well-formed `System` with
all of its gradients and Jacobians by giving only the coordinate function and
the potential energy function, and in such a clean and concise way!

### Equations of Motion

At this point, we’re ready to write our final equations of motion, which we
found to be given by:

![
\\begin{aligned}
\\dot{q} & = \\hat{K}\^{-1} \\mathbf{p} \\\\
\\dot{p}\_q & = \\mathbf{p}\^T \\hat{K}\^{-1} \\hat{J}\_f\^T \\hat{M}
        \\left\[ \\nabla\_{\\mathbf{q}} \\hat{J}\_f \\right\] \\hat{K}\^{-1} \\mathbf{p}
    - \\nabla\_{\\mathbf{q}} PE(\\mathbf{q})
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%5Cdot%7Bq%7D%20%26%20%3D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%20%5C%5C%0A%5Cdot%7Bp%7D_q%20%26%20%3D%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%0A%20%20%20%20%20%20%20%20%5Cleft%5B%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A%20%20%20%20-%20%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20PE%28%5Cmathbf%7Bq%7D%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
\dot{q} & = \hat{K}^{-1} \mathbf{p} \\
\dot{p}_q & = \mathbf{p}^T \hat{K}^{-1} \hat{J}_f^T \hat{M}
        \left[ \nabla_{\mathbf{q}} \hat{J}_f \right] \hat{K}^{-1} \mathbf{p}
    - \nabla_{\mathbf{q}} PE(\mathbf{q})
\end{aligned}
")

This equation isn’t particularly beautiful, but it’s straightforward to
translate it into Haskell (using
![\\hat{K} = \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D%20%3D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f "\hat{K} = \hat{J}_f^T \hat{M} \hat{J}_f")):

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L92-109
hamilEqns
    :: (KnownNat n, KnownNat m)
    => System m n
    -> Phase n
    -> (R n, R n)       -- dq/dt and dp/dt
hamilEqns s (Phase q p) = (dqdt, dpdt)
  where
    j       = sysJacobian s q
    trj     = tr j
    mHat    = diag (sysInertia s)
    kHat    = trj <> mHat <> j
    kHatInv = inv kHat
    dqdt    = kHatInv #> p
    dpdt    = vec2r bigUglyThing - sysPotentialGrad s q
      where
        bigUglyThing =
          fmap (\j2 -> -p <.> kHatInv #> trj #> mHat #> j2 #> kHatInv #> p)
               (sysJacobian2 s q)
```

Of course, there is no way to get around the big ugly math term in
![\\dot{p}\_q](https://latex.codecogs.com/png.latex?%5Cdot%7Bp%7D_q "\dot{p}_q"),
but at least it is a direct reading of the math!

The result of `hamilEqns` gives the rate of change of the components of our
`Phase n`.

The rest of the processes then is just to “step” `Phase n`. Gradually update it,
following these rate of changes!

### Numerical Integration

This process is known as \[Numerical Integration\]\[integration\], and the
“best” way to do it is quite a big field, so for this article we’re going to be
using the extremely extremely simple [Euler
method](https://en.wikipedia.org/wiki/Euler_method) to progress our system
through time.

Disclaimer – The Euler method is typically a **very very bad** choice for
numerical integration (even though, as popularized in the movie *Hidden
Figures*, it was good enough to [send humans to
space?](http://www.latimes.com/science/sciencenow/la-sci-sn-hidden-figures-katherine-johnson-20170109-story.html)).
We are just choosing it for this article because it’s very simple, conceptually!

The basic idea is that you pick a time-step,
![\\Delta t](https://latex.codecogs.com/png.latex?%5CDelta%20t "\Delta t"), and
update each coordinate as:

![
x(t + \\Delta t) = x(t) + \\dot{x} \\Delta t
](https://latex.codecogs.com/png.latex?%0Ax%28t%20%2B%20%5CDelta%20t%29%20%3D%20x%28t%29%20%2B%20%5Cdot%7Bx%7D%20%5CDelta%20t%0A "
x(t + \Delta t) = x(t) + \dot{x} \Delta t
")

Which makes sense visually if we imagine
![\\dot{x}](https://latex.codecogs.com/png.latex?%5Cdot%7Bx%7D "\dot{x}") as the
“slope” of ![x](https://latex.codecogs.com/png.latex?x "x") – it just means to
follow the slope another
![\\Delta t](https://latex.codecogs.com/png.latex?%5CDelta%20t "\Delta t")
steps. If the slope stays constant, this method is perfectly accurate. The
inaccuracy, of course, happens when the slope changes drastically within that
![\\Delta t](https://latex.codecogs.com/png.latex?%5CDelta%20t "\Delta t") (and
also from the fact that small errors cause errors in the new calculations of
![\\dot{x}](https://latex.codecogs.com/png.latex?%5Cdot%7Bx%7D "\dot{x}"), and
so compound over time)

You can understand this symbolically, as well, by remembering that the
derivative can be approximated by
![\\dot{x} \\approx \\frac{x(t + \\Delta t) - x(t)}{\\Delta t}](https://latex.codecogs.com/png.latex?%5Cdot%7Bx%7D%20%5Capprox%20%5Cfrac%7Bx%28t%20%2B%20%5CDelta%20t%29%20-%20x%28t%29%7D%7B%5CDelta%20t%7D "\dot{x} \approx \frac{x(t + \Delta t) - x(t)}{\Delta t}")
for small
![\\Delta t](https://latex.codecogs.com/png.latex?%5CDelta%20t "\Delta t"), and
so we can do a little bit of symbolic manipulation to get
![x(t + \\Delta t) \\approx \\dot{x} \\Delta t + x(t)](https://latex.codecogs.com/png.latex?x%28t%20%2B%20%5CDelta%20t%29%20%5Capprox%20%5Cdot%7Bx%7D%20%5CDelta%20t%20%2B%20x%28t%29 "x(t + \Delta t) \approx \dot{x} \Delta t + x(t)").

We can directly translate this into Haskell:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L111-119
stepEuler
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ q(t) and p(t)
    -> Phase n          -- ^ q(t + dt) and p(t + dt)
stepEuler s dt ph@(Phase q p) = Phase (q + konst dt * dq) (p + konst dt * dp)
  where
    (dq, dp) = hamilEqns s ph
```

And repeatedly evolve this system as a lazy list:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L121-129
runSystem
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ initial phase
    -> [Phase n]        -- ^ progression of the system using Euler integration
runSystem s dt = go
  where
    go p0 = p0 : go (stepEuler s dt p0)
```

Running with it!
----------------

And…that’s it! Granted, in real life, we would be using a less naive integration
method, but this is essentially the entire process!

Let’s generate the boring system, a 5kg particle in 2D Cartesian Coordinates
under gravity –

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L131-136
simpleSystem :: System 2 2
simpleSystem = mkSystem (vec2 5 5) id pots
  where
    -- U(x,y) = 9.8 * y
    pots :: RealFloat a => V.Vector 2 a -> a
    pots xy = 9.8 * (xy `V.index` 1)
```

If we initialize the particle at position
![\\mathbf{q}\_0 = \\langle 0, 0 \\rangle](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D_0%20%3D%20%5Clangle%200%2C%200%20%5Crangle "\mathbf{q}_0 = \langle 0, 0 \rangle")
and velocity
![\\mathbf{v}\_0 = \\langle 1, 3 \\rangle](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D_0%20%3D%20%5Clangle%201%2C%203%20%5Crangle "\mathbf{v}_0 = \langle 1, 3 \rangle")
(that is,
![v\_x = 1](https://latex.codecogs.com/png.latex?v_x%20%3D%201 "v_x = 1") and
![v\_y = 3](https://latex.codecogs.com/png.latex?v_y%20%3D%203 "v_y = 3")), we
should see something that travels at a constant velocity in x and something that
starts moving “upwards” (in positive y) and eventually reaches a peak and moves
downwards.

We can make our initial configuration:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L138-141
simpleConfig0 :: Config 2
simpleConfig0 = Config { confPositions  = vec2 0 0
                       , confVelocities = vec2 1 3
                       }
```

And then…let it run!

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L143-147
simpleMain :: IO ()
simpleMain =
    mapM_ (disp 2 . phasePositions)  -- position with 2 digits of precision
  . take 25                          -- 25 steps
  $ runSystem simpleSystem 0.1 (toPhase simpleSystem simpleConfig0)
```

We get:

``` {.haskell}
ghci> :l Hamilton.hs
ghci> simpleMain
0     0
0.10  0.30
0.20  0.58
0.30  0.84
0.40  1.08
0.50  1.30
0.60  1.51
0.70  1.69
0.80  1.85
0.90  1.99
1.00  2.12
1.10  2.22
1.20  2.31
1.30  2.37
1.40  2.42
1.50  2.44
1.60  2.45
1.70  2.43
1.80  2.40
1.90  2.35
2.00  2.28
2.10  2.18
2.20  2.07
2.30  1.94
2.40  1.79
```

Exactly what we’d expect! The `x` positions increase steadily, and the `y`
positions increase, slow down, and start decreasing.

We can try a slightly more complicated example that validates all of the work
we’ve done – let’s simulate a simple pendulum. The state of a pendulum is
characterized by one coordinate –
![\\theta](https://latex.codecogs.com/png.latex?%5Ctheta "\theta"), which refers
to the angular (clockwise) from the equilibrium “hanging straight down”
position.
![\\theta = 0](https://latex.codecogs.com/png.latex?%5Ctheta%20%3D%200 "\theta = 0")
corresponds to 6 o’ clock,
![\\theta = \\pi/2](https://latex.codecogs.com/png.latex?%5Ctheta%20%3D%20%5Cpi%2F2 "\theta = \pi/2")
corresponds to 9 o’ clock,
![\\theta = - \\pi / 2](https://latex.codecogs.com/png.latex?%5Ctheta%20%3D%20-%20%5Cpi%20%2F%202 "\theta = - \pi / 2")
corresponds to 3 o’ clock, etc. For a pendulum of length
![l](https://latex.codecogs.com/png.latex?l "l"), we can translate that as
![\\langle x, y \\rangle = \\langle - l sin(\\theta), - l cos(\\theta) \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20x%2C%20y%20%5Crangle%20%3D%20%5Clangle%20-%20l%20sin%28%5Ctheta%29%2C%20-%20l%20cos%28%5Ctheta%29%20%5Crangle "\langle x, y \rangle = \langle - l sin(\theta), - l cos(\theta) \rangle").

Let’s set up that system! We’ll put it under normal gravity potential, again
(![U(x,y) = 9.8 y](https://latex.codecogs.com/png.latex?U%28x%2Cy%29%20%3D%209.8%20y "U(x,y) = 9.8 y")).
Our initial position
![\\theta\_0](https://latex.codecogs.com/png.latex?%5Ctheta_0 "\theta_0") will
be at equilibrium, and our initial angular velocity
![v\_{\\theta 0}](https://latex.codecogs.com/png.latex?v_%7B%5Ctheta%200%7D "v_{\theta 0}")
will be 0.1 radians/sec (clockwise), as we try to induce harmonic motion:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/hamilton1/Hamilton.hs#L150-172
pendulum :: System 2 1
pendulum = mkSystem (vec2 5 5) coords pots      -- 5kg particle
  where
    -- <x,y> = <-0.5 sin(theta), -0.5 cos(theta)>
    -- pendulum of length 0.25
    coords :: RealFloat a => V.Vector 1 a -> V.Vector 2 a
    coords (V.head->theta) = fromJust
                           . V.fromList
                           $ [- 0.25 * sin theta, - 0.25 * cos theta]
    -- U(x,y) = 9.8 * y
    pots :: RealFloat a => V.Vector 1 a -> a
    pots q = 9.8 * (coords q `V.index` 1)

pendulumConfig0 :: Config 1
pendulumConfig0 = Config { confPositions  = 0
                         , confVelocities = 0.1
                         }

pendulumMain :: IO ()
pendulumMain =
    mapM_ (disp 3 . phasePositions)  -- position with 2 digits of precision
  . take 25                          -- 25 steps
  $ runSystem pendulum 0.1 (toPhase pendulum pendulumConfig0)
```

This pendulum should wobble back and forth, ever so slightly, around
equilibrium.

``` {.haskell}
ghci> :l Hamilton.hs
ghci> pendulumMain
0
0.010
0.020
0.029
0.037
0.042
0.045
0.044
0.040
0.032
0.021
0.007
-0.008
-0.023
-0.038
-0.051
-0.061
-0.068
-0.069
-0.065
-0.056
-0.041
-0.022
-0.000
0.023
```

We see our ![\\theta](https://latex.codecogs.com/png.latex?%5Ctheta "\theta")
coordinate increasing, then turning around and decreasing, swinging the other
way past equilibrium, and then turning around and heading back!

Clearly our system is gaining some sort of phantom energy, since it rises up to
0.045 on the left, and then all the way up to -0.69 on the right. Rest assured
that this is simply from the inaccuracies in Euler’s Method, which we used to
integrate!

But, conceptually, this is a success! We *automatically generated equations of
motion for a pendulum*. Sweet!

Wrap-Up
-------

We traveled through the world of physics, math, Haskell, and back again to
achieve something that would have initially seemed like a crazy thought
experiment. But, utilizing Hamiltonian mechanics, we have a system that can
automatically generate equations of motion given your coordinate system and a
potential energy function.

See my [previous
post](https://blog.jle.im/entry/introducing-the-hamilton-library.html) for even
crazier examples – involving multiple objects, double pendulums, and more! And
check out my [hamilton](http://hackage.haskell.org/package/hamilton) library,
which includes demos for exotic interesting systems, rendered graphically on
your terminal.

I realize that this was a lot, so if you have any questions or suggestions for
clarifications, feel free to leave a comment, drop me a
[tweet](https://twitter.com/mstk "Twitter"), or find me on the freenode
*\#haskell* channel (where I usually idle as *jle\`*!)

[^1]: The picture with a time-dependent Hamiltonian is different, but only
    slightly. In the time-dependent case, the system still *tries* to move along
    contour lines at every point in time, but the mountain is constantly
    changing underneath it and the contour lines keep on shifting underneath it.
    Sounds like life!

[^2]: There’s also another perpendicular vector,
    ![\\langle -y, x \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20-y%2C%20x%20%5Crangle "\langle -y, x \rangle"),
    but we do not speak of that.

[^3]: Disclaimer: I am not a surfer

[^4]: ![\\hat{J\_f}](https://latex.codecogs.com/png.latex?%5Chat%7BJ_f%7D "\hat{J_f}")
    is full-rank (meaning
    ![\\hat{K}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D "\hat{K}") is
    invertible) if its rows are linearly independent. This should be the case as
    you don’t have any redundant or duplicate coordinates in your general
    coordinate system.

[^5]: The picture with a time-dependent Hamiltonian is different, but only
    slightly. In the time-dependent case, the system still *tries* to move along
    contour lines at every point in time, but the mountain is constantly
    changing underneath it and the contour lines keep on shifting underneath it.
    Sounds like life!

[^6]: There’s also another perpendicular vector,
    ![\\langle -y, x \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20-y%2C%20x%20%5Crangle "\langle -y, x \rangle"),
    but we do not speak of that.

[^7]: Disclaimer: I am not a surfer

[^8]: ![\\hat{J\_f}](https://latex.codecogs.com/png.latex?%5Chat%7BJ_f%7D "\hat{J_f}")
    is full-rank (meaning
    ![\\hat{K}](https://latex.codecogs.com/png.latex?%5Chat%7BK%7D "\hat{K}") is
    invertible) if its rows are linearly independent. This should be the case as
    you don’t have any redundant or duplicate coordinates in your general
    coordinate system.
