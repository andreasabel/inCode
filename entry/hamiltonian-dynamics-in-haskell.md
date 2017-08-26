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
![\\left \\langle \\frac{\\partial}{\\partial \\mathbf{q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p}), \\frac{\\partial}{\\partial \\mathbf{p}} \\mathcal{H}(\\mathbf{q},\\mathbf{p}) \\right \\rangle](https://latex.codecogs.com/png.latex?%5Cleft%20%5Clangle%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cmathbf%7Bq%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%2C%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20%5Cmathbf%7Bp%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%5Cright%20%5Crangle "\left \langle \frac{\partial}{\partial \mathbf{q}} \mathcal{H}(\mathbf{q},\mathbf{p}), \frac{\partial}{\partial \mathbf{p}} \mathcal{H}(\mathbf{q},\mathbf{p}) \right \rangle").

But we want to move along the *contour lines*…and these are the lines
*perpendicular* to the direction of steepest descent. The vector perpendicular
to
![\\langle x, y \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20x%2C%20y%20%5Crangle "\langle x, y \rangle")
is
![\\langle y, -x \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20y%2C%20-x%20%5Crangle "\langle y, -x \rangle"),[^2]
so we just derived the actual Hamiltonian equations of motion: just move in the
direction perpendicular to the steepest ascent!

![
\\dot{q} = \\frac{\\partial}{\\partial p\_q} \\mathcal{H}(\\mathbf{q},\\mathbf{p})
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7Bq%7D%20%3D%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20p_q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%0A "
\dot{q} = \frac{\partial}{\partial p_q} \mathcal{H}(\mathbf{q},\mathbf{p})
")

![
\\dot{p\_q} = - \\frac{\\partial}{\\partial q} \\mathcal{H}(\\mathbf{q},\\mathbf{p})
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7Bp_q%7D%20%3D%20-%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%0A "
\dot{p_q} = - \frac{\partial}{\partial q} \mathcal{H}(\mathbf{q},\mathbf{p})
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

Just linear momentum, like I claimed before!

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
KE(\\dot{\\mathbf{x}}) = \\frac{1}{2} \\dot{\\mathbf{x}\^T} \\hat{M} \\dot{\\mathbf{x}}
](https://latex.codecogs.com/png.latex?%0AKE%28%5Cdot%7B%5Cmathbf%7Bx%7D%7D%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Cdot%7B%5Cmathbf%7Bx%7D%5ET%7D%20%5Chat%7BM%7D%20%5Cdot%7B%5Cmathbf%7Bx%7D%7D%0A "
KE(\dot{\mathbf{x}}) = \frac{1}{2} \dot{\mathbf{x}^T} \hat{M} \dot{\mathbf{x}}
")

Where ![\\hat{M}](https://latex.codecogs.com/png.latex?%5Chat%7BM%7D "\hat{M}")
is the [diagonal matrix](https://en.wikipedia.org/wiki/Diagonal_matrix) whose
entries are the masses of each coordinate, and
![\\dot{\\mathbf{x}}](https://latex.codecogs.com/png.latex?%5Cdot%7B%5Cmathbf%7Bx%7D%7D "\dot{\mathbf{x}}")
is the column vector of all of the (Cartesian) coordinates,
![\\left\[ \\dot{x\_1}\\, \\dot{x\_2}\\, \\dot{x\_3}\\, \\dots \\right\]\^T](https://latex.codecogs.com/png.latex?%5Cleft%5B%20%5Cdot%7Bx_1%7D%5C%2C%20%5Cdot%7Bx_2%7D%5C%2C%20%5Cdot%7Bx_3%7D%5C%2C%20%5Cdots%20%5Cright%5D%5ET "\left[ \dot{x_1}\, \dot{x_2}\, \dot{x_3}\, \dots \right]^T").

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
![\\dot{x\_1}](https://latex.codecogs.com/png.latex?%5Cdot%7Bx_1%7D "\dot{x_1}")
is the [total derivative](https://en.wikipedia.org/wiki/Total_derivative) of
![x\_1](https://latex.codecogs.com/png.latex?x_1 "x_1") with respect to time:

![
\\dot{x\_1} = \\frac{\\partial f\_1}{\\partial q\_1} \\dot{q\_1} +
    \\frac{\\partial f\_1}{\\partial q\_2} \\dot{q\_2} +
    \\frac{\\partial f\_1}{\\partial q\_3} \\dot{q\_3} + \\dots
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7Bx_1%7D%20%3D%20%5Cfrac%7B%5Cpartial%20f_1%7D%7B%5Cpartial%20q_1%7D%20%5Cdot%7Bq_1%7D%20%2B%0A%20%20%20%20%5Cfrac%7B%5Cpartial%20f_1%7D%7B%5Cpartial%20q_2%7D%20%5Cdot%7Bq_2%7D%20%2B%0A%20%20%20%20%5Cfrac%7B%5Cpartial%20f_1%7D%7B%5Cpartial%20q_3%7D%20%5Cdot%7Bq_3%7D%20%2B%20%5Cdots%0A "
\dot{x_1} = \frac{\partial f_1}{\partial q_1} \dot{q_1} +
    \frac{\partial f_1}{\partial q_2} \dot{q_2} +
    \frac{\partial f_1}{\partial q_3} \dot{q_3} + \dots
")

Or, in short:

![
\\dot{x\_i} = \\sum\_{j = 1}\^n \\frac{\\partial f\_i}{\\partial q\_j} \\dot{q\_j}
](https://latex.codecogs.com/png.latex?%0A%5Cdot%7Bx_i%7D%20%3D%20%5Csum_%7Bj%20%3D%201%7D%5En%20%5Cfrac%7B%5Cpartial%20f_i%7D%7B%5Cpartial%20q_j%7D%20%5Cdot%7Bq_j%7D%0A "
\dot{x_i} = \sum_{j = 1}^n \frac{\partial f_i}{\partial q_j} \dot{q_j}
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

Easy peasy. But finding the partial derivatives with respect to
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
![\\frac{\\partial}{\\partial t} A\^{-1} = - A\^{-1} \\left\[ \\frac{\\partial}{\\partial t} A \\right\] A\^{-1}](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20t%7D%20A%5E%7B-1%7D%20%3D%20-%20A%5E%7B-1%7D%20%5Cleft%5B%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20t%7D%20A%20%5Cright%5D%20A%5E%7B-1%7D "\frac{\partial}{\partial t} A^{-1} = - A^{-1} \left[ \frac{\partial}{\partial t} A \right] A^{-1}"),
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
\\frac{\\partial}{\\partial q} \\left\[ \\hat{J}\_f\^T \\hat{M} \\hat{J}\_f \\right\] =
    2 \\hat{J}\_f\^T \\hat{M} \\left\[ \\frac{\\partial}{\\partial q} \\hat{J}\_f \\right\]
](https://latex.codecogs.com/png.latex?%0A%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20%5Cleft%5B%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%3D%0A%20%20%20%202%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%20%5Cleft%5B%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%0A "
\frac{\partial}{\partial q} \left[ \hat{J}_f^T \hat{M} \hat{J}_f \right] =
    2 \hat{J}_f^T \hat{M} \left[ \frac{\partial}{\partial q} \hat{J}_f \right]
")

![\\frac{\\partial}{\\partial q} \\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q%7D%20%5Chat%7BJ%7D_f "\frac{\partial}{\partial q} \hat{J}_f")
(an
![m \\times n](https://latex.codecogs.com/png.latex?m%20%5Ctimes%20n "m \times n")
matrix, like
![\\hat{J}\_f](https://latex.codecogs.com/png.latex?%5Chat%7BJ%7D_f "\hat{J}_f"))
represents the *second derivatives* of
![f](https://latex.codecogs.com/png.latex?f "f") – the derivative of the
derivatives. And with that, we have our final expression for
![\\nabla\_{\\mathbf{q}} \\mathcal{H}(\\mathbf{q},\\mathbf{p})](https://latex.codecogs.com/png.latex?%5Cnabla_%7B%5Cmathbf%7Bq%7D%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29 "\nabla_{\mathbf{q}} \mathcal{H}(\mathbf{q},\mathbf{p})"):

![
\\frac{\\partial}{\\partial q\_i} \\mathcal{H}(\\mathbf{q},\\mathbf{p}) =
    - \\mathbf{p}\^T \\hat{K}\^{-1} \\hat{J}\_f\^T \\hat{M}
        \\left\[ \\frac{\\partial}{\\partial q\_i} \\hat{J}\_f \\right\] \\hat{K}\^{-1} \\mathbf{p}
    + \\frac{\\partial}{\\partial q\_i} PE(\\mathbf{q})
](https://latex.codecogs.com/png.latex?%0A%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Cmathcal%7BH%7D%28%5Cmathbf%7Bq%7D%2C%5Cmathbf%7Bp%7D%29%20%3D%0A%20%20%20%20-%20%5Cmathbf%7Bp%7D%5ET%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Chat%7BJ%7D_f%5ET%20%5Chat%7BM%7D%0A%20%20%20%20%20%20%20%20%5Cleft%5B%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20%5Chat%7BJ%7D_f%20%5Cright%5D%20%5Chat%7BK%7D%5E%7B-1%7D%20%5Cmathbf%7Bp%7D%0A%20%20%20%20%2B%20%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20PE%28%5Cmathbf%7Bq%7D%29%0A "
\frac{\partial}{\partial q_i} \mathcal{H}(\mathbf{q},\mathbf{p}) =
    - \mathbf{p}^T \hat{K}^{-1} \hat{J}_f^T \hat{M}
        \left[ \frac{\partial}{\partial q_i} \hat{J}_f \right] \hat{K}^{-1} \mathbf{p}
    + \frac{\partial}{\partial q_i} PE(\mathbf{q})
")

Where
![\\frac{\\partial}{\\partial q\_i} PE(\\mathbf{q})](https://latex.codecogs.com/png.latex?%5Cfrac%7B%5Cpartial%7D%7B%5Cpartial%20q_i%7D%20PE%28%5Cmathbf%7Bq%7D%29 "\frac{\partial}{\partial q_i} PE(\mathbf{q})")
is just the ![i](https://latex.codecogs.com/png.latex?i "i")th component of the
gradient of ![PE](https://latex.codecogs.com/png.latex?PE "PE").

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
    ![m](https://latex.codecogs.com/png.latex?m "m") Cartesian coordinates
2.  A function
    ![\\mathbb{R}\^n \\rightarrow \\mathbb{R}\^m](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D%5Em "\mathbb{R}^n \rightarrow \mathbb{R}^m")
    to convert the generalized coordinates
    (![\\mathbb{R\^n}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%5En%7D "\mathbb{R^n}"))
    to Cartesian coordinates
    (![\\mathbb{R}\^m](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5Em "\mathbb{R}^m"))
3.  The potential energy function
    ![\\mathbb{R}\^n \\rightarrow \\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5En%20%5Crightarrow%20%5Cmathbb%7BR%7D "\mathbb{R}^n \rightarrow \mathbb{R}")
    in the generalized coordinates
    (![\\mathbb{R\^n}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%5En%7D "\mathbb{R^n}"))

From these alone, we can derive the equations of motion for the particles in
phase space as a system of first-order ODEs using the process described above.
Then, given an initial phase space position, we can use any ol’ first order ODE
integrator (like the great ones from the [GNU Scientific
Library](https://www.gnu.org/software/gsl/)) to simulate our system’s motion
through phase space. That is, to “surf the hamiltonian waves in phase space”, so
to speak.

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
