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

1.  Basic concepts of multivariable calculus (like partial and
    total derivatives).
2.  Concepts of linear algebra (like dot products, matrix multiplication, and
    matrix inverses)

No physics knowledge is assumed, but knowing a little bit of first semester
physics would help you gain a bit more of an appreciation for the end result!

Hamiltonian Mechanics
---------------------

As mentioned in the previous post, Hamiltonian mechanics is a re-imagining of
dynamics and mechanics (think “the world post-$F = m a$”) that not only opened
up new doors to solving problems in classical, but also ended up being the right
angle of viewing the world to unlock statistical mechanics and thermodynamics,
and later even quantum mechanics.

Hamiltonian mechanics lets you parameterize your system’s “position” in
arbitrary ways (like the angle of rotation, for pendulum problems) and then
posits that the full state of the system exists in something called *phase
space*, and that the system’s dynamics is its motion through phase space that is
dictated by the geometry of the *Hamiltonian* of that phase space.

The system’s *Hamiltonian* is a $\mathbb{R}^{2n} \rightarrow \mathbb{R}$
function on phase space (where $n$ is the number of coordinates parameterizing
your system) to $\mathbb{R}$. For a time-independent system, the picture of the
dynamics is pretty simple: the system moves along the *contour lines* of the
*Hamiltonian* – the lines of equal “height”.

![Example of contour lines of a $\mathbb{R}^2 \rightarrow \mathbb{R}$ function –
the elevation of land. From the [Ordinace
Survey](https://www.ordnancesurvey.co.uk/blog/2015/11/map-reading-skills-making-sense-of-contour-lines/)
website.](/img/entries/hamilton/contour-lines.jpg "Contour lines")

In the example above, if we imagine that phase space is the 2D location, then
the *Hamiltonian* is the mountain. And for a system dropped anywhere on the
mountain, its motion would be along the contour lines. For example, if a system
started somewhere along the 10 contour line, it would begin to oscillate the
entire phase space along the 10 contour line.[^1]

*Every* [smooth](https://en.wikipedia.org/wiki/Smooth_jazz)
$\mathbb{R}^{2n} \rightarrow \mathbb{R}$ function on phase space can be used as
a Hamiltonian to describe the physics of some system. So, given any “mountain
range” on phase space, any “elevation map” or real-valued function on phase
space, you can treat it as a description of the dynamics of some physical
system.

The *trick*, then, to using Hamiltonian dynamics to model your system, is:

1.  Finding the phase space to describe your system. This can be done based on
    any continuous parameterization of your system (“generalized coordinates”),
    like angles of pendulums and so on.

2.  Finding the Hamiltonian on that phase space to describe your system.

And then Hamilton’s dynamics will give you the rest! All you do is “follow the
contour lines” on that Hamiltonian!

### Phase Space

So far the only thing I’ve really said in detail is that if your system’s state
has $n$ parameters, then the corresponding phase space is $2n$-dimensional (and
that Hamiltonian mechanics is somehow about systems moving around in phase
space). *Phase space* is a $2n$-dimensional space parameterized by:

1.  All of the current values of the $n$ parameters (“generalized coordinates”)
2.  All of the current “generalized momenta” of those $n$ parameters

So if you were parameterizing your pendulum system by, say, the angle of the
pendulum, the phase space would be the current angle of the pendulum along with
the current “generalized momentum” associated with the angle of the pendulum.
What exactly *is* generalized momentum? We’ll go over calculating it eventually,
but what does it represent…*physically*?

I could give you some spiel here about the underlying Lie algebra of the Lie
group associated with the generalized coordinates, but I don’t think that it
would be very intuitively appealing in a physical sense. But what I *can* say is
that the generalized momenta associated with (“conjugate to”) certain sets of
familiar coordinates yield things that we typically call “momenta”:

1.  The momentum conjugate to normal Cartesian coordinates is just our normal
    run-of-the-mill *linear momentum* (in the $\mathbf{p} = m \mathbf{v}$) from
    first semester physics.

2.  The momentum conjugate to the angle $\theta$ in polar coordinates is
    *angular momentum* ($\mathbf{L} = m \mathbf{r} \times \mathbf{v}$, or
    $L = m r^2 \dot{\theta}$) from first semester physics.

    The momentum conjugate to the radial coordinate $r$ in polar coordinates is
    also just boring old linear momentum $p_r = m \dot{r}$.

So, it’s our normal momentum (for linear and polar coordinates) *generalized* to
arbitrary coordinates.

### Hamiltonian Dynamics

I’ve explained Hamiltonian dynamics for time-independent Hamiltonians as “follow
the contour lines”. If you remember your basic multi-variable calculus course,
you’ll know that the line of “steepest ascent” is the gradient. If we call the
Hamiltonian $\mathcal{H}(\mathbf{q},\mathbf{p})$ (where $\mathbf{q}$ is the
vector of positions and $\mathbf{p}$ is the vector of momenta), then the
direction of steepest ascent is
$\left \langle \frac{\partial}{\partial \mathbf{q}} \mathcal{H}(\mathbf{q},\mathbf{p}), \frac{\partial}{\partial \mathbf{p}} \mathcal{H}(\mathbf{q},\mathbf{p}) \right \rangle$.

But we want to move along the *contour lines*…and these are the lines
*perpendicular* to the direction of steepest descent. The vector perpendicular
to $\langle x, y \rangle$ is $\langle y, -x \rangle$, so we just derived the
actual Hamiltonian equations of motion: just move in the direction perpendicular
to the steepest descent!

$$
\dot{q} = \frac{\partial}{\partial p_q} \mathcal{H}(\mathbf{q},\mathbf{p})
$$

$$
\dot{p_q} = - \frac{\partial}{\partial q} \mathcal{H}(\mathbf{q},\mathbf{p})
$$

Which holds for every generalized coordinate $q$, where $p_q$ is the momentum
conjugate to that coordinate.

Essentially, these give you “updating functions” for $q$ and $p_q$ – given
$\mathcal{H}(\mathbf{q},\mathbf{p})$, you have a way to “update” the particle’s
position in phase space. Just take the partial derivatives of $\mathcal{H}$ at
every step in time! To update $q$, nudge it by
$\frac{\partial}{\partial p_q} \mathcal{H}(\mathbf{q},\mathbf{p})$. To update
$p_q$, nudge it by
$\frac{\partial}{\partial q} \mathcal{H}(\mathbf{q},\mathbf{p})$!

This picture is appealing to me in a visceral way because it sort of seems like
the system is “surfing” along the Hamiltonian’s contour lines. It’s being
“pushed” *faster* when the Hamiltonian is steeper, and slower when it’s more
shallow. If you have super steep Hamiltonians, you’ll just zip right through
phase space, but if you have shallow Hamiltonians, you’ll take a comfy glide
along that contour line you’re on. I can apply all my intuition as a surfer[^2]
to Hamiltonian mechanics!

Hamiltonian Dynamics and Physical Systems
-----------------------------------------

Earlier I mentioned that the two steps for applying Hamiltonian mechanics to
your system was figuring out your system’s conjugate momenta and the appropriate
Hamiltonian. Let’s get onto that. I’m going to make a couple of simplifying
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

We’ll define the momentum conjugate to coordinate $q$ as

$$
\frac{\partial}{\partial \dot{q}} KE(\dot{\mathbf{q}})
$$

Where $KE(\dot{\mathbf{q}))$ is the kinetic energy of the system, which is an
explicit function on the rate of changes of the coordinates. For example, for
normal Cartesian coordinates in one dimension,
$KE(\dot{x}) = \frac{1}{2} m \dot{x}^2$. So the momentum conjugate to $x$ is:

$$
p_x = \frac{\partial}{\partial \dot{x}} \frac{1}{2} m \dot{x}^2 = m \dot{x}
$$

Just linear momentum, like I claimed before!

Alright, now let’s generalize this to arbitrary coordinates. In general, for
*Cartesian* coordinates, the kinetic energy will always be

$$
KE(\dot{\mathbf{x}}) = \frac{1}{2} \left[ m_1 x_1^2 + m_2 x_2^2 + m_3 x_3^2 \dots \right]
$$

Where $m$ is the inertia associated with each coordinate…for example, if there’s
an object of mass $m$ at $\langle x_1, x_2 \rangle$, then $m_1 = m_2 = m$.

To make things more convenient, we’ll treat this as a quadratic form over an
inertia matrix:

$$
KE(\dot{\mathbf{x}}) = \frac{1}{2} \mathbf{x}^T \hat{M} \mathbf{x}
$$

[^1]: The picture with a time-dependent Hamiltonian is different, but only
    slightly. In the time-dependent case, the system still *tries* to move along
    contour lines at every point in time, but the mountain is constantly
    changing underneath it and the contour lines keep on shifting underneath it.
    Sounds like life!

[^2]: Disclaimer: I am not a surfer
