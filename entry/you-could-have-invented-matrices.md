You Could Have Invented Matrices!
=================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/you-could-have-invented-matrices.html)

You could have invented matrices!

Let's talk about vectors. A **vector** (denoted as
![\\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D "\mathbf{v}"),
a lower-case bold italicized letter) is an element in a **vector space**, which
means that it can be "scaled", like
![c \\mathbf{v}](https://latex.codecogs.com/png.latex?c%20%5Cmathbf%7Bv%7D "c \mathbf{v}")
(the ![c](https://latex.codecogs.com/png.latex?c "c") is called a "scalar" ---
creative name, right?) and added, like
![\\mathbf{v} + \\mathbf{u}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D%20%2B%20%5Cmathbf%7Bu%7D "\mathbf{v} + \mathbf{u}").

In order for vector spaces and their operations to be valid, they just have to
obey some common-sense rules (like associativity, commutativity, distributivity,
etc.) that allow us to make meaningful conclusions.[^1]

Dimensionality
--------------

One neat thing about vector spaces is that *some* of them (if you're lucky) have
a notion of **dimensionality**. We say that a vector space is N-dimensional if
there exists N "basis" vectors
![\\mathbf{e}\_1, \\mathbf{e}\_2 \\ldots \\mathbf{e}\_N](https://latex.codecogs.com/png.latex?%5Cmathbf%7Be%7D_1%2C%20%5Cmathbf%7Be%7D_2%20%5Cldots%20%5Cmathbf%7Be%7D_N "\mathbf{e}_1, \mathbf{e}_2 \ldots \mathbf{e}_N")
where *any* vector can be described as scaled sums of all of them, and that N is
the lowest number of basis vectors needed. For example, if a vector space is
3-dimensional, then it means that *any* vector
![\\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D "\mathbf{v}")
in that space can be broken down as:

![
\\mathbf{v} = a \\mathbf{e}\_1 + b \\mathbf{e}\_2 + c \\mathbf{e}\_3
](https://latex.codecogs.com/png.latex?%0A%5Cmathbf%7Bv%7D%20%3D%20a%20%5Cmathbf%7Be%7D_1%20%2B%20b%20%5Cmathbf%7Be%7D_2%20%2B%20c%20%5Cmathbf%7Be%7D_3%0A "
\mathbf{v} = a \mathbf{e}_1 + b \mathbf{e}_2 + c \mathbf{e}_3
")

Where ![a](https://latex.codecogs.com/png.latex?a "a"),
![b](https://latex.codecogs.com/png.latex?b "b"), and
![c](https://latex.codecogs.com/png.latex?c "c") are scalars.

Dimensionality is really a statement about being able to decompose any vector in
that vector space into a useful set of bases. For a 3-dimensional vector space,
you need at least 3 vectors to make a bases that can reproduce *any* vector in
your space.

In physics, we often treat reality as taking place in a three-dimensional vector
space. The basis vectors are often called
![\\hat{\\mathbf{i}}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cmathbf%7Bi%7D%7D "\hat{\mathbf{i}}"),
![\\hat{\\mathbf{j}}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cmathbf%7Bj%7D%7D "\hat{\mathbf{j}}"),
and
![\\hat{\\mathbf{k}}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cmathbf%7Bk%7D%7D "\hat{\mathbf{k}}"),
and so we say that we can describe our 3D physics vectors as
![\\mathbf{v} = v\_x \\hat{\\mathbf{i}} + v\_y \\hat{\\mathbf{j}} + v\_x \\hat{\\mathbf{k}}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D%20%3D%20v_x%20%5Chat%7B%5Cmathbf%7Bi%7D%7D%20%2B%20v_y%20%5Chat%7B%5Cmathbf%7Bj%7D%7D%20%2B%20v_x%20%5Chat%7B%5Cmathbf%7Bk%7D%7D "\mathbf{v} = v_x \hat{\mathbf{i}} + v_y \hat{\mathbf{j}} + v_x \hat{\mathbf{k}}")

### Encoding

One neat thing that physicists take advantage of all the time is that if we
*agree* on a set of basis vectors and a specific ordering, we can actually
*encode* any vector
![\\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D "\mathbf{v}")
in terms of those basis vectors.

So in physics, we can say "Let's encode vectors in terms of
![\\hat{\\mathbf{i}}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cmathbf%7Bi%7D%7D "\hat{\mathbf{i}}"),
![\\hat{\\mathbf{j}}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cmathbf%7Bj%7D%7D "\hat{\mathbf{j}}"),
and
![\\hat{\\mathbf{k}}](https://latex.codecogs.com/png.latex?%5Chat%7B%5Cmathbf%7Bk%7D%7D "\hat{\mathbf{k}}"),
in that order." Then, we can *write*
![\\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D "\mathbf{v}")
as
![\\langle v\_x, v\_y, v\_z \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20v_x%2C%20v_y%2C%20v_z%20%5Crangle "\langle v_x, v_y, v_z \rangle"),
and understand that we really
mean![\\mathbf{v} = v\_x \\hat{\\mathbf{i}} + v\_y \\hat{\\mathbf{j}} + v\_x \\hat{\\mathbf{k}}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D%20%3D%20v_x%20%5Chat%7B%5Cmathbf%7Bi%7D%7D%20%2B%20v_y%20%5Chat%7B%5Cmathbf%7Bj%7D%7D%20%2B%20v_x%20%5Chat%7B%5Cmathbf%7Bk%7D%7D "\mathbf{v} = v_x \hat{\mathbf{i}} + v_y \hat{\mathbf{j}} + v_x \hat{\mathbf{k}}").

Note that
![\\langle v\_x, v\_y, v\_z \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20v_x%2C%20v_y%2C%20v_z%20%5Crangle "\langle v_x, v_y, v_z \rangle")
is **not** the same thing as the **vector**
![\\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D "\mathbf{v}").
It is *an encoding* of that vector, that only makes sense once we choose to
*agree* on a specific set of basis.

For an N-dimensional vector space, it means that, with a minimum of N items, we
can represent any vector in that space. And, if we agree on those N items, we
can devise an encoding, such that:

![
\\langle v\_1, v\_2 \\dots v\_N \\rangle
](https://latex.codecogs.com/png.latex?%0A%5Clangle%20v_1%2C%20v_2%20%5Cdots%20v_N%20%5Crangle%0A "
\langle v_1, v_2 \dots v_N \rangle
")

will *represent* the vector:

![
v\_1 \\mathbf{e}\_1 + v\_2 \\mathbf{e}\_2 + \\ldots + v\_N \\mathbf{e}\_N
](https://latex.codecogs.com/png.latex?%0Av_1%20%5Cmathbf%7Be%7D_1%20%2B%20v_2%20%5Cmathbf%7Be%7D_2%20%2B%20%5Cldots%20%2B%20v_N%20%5Cmathbf%7Be%7D_N%0A "
v_1 \mathbf{e}_1 + v_2 \mathbf{e}_2 + \ldots + v_N \mathbf{e}_N
")

Note that what this encoding represents is *completely dependent* on what
![\\mathbf{e}\_1, \\mathbf{e}\_2 \\ldots \\mathbf{e}\_N](https://latex.codecogs.com/png.latex?%5Cmathbf%7Be%7D_1%2C%20%5Cmathbf%7Be%7D_2%20%5Cldots%20%5Cmathbf%7Be%7D_N "\mathbf{e}_1, \mathbf{e}_2 \ldots \mathbf{e}_N")
we pick, and in what order. The basis vectors we pick are arbitrary, and
determine what our encoding looks like.

To highlight this, note that the same vector
![\\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D "\mathbf{v}")
has many different potential encodings --- all you have to do is pick a
different set of basis vectors, or even just re-arrange the ones you already
have. However, all of those encodings correspond go the same vector
![\\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D "\mathbf{v}").

One interesting consequence of this is that any N-dimensional vector space whose
scalars are in
![\\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D "\mathbb{R}")
is actually isomorphic to
![\\mathbf{R}\^N](https://latex.codecogs.com/png.latex?%5Cmathbf%7BR%7D%5EN "\mathbf{R}^N")
--- the vector space of N-tuples of real numbers. Because of this, we often call
*all* N-dimensional vector spaces (whose scalars are in
![\\mathbb{R}](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D "\mathbb{R}"))
as
![\\mathbb{R}\^N](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5EN "\mathbb{R}^N").
You will often hear physicists saying that the three-dimensional vector spaces
they use are
![\\mathbb{R}\^3](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5E3 "\mathbb{R}^3").
However, what they really mean is that their vectors are isomorphic to
![\\mathbb{R}\^3](https://latex.codecogs.com/png.latex?%5Cmathbb%7BR%7D%5E3 "\mathbb{R}^3").

Linear Transformations
----------------------

Now, one of the most interesting things in mathematics is the idea of the
**linear transformation**. Linear transformations are useful to study because:

1.  They are ubiquitious. They come up everywhere in engineering, physics,
    mathematics, data science, economics, and pretty much any mathematical
    theory. And there are even more situations which can be *approximated* by
    linear transformations.
2.  They are mathematically very nice to work with and study, in practice.

A linear transformation,
![f(\\mathbf{x})](https://latex.codecogs.com/png.latex?f%28%5Cmathbf%7Bx%7D%29 "f(\mathbf{x})"),
is a function that "respects" addition and scaling:

![
\\begin{aligned}
f(c\\mathbf{x}) & = c f(\\mathbf{x}) \\\\
f(\\mathbf{x} + \\mathbf{y}) & = f(\\mathbf{x}) + f(\\mathbf{y})
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0Af%28c%5Cmathbf%7Bx%7D%29%20%26%20%3D%20c%20f%28%5Cmathbf%7Bx%7D%29%20%5C%5C%0Af%28%5Cmathbf%7Bx%7D%20%2B%20%5Cmathbf%7By%7D%29%20%26%20%3D%20f%28%5Cmathbf%7Bx%7D%29%20%2B%20f%28%5Cmathbf%7By%7D%29%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
f(c\mathbf{x}) & = c f(\mathbf{x}) \\
f(\mathbf{x} + \mathbf{y}) & = f(\mathbf{x}) + f(\mathbf{y})
\end{aligned}
")

This means that if you scale the input, the output is scaled by the same amount.
And also, if you transform the sum of two things, it's the same as the sum of
the transformed things (it "distributes").

Note that I snuck in vector notation, because the concept of vectors are
*perfectly suited* for studying linear transformations. That's because talking
about linear transformations requires talking about scaling and adding,
and...hey, that's just exactly what vectors have!

From now on, we'll talk about linear transformations specifically on
*N-dimensional vector spaces* (vector spaces that have dimensions and bases we
can use).

### Studying linear transformations

From first glance, a linear transformation's description doesn't look too useful
or analyzable. All you have is
![f(\\mathbf{v})](https://latex.codecogs.com/png.latex?f%28%5Cmathbf%7Bv%7D%29 "f(\mathbf{v})").
It could be anything! Right? Just a black box function?

Actually, we can exploit its linearity and the fact that we're in a vector space
with a basis to analyze the heck out of any linear transformation, and see that
all of them actually have to follow some specific pattern.

Let's say that
![A(\\mathbf{x})](https://latex.codecogs.com/png.latex?A%28%5Cmathbf%7Bx%7D%29 "A(\mathbf{x})")
is a linear transformation from N-dimensional vector space
![V](https://latex.codecogs.com/png.latex?V "V") to M-dimensional vector space
![U](https://latex.codecogs.com/png.latex?U "U"). That is,
![A : V \\rightarrow U](https://latex.codecogs.com/png.latex?A%20%3A%20V%20%5Crightarrow%20U "A : V \rightarrow U").

Because we know that any vector
![\\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D "\mathbf{v}")
in ![V](https://latex.codecogs.com/png.latex?V "V") can be decomposed as
![v\_1 \\mathbf{e}\_1 + v\_2 \\mathbf{e}\_2 + \\ldots v\_n \\mathbf{e}\_N](https://latex.codecogs.com/png.latex?v_1%20%5Cmathbf%7Be%7D_1%20%2B%20v_2%20%5Cmathbf%7Be%7D_2%20%2B%20%5Cldots%20v_n%20%5Cmathbf%7Be%7D_N "v_1 \mathbf{e}_1 + v_2 \mathbf{e}_2 + \ldots v_n \mathbf{e}_N"),
we really can just look at how a transformation
![A](https://latex.codecogs.com/png.latex?A "A") acts on this decomposition. For
example, if ![V](https://latex.codecogs.com/png.latex?V "V") is
three-dimensional:

![
A(\\mathbf{v}) = A(v\_1 \\mathbf{e}\_1 + v\_2 \\mathbf{e}\_2 + v\_3 \\mathbf{e}\_3)
](https://latex.codecogs.com/png.latex?%0AA%28%5Cmathbf%7Bv%7D%29%20%3D%20A%28v_1%20%5Cmathbf%7Be%7D_1%20%2B%20v_2%20%5Cmathbf%7Be%7D_2%20%2B%20v_3%20%5Cmathbf%7Be%7D_3%29%0A "
A(\mathbf{v}) = A(v_1 \mathbf{e}_1 + v_2 \mathbf{e}_2 + v_3 \mathbf{e}_3)
")

Hm. Doesn't seem very insightful, does it?

### A simple definition

But! We can exploit the linearity of
![A](https://latex.codecogs.com/png.latex?A "A") (that it distributes and
scales) to rewrite that as:

![
A(\\mathbf{v}) = v\_1 A(\\mathbf{e}\_1) + v\_2 A(\\mathbf{e}\_2) + v\_3 A(\\mathbf{e}\_3)
](https://latex.codecogs.com/png.latex?%0AA%28%5Cmathbf%7Bv%7D%29%20%3D%20v_1%20A%28%5Cmathbf%7Be%7D_1%29%20%2B%20v_2%20A%28%5Cmathbf%7Be%7D_2%29%20%2B%20v_3%20A%28%5Cmathbf%7Be%7D_3%29%0A "
A(\mathbf{v}) = v_1 A(\mathbf{e}_1) + v_2 A(\mathbf{e}_2) + v_3 A(\mathbf{e}_3)
")

Wow! This just means that, to study
![A](https://latex.codecogs.com/png.latex?A "A"), **all you need to study** is
how ![A](https://latex.codecogs.com/png.latex?A "A") acts on our *basis
vectors*. If you know how ![A](https://latex.codecogs.com/png.latex?A "A") acts
on our basis vectors of our vector space, that's really "all there is" about
![A](https://latex.codecogs.com/png.latex?A "A")! Not such a black box anymore!

That is, if I were to ask you, "Hey, what is
![A](https://latex.codecogs.com/png.latex?A "A") like?", *all you'd have to tell
me* is the result of
![A(\\mathbf{e}\_1)](https://latex.codecogs.com/png.latex?A%28%5Cmathbf%7Be%7D_1%29 "A(\mathbf{e}_1)"),
![A(\\mathbf{e}\_2](https://latex.codecogs.com/png.latex?A%28%5Cmathbf%7Be%7D_2 "A(\mathbf{e}_2"),
and
![A(\\mathbf{e}\_3)](https://latex.codecogs.com/png.latex?A%28%5Cmathbf%7Be%7D_3%29 "A(\mathbf{e}_3)").
Just give me those three *vectors*, and we *uniquely determine
![A](https://latex.codecogs.com/png.latex?A "A")*.

To put in another way, *any linear transformation* from a three-dimensional
vector space is *uniquely* characterized by *three vectors*:
![A(\\mathbf{e}\_1)](https://latex.codecogs.com/png.latex?A%28%5Cmathbf%7Be%7D_1%29 "A(\mathbf{e}_1)"),
![A(\\mathbf{e}\_2)](https://latex.codecogs.com/png.latex?A%28%5Cmathbf%7Be%7D_2%29 "A(\mathbf{e}_2)"),
and
![A(\\mathbf{e}\_3)](https://latex.codecogs.com/png.latex?A%28%5Cmathbf%7Be%7D_3%29 "A(\mathbf{e}_3)").

Those three vectors *completely define*
![A](https://latex.codecogs.com/png.latex?A "A").

In general, we see that *any linear transformation* from an N-dimensional vector
space can be *completely defined* by N vectors: the N results of that
transformation on each of N basis vectors we choose.

### Enter the Matrix

Okay, so how do we "give"/define/state those N vectors?

Well, recall that the result of ![A](https://latex.codecogs.com/png.latex?A "A")
is *itself* a vector, in a M-dimensional vector space
![U](https://latex.codecogs.com/png.latex?U "U"). Let's pretend that U is
2-dimensional, for now.

Remember also that any vector
![\\mathbf{u}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bu%7D "\mathbf{u}")
in ![U](https://latex.codecogs.com/png.latex?U "U") can be represented as
![u\_1 \\mathbf{q}\_1 + u\_2 \\mathbf{q}\_2](https://latex.codecogs.com/png.latex?u_1%20%5Cmathbf%7Bq%7D_1%20%2B%20u_2%20%5Cmathbf%7Bq%7D_2 "u_1 \mathbf{q}_1 + u_2 \mathbf{q}_2"),
where
![\\mathbf{q}\_1](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D_1 "\mathbf{q}_1")
and
![\\mathbf{q}\_2](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bq%7D_2 "\mathbf{q}_2")
is an arbitrary choice of basis vectors.

This means that
![A(\\mathbf{e}\_1)](https://latex.codecogs.com/png.latex?A%28%5Cmathbf%7Be%7D_1%29 "A(\mathbf{e}_1)")
etc. can also be represented in terms of these basis vectors. So, laying it all
out:

![
\\begin{aligned}
A(\\mathbf{e}\_1) & = a\_{1,1} \\mathbf{q}\_1 + a\_{2,1} \\mathbf{q}\_2 \\\\
A(\\mathbf{e}\_2) & = a\_{1,2} \\mathbf{q}\_1 + a\_{2,2} \\mathbf{q}\_2 \\\\
A(\\mathbf{e}\_3) & = a\_{1,3} \\mathbf{q}\_1 + a\_{2,3} \\mathbf{q}\_2
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0AA%28%5Cmathbf%7Be%7D_1%29%20%26%20%3D%20a_%7B1%2C1%7D%20%5Cmathbf%7Bq%7D_1%20%2B%20a_%7B2%2C1%7D%20%5Cmathbf%7Bq%7D_2%20%5C%5C%0AA%28%5Cmathbf%7Be%7D_2%29%20%26%20%3D%20a_%7B1%2C2%7D%20%5Cmathbf%7Bq%7D_1%20%2B%20a_%7B2%2C2%7D%20%5Cmathbf%7Bq%7D_2%20%5C%5C%0AA%28%5Cmathbf%7Be%7D_3%29%20%26%20%3D%20a_%7B1%2C3%7D%20%5Cmathbf%7Bq%7D_1%20%2B%20a_%7B2%2C3%7D%20%5Cmathbf%7Bq%7D_2%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
A(\mathbf{e}_1) & = a_{1,1} \mathbf{q}_1 + a_{2,1} \mathbf{q}_2 \\
A(\mathbf{e}_2) & = a_{1,2} \mathbf{q}_1 + a_{2,2} \mathbf{q}_2 \\
A(\mathbf{e}_3) & = a_{1,3} \mathbf{q}_1 + a_{2,3} \mathbf{q}_2
\end{aligned}
")

[^1]: In short, vector spaces form an Abelian group (which is another way of
    just saying that addition is commutative, associative, has an identity, and
    an inverse), and scalars have to play nice with addition
    (![c(\\mathbf{v} + \\mathbf{u}) = c \\mathbf{v} + c \\mathbf{u}](https://latex.codecogs.com/png.latex?c%28%5Cmathbf%7Bv%7D%20%2B%20%5Cmathbf%7Bu%7D%29%20%3D%20c%20%5Cmathbf%7Bv%7D%20%2B%20c%20%5Cmathbf%7Bu%7D "c(\mathbf{v} + \mathbf{u}) = c \mathbf{v} + c \mathbf{u}"),
    and
    ![(c + d)\\mathbf{v} = c \\mathbf{v} + d \\mathbf{v}](https://latex.codecogs.com/png.latex?%28c%20%2B%20d%29%5Cmathbf%7Bv%7D%20%3D%20c%20%5Cmathbf%7Bv%7D%20%2B%20d%20%5Cmathbf%7Bv%7D "(c + d)\mathbf{v} = c \mathbf{v} + d \mathbf{v}")).
    Also, scalars themselves form a field.
