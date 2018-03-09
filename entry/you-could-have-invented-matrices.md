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
such that
![\\mathbf{v} = v\_x \\hat{\\mathbf{i}} + v\_y \\hat{\\mathbf{j}} + v\_x \\hat{\\mathbf{k}}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D%20%3D%20v_x%20%5Chat%7B%5Cmathbf%7Bi%7D%7D%20%2B%20v_y%20%5Chat%7B%5Cmathbf%7Bj%7D%7D%20%2B%20v_x%20%5Chat%7B%5Cmathbf%7Bk%7D%7D "\mathbf{v} = v_x \hat{\mathbf{i}} + v_y \hat{\mathbf{j}} + v_x \hat{\mathbf{k}}").

Note that
![\\langle v\_x, v\_y, v\_z \\rangle](https://latex.codecogs.com/png.latex?%5Clangle%20v_x%2C%20v_y%2C%20v_z%20%5Crangle "\langle v_x, v_y, v_z \rangle")
is **not** the same thing, conceptually, as the **vector**
![\\mathbf{v}](https://latex.codecogs.com/png.latex?%5Cmathbf%7Bv%7D "\mathbf{v}").
It is *an encoding* of that vector, that only makes sense once we choose to
*agree* on a specific set of basis.

For an N-dimensional vector space, it means that, with N items, we can represent
any vector in that space. And, if we agree on those N items, we can devise an
encoding, such that:

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

[^1]: In short, vector spaces form an Abelian group (which is another way of
    just saying that addition is commutative, associative, has an identity, and
    an inverse), and scalars have to play nice with addition
    (![c(\\mathbf{v} + \\mathbf{u}) = c \\mathbf{v} + c \\mathbf{u}](https://latex.codecogs.com/png.latex?c%28%5Cmathbf%7Bv%7D%20%2B%20%5Cmathbf%7Bu%7D%29%20%3D%20c%20%5Cmathbf%7Bv%7D%20%2B%20c%20%5Cmathbf%7Bu%7D "c(\mathbf{v} + \mathbf{u}) = c \mathbf{v} + c \mathbf{u}"),
    and
    ![(c + d)\\mathbf{v} = c \\mathbf{v} + d \\mathbf{v}](https://latex.codecogs.com/png.latex?%28c%20%2B%20d%29%5Cmathbf%7Bv%7D%20%3D%20c%20%5Cmathbf%7Bv%7D%20%2B%20d%20%5Cmathbf%7Bv%7D "(c + d)\mathbf{v} = c \mathbf{v} + d \mathbf{v}")).
    Also, scalars themselves form a field.
