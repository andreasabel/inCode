Practical Dependent Types: A Deeper Look at Proofs
==================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/practical-dependent-types-in-haskell-3.html)

Hi! This will be the final post of the [dependently typed neural network
series](https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html).
This post has been long put-off because there really wasn't *too* much more we
can learn about dependent types and programming with dependent types from this
example. Still, this example still has a bit more to offer us, so let's take one
last look :) Think of this post as an epilogue.

In [Part
1](https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html), we
tried solving our problem in an "untyped" way, recognized all of those Haskell
red flags, and then added types to make things safer and reap benefits in API
clarity, compiler support, and a smoother code writing experience. In [Part
2](https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html), we
looked at how integrating our typed programs with the real world, where types
might vary at runtime, by levering existential types in two different forms.

The Secret Life of Proofs
-------------------------

So far in this journey, we've hand-waved the idea of "proofs", and their role in
this dependently typed programming. In fact, I've sort of intentionally left the
idea vague, because I feel like a lot of other dependent type
introductions/tutorials over-emphasize proofs, to the point there is a public
impression that [dependently typed programming is all about
proofs](https://www.reddit.com/r/haskell/comments/62uv6g/verify_your_typeclass_instances_in_haskell_today/dfpt2g7/):

> I think dependent types are pretty cool, but honestly the sheer difficulty
> jump from quickcheck tests to actual proofs to me seems a bit too massive to
> justify having to deal with them much.
>
> I think if we just had testable functions (basically just predicates that
> should always return true) alongside classes that could be quickchecked
> against. And actually took it very seriously when an instance isn't lawful
> (basically just disallow it almost always), then I think we would be in a
> pretty darn good spot.

There's a popular (mis?)conception that dependent types are basically a
replacement for QuickCheck, or refinement types (like Liquid Haskell). But
really, as we have seen, they are much more --- they let you leverage the
compiler to help you write your code, help you enforce properties at the
structural level for your types, create more expressive API's, and much more.
They aren't "just" to prove properties about your program.

In reality, the idea of dependent types and the idea of proofs sort of are
separate but complementing ideas. Dependent types enable this "proofy" style
discussed in the comment, but, also, the ability to dynamically construct and
manipulate proofs opens up many doors for the capabilities of dependently typed
programs.

### What are they?

In the context of dependent types (in Haskell), a "proof" is general
(non-rigorous, informal) term for a *runtime value* that GHC/Haskell can use for
*type-level* shenanigans. In other words, it's a *term-level* witness for a
*type-level* truth.

It's the critical *run-time* values that we use to power our *type-level* fun,
and are also commonly known as "witnesses".

I don't believe there is any rigorous definition, but the term comes up whenever
we talk about generating or using run-time values that give us type-level power.

In this article, the simplest example of something that can be called a proof or
a witness is the singleton for `Nat`s: A value of type `Sing n` (for `Nat n`)
"witnesses" a `KnownNat n` constraint.

We saw it earlier when we wanted to pass along evidence that our `n` has an
instance of `KnownNat`:

`haskell nIsKnownNatWePromise :: forall n. Sing n -> Integer nIsKnonwNatWePromise = \case     SNat -> natVal (Proxy :: Proxy n)`

In the case statement above, in the `SNat ->` branch, GHC knows that `n` has a
`KnownNat` instance. This runtime value, `SNat`, carries the fact that we have
an instance of `KnownNat n`. Remember that
`natVal :: KnownNat n => p n -> Integer` -- it only works of `n` is an instance
of `KnownNat`. `SNat :: Sing n` is a runtime value that "gives us" that
instance. It's a "proof" that `n` is an instance of `KnownNat`.

### First-Class Values

The *key* to their usefulness is that, as runtime values, they are *first-class
values* in Haskell. We can manipulate them, pass them, construct them, etc.,
just as if they were normal values. You can construct complex proofs from simple
ones, compose them, etc.

For example, check out the function `%:+` from the *singletons* package:

`haskell (%:+) :: forall (n :: Nat) (m :: Nat). Sing n -> Sing m -> Sing (n + m)`

Given, at runtime, a witness for `KnownNat n` (our `Sing n`) and a witness for
`KnownNat m`, we can construct, at runtime a witness for `KnownNat (n + m)`:

`haskell add :: Integer -> Integer -> Integer add x y = withSomeSing x $ \(sx :: Sing x) ->           withSomeSing y $ \(sy :: Sing y) ->             case sx %:+ sy of               -- z is x + y               (SNat :: Sing z) -> natVal (Proxy :: Proxy z)`

`haskell ghci> add 5 7 12`

&lt;!-- Uniting Existential Contexts --&gt; &lt;!-- ----------------------------
--&gt;

&lt;!-- In the last exercise, we introduced `SomeNet`: --&gt;

&lt;!-- ~~~haskell --&gt; &lt;!-- !!!dependent-haskell/NetworkTyped2.hs "data
SomeNet" --&gt; &lt;!-- ~~~ --&gt;

&lt;!-- `SomeNet` is actually a big step above `OpaqueNet` because now its
external API --&gt; &lt;!-- (the size of vectors that it takes/outputs) is now
existentially quantified, so --&gt; &lt;!-- this presents some unique
challenges. --&gt;

&lt;!-- Recall that we was able to write `runOpaqueNet` without much problems,
because --&gt; &lt;!-- the types guaranteed that everything made sense: --&gt;

&lt;!-- ~~~haskell --&gt; &lt;!-- !!!dependent-haskell/NetworkTyped2.hs
"runOpaqueNet ::" --&gt; &lt;!-- ~~~ --&gt;

&lt;!-- In fact, GHC actually enforces that everything works out --- it knows
that you --&gt; &lt;!-- run a `n'` with an `R i`, and sees that `x` is an `R i`,
and also knows that --&gt; &lt;!-- whatever the internal structure is, an `R o`
is always what pops out regardless --&gt; &lt;!-- if `hs` is `'[]`, `'[5,3]` or
`'[100,200,4]`. --&gt;

&lt;!-- But can we write a sensible `runSomeNet`? What would the type even be?
Let's --&gt; &lt;!-- try an initial attempt: --&gt;

&lt;!-- ~~~haskell --&gt; &lt;!-- runSomeNet :: (KnownNat i, KnownNat o) --&gt;
&lt;!-- =&gt; SomeNet --&gt; &lt;!-- -&gt; R i --&gt; &lt;!-- -&gt; R o --&gt;
&lt;!-- runSomeNet n x = case n of --&gt; &lt;!-- SNet n' -&gt; runNet n' x
--&gt; &lt;!-- ~~~ --&gt;

&lt;!-- Hm. This clearly won't work, because the network inside `SomeNet` might
not --&gt; &lt;!-- even take the `R i` that we give it. What if it takes a
`R 5`, but we pass in --&gt; &lt;!-- an `R 10`? Remember, because of universal
quantification, `runSomeNet` has to --&gt; &lt;!-- work with *any* `i`, be it 5,
10, or 100. But the internal network might not --&gt; &lt;!-- be so
accommodating. If we try to write it, GHC will complain immediately. In --&gt;
&lt;!-- short, `runSomeNet` should be *partial*, and return a `Maybe`. --&gt;

&lt;!-- ~~~haskell --&gt; &lt;!-- runSomeNet :: (KnownNat i, KnownNat o) --&gt;
&lt;!-- =&gt; SomeNet --&gt; &lt;!-- -&gt; R i --&gt; &lt;!-- -&gt; Maybe (R o)
--&gt; &lt;!-- ~~~ --&gt;

&lt;!-- We can see another problem here --- We can't have it return `R o`, of
course, --&gt; &lt;!-- because `o` is universally quantified here, so the user
can decide `o`. But --&gt; &lt;!-- `o` isn't free for the user to pick...it's
determined by the network inside --&gt; &lt;!-- `SNet`. So, the `o` has to be
existentially quantified. We'll return a --&gt; &lt;!-- continuation-style
existentially quantified `o` here, because *hmatrix* doesn't --&gt; &lt;!-- come
with a built-in constructor-style quantifier: --&gt;

&lt;!-- ~~~haskell --&gt; &lt;!-- runSomeNet :: KnownNat i --&gt; &lt;!-- =&gt;
SomeNet --&gt; &lt;!-- -&gt; R i --&gt; &lt;!-- -&gt; (forall o. KnownNat o
=&gt; R o -&gt; r) --&gt; &lt;!-- -&gt; Maybe r --&gt; &lt;!-- ~~~ --&gt;

&lt;!-- And finally, we have a type signature that makes sense: give a `SomeNet`
and an --&gt; &lt;!-- `R i`, and possibly get in return an existentially
quantified `R o`. If the `R --> <!-- i` doesn't fit into the `SomeNet`, the
result will be `Nothing`. --&gt;

&lt;!-- Now that we have a type, let's try implementing it: --&gt;

&lt;!-- ~~~haskell --&gt; &lt;!-- runSomeNet :: KnownNat i --&gt; &lt;!-- =&gt;
SomeNet --&gt; &lt;!-- -&gt; R i --&gt; &lt;!-- -&gt; (forall o. KnownNat o
=&gt; R o -&gt; r) --&gt; &lt;!-- -&gt; Maybe r --&gt; &lt;!-- runSomeNet n x f
= case n of --&gt; &lt;!-- SNet (n' :: Network i' hs o) -&gt; --&gt; &lt;!-- if
natVal (Proxy @i') == natVal (Proxy @i) --&gt; &lt;!-- then Just (f (runNet n'
x)) --&gt; &lt;!-- else Nothing --&gt; &lt;!-- ~~~ --&gt;

&lt;!-- First, we open it and check if the `i'` inside the `SNet` is the same as
the --&gt; &lt;!-- `i` we get as input. If it is, we return `Just`, and if not,
`Nothing`. --&gt;

&lt;!-- Unfortunately, this doesn't really work. That's because our silly little
`==` --&gt; &lt;!-- doesn't actually prove to GHC that the two lengths are
equal. GHC will still --&gt; &lt;!-- believe that `i` and `i'` are different, in
general. --&gt;

&lt;!-- And why should it believe that `i ~ i'`, just because of `==`? Remember
that --&gt; &lt;!-- `==` is a user-defined function, and can return anything.
Why should the type --&gt; &lt;!-- checker be ~~fooled~~ convinced by a silly
user-defined function? --&gt;

&lt;!-- The problem is that the `Bool` returned doesn't really tell the compiler
--&gt; &lt;!-- anything. It's just a bit of information, and doesn't really come
with any --&gt; &lt;!-- proof that the two types are actually equal. What we
need is a way to *prove* --&gt; &lt;!-- to the compiler (and the typechecker)
that the two are equal. --&gt;

&lt;!-- \#\#\# Proofs --&gt;

&lt;!-- We got far without talking about proofs, but really, you can only expect
to --&gt; &lt;!-- get so far when talking about dependently typed programming
without talking --&gt; &lt;!-- about proofs! Proofs are, in a way, essential to
the very essence of --&gt; &lt;!-- dependently typed programming.\[^proofs\]
--&gt;

&lt;!-- \[^proofs\]: One thing I've noticed, however, is that a lot of
dependently typed --&gt; &lt;!-- programming introductions *begin* with proofs,
and go to applications later. --&gt; &lt;!-- Here, I hope I can change that
trend by starting with the applications, and --&gt; &lt;!-- bringing proofs
later after being able to see their motivation! --&gt;
