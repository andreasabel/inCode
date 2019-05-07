The Applicative Interpreter Combinator Design Pattern
=====================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/interpreter-combinators.html)

Recently I've been having a lot of fun with what I have been calling the
"Applicative Interpreter Combinator" design pattern. It is heavily influenced by
ideas like [Data types a la
Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) and
[unified free monoidal
functors](http://oleg.fi/gists/posts/2018-02-21-single-free.html), but the end
goal is slightly different in spirit.

The goal is to represent Applicative (typically non-monadic) computations
(things like parsers, things to execute, things to consume or produce data) by
assembling "self-evident" basic primitives and subjecting them to many
*different* successive transformations and combiners. The process of doing so:

1.  Forces you to make explicit decisions about the structure of your
    computation type as an ADT.
2.  Allows you to retain isolation of fundamental parts of your domain as
    separate types
3.  Lets you manipulate the structure of your final computation type through
    *normal Haskell techniques* like pattern matching. The structure is
    available throughout the entire process, so you can replace individual
    components and values within your structure.
4.  Allows you to fully *reflect* the structure of your final computation
    through pattern matching and folds, so you can inspect the structure and
    produce useful summaries.

Like "data types a la carte" and free monad/applicative/alternative designs,
these techniques allow you to separate the assembly and inspection of your
programs from the "running" of them. However, the main difference is that here
we focus not just on products and sums, but many different varied and
multi-purpose combinators --- a bona fide "zoo" of combinators. Furthermore, our
goal is not to design a functor that we can throw into `Fix` or `Free` in the
end. We might use a fixed-point or two, not as a "big picture", but rather as an
intermediate step. The *functor itself* is the goal, *not* its fixed point.

This post will be a tour of many different combinators (taken from all over the
Haskell ecosystem --- places like
[kan-extensions](https://hackage.haskell.org/package/kan-extensions),
[transformers](https://hackage.haskell.org/package/transformers),
[free](https://hackage.haskell.org/package/free), and even
[base](https://hackage.haskell.org/package/base)) and try to compile and compare
them in a systematic way. We'll be looking at how they act on a couple of base
primitives, and seeing the effect that each one has on our primitives.

Setting the Playing Field
-------------------------

First, let's set up our base primitive functors that we will be playing around
with and seeing how all of these primitives are affected by our combinators.

In the end, we're going to be building a *command line options schema*, which we
can run as a parser or summarize.

A command line options schema has two basic parts:

-   *Positional arguments* (think `mv <src> <dest>`)
-   *Options* (think `ls --all`)

These two will be the building blocks of our parser!

First, a Functor to represent a schema for a positional argument:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L26-L31

data Arg a = Arg
    { argName  :: String
    , argHelp  :: String
    , argRead  :: ReadM a
    }
  deriving Functor
```

A schema describing an argument that parses a value of type `a` contains a name,
a help message, and a `ReadM a`, which is *optparse-applicative*'s string parser
data type (it contains information on how to parse a `String` into an `a`). For
the most part, we only need to care about two `ReadM a`s,
`auto :: Read a => ReadM a`, a `ReadM` that works for all `Read` instances, and
`str :: IsString s => ReadM a`, a `ReadM` that works for all string-like types
(like `String` and `Text`).

Let's define a simple interpreter for this primitive, creating an
*optparse-applicative* parser:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L47-L50

argParser :: Arg a -> Parser a
argParser Arg{..} = argument argRead $
        help    argHelp
     <> metavar argName
```

Here we using the `-XRecordWildcards` extension to bind all of the fields in
`Arg` for us to use, for convenience. This is just *optparse-applicative*'s
method of describing parser with a single argument, and when we "run" it, it
will parse it as a single positional argument using the `ReadM`.

For example, we'll make a test `Arg` that parses a name:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L71-L76

nameArg :: Arg String
nameArg = Arg
    { argName = "<name>"
    , argHelp = "A person's name"
    , argRead = str
    }
```

And we'll create handy tester:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L78-L81

testParser :: Parser a -> String -> IO a
testParser p = handleParseResult
             . execParserPure defaultPrefs (info (p <**> helper) mempty)
             . words
```

We can now test it out:

``` {.haskell}
ghci> testParser (argParser nameArg) "--help"
-- Usage: <interactive> <name>
--
-- Available options:
--   <name>                   A person's name
--   -h,--help                Show this help text
ghci> testParser (argParser nameArg) "alice"
-- "alice"
ghci> testParser (argParser nameArg) "bob"
-- "bob"
```

So if we enter a single positional argument, it gets parsed as itself.

Note that `Arg` is a `Functor`, so we can fmap a transformation on the result:

``` {.haskell}
ghci> testParser (argParser (map toUpper <$> nameArg)) "carol"
"CAROL"
```

Sums
----

Products
--------

Convolutions
------------

Compositions
------------

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
