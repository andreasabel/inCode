Day by Day: Applicative Interpreters a la Carte
===============================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/applicative-interpreters-a-la-carte.html)

I had the pleasure of working with both the
*[servant](https://hackage.haskell.org/package/servant)* and
*[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)*
libraries recently, which culminated in the
*[servant-cli](https://hackage.haskell.org/package/servant-cli)* library. At
first, I did everything by directly manipulating a `Parser` type, which is the
type of command line argument parsers from *optparse-applicative*. However, I
ran into an issue very quickly: I needed to be able to manipulate the
*structure* of a command line parser directly --- deleting and modifying
commands, the desire to be able to pattern match, reflect on the arguments
needed, move arguments around, etc. By working directly with `Parser`, I threw
everything into a black box that I could not easily inspect. Have you ever felt
like this with IO, or any other Applicative (parsers or otherwise) you've had to
use?

I realized that I needed to make an algebraic data type that could represent the
*structure* of a command line parser. I needed an ADT that I could:

-   Pattern match on to modify, shift, and re-arrange components of the parser
-   Reflect to inspect what sort of arguments the parser will ask for, what
    command line options are being asked for, etc.
-   Build from on very simple components that I could combine together in a
    logical way with semantic combinators.
-   Finally, "run" as an *optparse-applicative* combinator, an interactive
    wizard, or generate rich documentation, etc.

During this journey, I ended up falling in love with a pattern I am calling
"Applicative Interpreters a la Carte", that I believe is unique to Functor and
Applicative (that is, explicitly non-monadic) interpreters.

In this post we'll be creating such a structure from simple components using
Applicative Interpreter Combinators such as `Day`, `:*:`, `:+:`, `:*:`, `Ap`
(the free Applicative), `Alt` (the free Alternative), `Coyoneda` (the free
Functor), `Lift` (the free Pointed), and more! We'll also explore fundamental
principles of each combinator as they relate to a bigger picture.

The Goal
--------

Let's define the overall structure of our command line menu: We will have a
hierarchy of nested sub-command menus that we can dig down. We can go down a
menu by specifying a "command" (with a catch-all wildcard command), and once we
get down to the action we want, we can finish up by specifying `--option`s.

For example, we can make a command line tool like "git"

-   

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
