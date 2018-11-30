Tries with Recursion Schemes
============================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/tries-with-recursion-schemes.html)

Not too long ago, I was browsing the [prequel memes
subreddit](https://www.reddit.com/r/PrequelMemes) --- a community built around
creative ways of remixing and re-contextualizing quotes from the cinematic
corpus of the three Star Wars prequel movies --- when I noticed that a fad was
in progress [constructing tries based on quotes as
keys](https://www.reddit.com/r/PrequelMemes/comments/9w59t4/i_expanded_it/)
indexing stills from the movie corresponding to those quotes.

This inspired me to try playing around with some tries myself, and it gave me an
excuse to play around with
*[recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)*
(one of my favorite Haskell libraries). If you haven't heard about it yet,
*recursion-schemes* (and the similar library
*[data-fix](https://hackage.haskell.org/package/data-fix)*) abstracts over
common recursive functions written on recursive data types. It exploits the fact
that a lot of recursive functions for different recursive data types all really
follow the same pattern and gives us powerful tools for writing cleaner and
safer code.

Recursion schemes is a perfect example of those amazing accidents that happen
throughout the Haskell ecosystem: an extremely "theoretically beautiful"
abstraction that also happens to be extremely useful for writing industrially
rigorous code.

Tries are a common intermediate-level recursive data type, and recursion-schemes
is a common intermediate-level library. So, as a fun intermediate-level Haskell
project, let's build a trie data type in Haskell based on recursion-schemes, to
see what it has to offer! The resulting data type will definitely not be a "toy"
--- it'll be something you can actually use to build meme diagrams of your own!

Trie
----

A [trie](https://en.wikipedia.org/wiki/Trie) (prefix tree) is a classic example
of a simple yet powerful data type most people encounter in school (I remember
being introduced to it through a project implementing a boggle solver).

Wikipedia has a nice picture:

![Sample Trie from Wikipedia, indexing lists of Char to
Ints](/img/entries/trie/wiki-trie.png "An example Trie")

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
