Advent of Haskell: Roll your own Holly Jolly streaming combinator library with
Free
===================================================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/holly-jolly-streaming-combinators.html)

Hi! Welcome, if you're joining us from the great [Advent of Haskell
2020](https://adventofhaskell.com/) event! Feel free to grab a hot chocolate and
sit back by the fireplace. I'm honored to be able to be a part of the event this
year; it's a great initiative and harkens back to the age-old Haskell tradition
of bite-sized Functional Programming "advent calendars". I remember when I was
first learning Haskell, [24 Days of
Hackage](https://ocharles.org.uk/pages/2012-12-01-24-days-of-hackage.html) was
one of my favorite series that helped me really get into the exciting world of
Haskell and the all the doors that functional programming can open.

For my entry, in the spirit of the holidays, I decided to treat it as a chance
to go reminisce about Haskell times of olde. Right now in Haskell we seem to be
in an era where the great debate in the future of the language is in [the best
way to handle composable
effects](https://www.stephendiehl.com/posts/decade.html#algebraic-effect-systems)
in Haskell, but if we roll back the calendar to the beginning of the decade,
this big existential war was in the best way to handle constant-space effectful
streaming.

The dust on that debate has more or less settled (the apparent answer: it
doesn't *really* matter, just use whatever is more integrated with what you are
already using and has a more robust relevant ecosystem built around it), but
recently I had an occasion to explore the space myself. I needed a specific sort
of streaming behavior that I couldn't express cleanly exactly in any of the
major options and I thought to look into what it would take to roll your own
using the "modern" tools of 2020. In the end it was a fun journey, because it
helped me understand things a lot better in a way I probably could not have
reached on my own when I was just starting out in Haskell when this topic was
"hot".

So in this post we'll be attempting to roll our own monadic streaming effects
combinators in a way that is made surprisingly (at least, to me) clean if we
approach it from a composable effects sort of style --- hopefully demonstrating
how natural a lot of previously complex topics become if we just imagine them in
the context of composable effects!

An effectful picture
--------------------

The "goal" is to make a system of composable types that is "pull-based", so we
can process data as it is read in from IO only as we need it, and never do more
work than we need to do up-front or leak memory when we stop using it.

So, the way I usually approach things like these is: "dress for the interface
you want, not the one you have." It involves:

1.  Thinking of the `m a` you want and how you would want to combine it/use it.
2.  Express the primitive actions of that thing
3.  Use some sort of free structure or effects system to enhance that primitive
    with the interface you are looking for.

For step 3, I've explored this concept in the past as [functor combinator
style](https://blog.jle.im/entry/functor-combinatorpedia.html), but there are
multiple ways to do it!

So, let's make our type!

``` {.haskell}
type Pipe i o m a = ...
```

where a `Pipe i o m a` represents a pipe component where:

-   `i`: the input of the pipe that it expects upstream
-   `o`: the output of the pipe that it will yield downstream
-   `m`: the monad that the underlying actions live in
-   `a`: the overall result of the pipe once it has terminated.

One nice thing about this setup is that by picking different values for the type
parameters, we can already get a nice classification for interesting subtypes:

1.  If `i` is `()` (or universally quantified) --- a `Pipe () o m a` --- it
    means that the pipe doesn't ever expect any sort of information upstream,
    and so can be considered a "source" that keeps on churning out values.

2.  If `o` is `Void` (or universally quantified) --- a `Pipe i Void m a` --- it
    means that the pipe will never yield anything downstream, because `Void` has
    no inhabitants that could possibly be yielded.

    ``` {.haskell}
    data Void
    ```

    This means that it acts like a "sink" that will keep on eating `i` values
    without ever outputting anything downstream.

3.  If `i` is `()` and `o` is `Void` (or they are both universally quantified),
    then the pipe doesn't expect any sort of information upstream, and also
    won't ever yield anything downstream... a `Pipe () Void m a` is just an
    `m a`!

4.  If `a` is `Void` (or universally quantified) --- a `Pipe i o m Void` --- it
    means that the pipe will never terminate, since `Void` has no inhabitants
    that could it could possibly produce upon termination.

To me, I think it embodies a lot of the nice principles about the "algebra" of
types that can be used to reason with inputs and outputs. Plus, it allows us to
unify sources, sinks, and non-terminating pipes all in one type!

Now let's think of the interface we want. We want to be able to:

``` {.haskell}
-- | Yield a value `o` downstream
yield :: o -> Pipe i o m ()

-- | Await a value `i` upstream
await :: Pipe i o m (Maybe i)

-- | Terminate with a result value
return :: a -> Pipe i o m a

-- | Sequence pipes one-after-another:
-- "do this until it terminates, then that one next"
(>>) :: Pipe i o m a -> Pipe i o m b -> Pipe i o m b

-- | In fact let's just make it a full fledged monad, why not?  We're designing
our dream interface here.
(>>=) :: Pipe i o m a -> (a -> Pipe i o m b) -> Pipe i o m b

-- | A pipe that simply does action in the underlying monad and terminates with
-- the result
lift :: m a -> Pipe i o m a

-- | Compose pipes, linking the output of one to the input of the other
(.|) :: Pipe i j m a -> Pipe j o m b -> Pipe i o m b

-- | Finally: run it all on a pipe expecting no input and never yielding:
runPipe :: Pipe () Void m a -> m a
```

So, these are going to be implementing "conduit-style" streaming combinators,
where streaming actions are monadic, and monadic sequencing represents "do this
after this one is done." Because of this property, they seem to also be
*pull-based* pipes, yields will block until a corresponding await can accept
what is yielded.

### Dress for the interface you want

"Dress for the interface you want", they all told me. So let's pretend we
already implemented this interface...what could we do with it?

Well, can write simple sources like "yield the contents from a file
line-by-line":

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L60-L67

sourceHandle :: Handle -> Pipe i String IO ()
sourceHandle handle = do
    res <- lift $ tryJust (guard . isEOFError) (hGetLine handle)
    case res of
      Left  _   -> return ()
      Right out -> do
        yield out 
        sourceHandle handle
```

Note that because the `i` is universally quantified, it means that we know that
`sourceFile` never ever awaits or touches any input: it's purely a source.

We can even write a simple sink, like "await and print the results to stdout as
they come":

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L69-L76

sinkStdout :: Pipe String o IO ()
sinkStdout = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        lift $ putStrLn x
        sinkStdout
```

And maybe we can write a pipe that takes input strings and converts them to all
capital letters and re-yields them:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L78-L85

toUpperPipe :: Monad m => Pipe String String m ()
toUpperPipe = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        yield (map toUpper x)
        toUpperPipe
```

And we can maybe write a pipe that stops as soon as it reads the line `STOP`.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L87-L96

untilSTOP :: Monad m => Pipe String String m ()
untilSTOP = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x
        | x == "STOP" -> pure ()
        | otherwise   -> do
            yield x
            untilSTOP
```

`untilSTOP` is really sort of the crux of what makes these streaming systems
useful: we only pull items from the file as we need it, and `untilSTOP` will
stop pulling anything as soon as we hit `STOP`, so no IO will happen anymore if
the upstream sink does IO.

### Our Ideal Program

Now ideally, we'd want to write a program that lets us compose the above pipes
to read from a file and output its contents to stdout, until it sees a STOP
line:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L98-L103

sampleProgram :: Handle -> Pipe i o IO ()
sampleProgram handle =
       sourceHandle handle
    .| untilSTOP
    .| toUpperPipe
    .| sinkStdout
```

Implementing the interface you want
-----------------------------------

Step 2 of our plan was to identify the primitive actions we want. Looking at our
interface, it seems like we can narrow things down to two:

``` {.haskell}
yield  :: o -> Pipe i o m ()
await  :: Pipe i o m (Maybe i)
lift   :: m a -> Pipe i o m a
return :: a   -> Pipe i o m a
```

However, we can note that `lift` and `return` will come from the `Monad` and
`MonadTrans` instances that we wish we had:

``` {.haskell}
class Monad m where
    return :: a -> m a

class MonadTrans p where
    lift :: m a -> p m a
```

The effects system/functor combinator plan is to identify your true primitives,
and let free structures give you the instances you need for them.

So this means we only need two primitives: `yield` and `await`. Then we just
throw them into some machinery that gives us a free `Monad` and `MonadTrans`
structure, and we're golden :)

In the style of the *[free](https://hackage.haskell.org/package/free)* library,
we'd write base functions to get an ADT that describes the primitive actions:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L20-L25

data PipeF i o a =
    YieldF o a
  | AwaitF (Maybe i -> a)
    deriving Functor
               
type Pipe i o = FreeT (PipeF i o)
```

The general structure of the base functor style is to represent each primitive
as a constructor: include any inputs, and then a continuation on what to do if
you had the result.

For example:

1.  For `YieldF`, you need an `o` to be able to yield. The second field should
    really be the continuation `() -> a`, since the result is `()`, but that's
    equivalent to `a` in Haskell.
2.  For `AwaitF`, you don't need any parameters to await, but the continuation
    is `Maybe i -> a` since you need to specify how to handle the `Maybe i`
    result.

This is specifically the structure that
*[free](https://hackage.haskell.org/package/free)* expects, but this principle
can be ported to any algebraic effects system.

And now...we're done! We can use the `FreeT` type from
*[Control.Monad.Trans.Free](https://hackage.haskell.org/package/free/docs/Control-Monad-Trans-Free.html)*,
and now we have our pipe interface, with a `Monad` and `MonadTrans` instance!

``` {.haskell}
type Pipe i o = FreeT (PipeF i o)
```

This takes our base functor and imbues it with a full `Monad` and `MonadTrans`
instance:

``` {.haskell}
lift :: m a -> FreeT (PipeF i o) m a
lift :: m a -> Pipe i o m a

return :: a -> FreeT (PipeF i o) m a
return :: a -> Pipe i o m a

(>>)  :: Pipe i o m a -> Pipe i o m b -> Pipe i o m b
(>>=) :: Pipe i o m a -> (a -> Pipe i o m b) -> Pipe i o m b
```

That's the essence of the free structure: it *adds* to our base functor
(`PipeF`) exactly the structure it needs to be able to implement the instances
it is free on. And it's all free as in beer! :D

Now we just need our functions to lift our primitives to `Pipe`, using
`liftF :: f a -> FreeT f m a`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L27-L31

yield :: Monad m => o -> Pipe i o m ()
yield x = liftF $ YieldF x ()

await :: Monad m => Pipe i o m (Maybe i)
await = liftF $ AwaitF id
```

(these things you can usually just fill in using type tetris, filling in values
with typed holes into they typecheck).

And now, we

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
