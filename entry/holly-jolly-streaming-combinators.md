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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L66-L73

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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L75-L82

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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L84-L91

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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L93-L102

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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L104-L109

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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L20-L23

data PipeF i o a =
    YieldF o a
  | AwaitF (Maybe i -> a)
    deriving Functor
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

Note that all of the individual pipes we had planned work as-is!

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L66-L102

sourceHandle :: Handle -> Pipe i String IO ()
sourceHandle handle = do
    res <- lift $ tryJust (guard . isEOFError) (hGetLine handle)
    case res of
      Left  _   -> return ()
      Right out -> do
        yield out
        sourceHandle handle

sinkStdout :: Pipe String o IO ()
sinkStdout = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        lift $ putStrLn x
        sinkStdout

toUpperPipe :: Monad m => Pipe String String m ()
toUpperPipe = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        yield (map toUpper x)
        toUpperPipe

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

That's because using `FreeT`, we imbue the structure required to do monadic
chaining (do notation) and MonadTrans (`lift`) for free!

To "run" our pipes, we can use `FreeT`'s "interpreter" function. This follows
the same pattern as for many free structures: specify how to handle each
individual base functor constructor, and it then gives you a handler to handle
the entire thing.

``` {.haskell}
iterT
    :: (PipeF i o (m a) -> m a)  -- ^ given a way to handle each base functor constructor ...
    -> Pipe i o m a -> m a       -- ^ here's a way to handle the whole thing
```

So let's write our base functor handler. Remember that we established earlier we
can only "run" a `Pipe () Void m a`: that is, pipes where `await` can always be
fed with no information (`()`) and no `yield` is ever called (because you cannot
yield with `Void`, a type with no inhabitants). We can directly translate this
to how we handle each constructor:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L58-L61

handlePipeF :: PipeF () Void (m a) -> m a
handlePipeF = \case
    YieldF o _ -> absurd o
    AwaitF f   -> f (Just ())
```

And so we get our full `runPipe`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L63-L64

runPipe :: Monad m => Pipe () Void m a -> m a
runPipe = iterT handlePipeF
```

I think this exemplifies most of the major beats when working with free
structures:

1.  Define the base functor
2.  Allow the free structure to imbue the proper structure over your base
    functor
3.  Write your interpreter to interpret the constructors of your base functor,
    and the free structure will give you a way to interpret the entire
    structure.

### Chaining

If you look at the list of all the things we wanted, we're still missing one
thing: pipe composition/input-output chaining. That's because it isn't a
primitive operation (like yield or await), and it wasn't given to us for free by
our free structure (`FreeT`, which gave us monadic composition and monad
transformer ability). So with how we have currently written it, there isn't any
way of getting around writing `(.|)` manually. So let's roll up our sleeves and
do the (admittedly minimal amount of) dirty work.

Let's think about the semantics of our pipe chaining. We want to never do more
work than we need to do, so we'll be "pull-based": for `f .| g`, try running `g`
as much as possible until it awaits anything from `f`. Only then do we try doing
`f`.

To implement this, we're going to have to dig in a little bit to the
implementation/structure of `FreeT`:

``` {.haskell}
newtype FreeT f m a = FreeT
    { runFreeT :: m (FreeF f a (FreeT f m a)) }

data FreeF f a b
      Pure a
    | Free (f b)
```

If this looks a little complicated, don't worry: on the face of it, it can be a
bit intimidating. And why is there a second internal data type?

Well, you can think of `FreeF f a b` as being a fancy version of
`Either a (f b)`. And the implementation of `FreeT` is saying that `FreeT f m a`
is *an m-action* that produces `Either a (FreeT f m a)`. So for example,
`FreeT f IO a` is an IO action that produces *either* the `a` (we're done, end
here!) or a `f (FreeT f m a))` (we have to handle an `f` here!)

``` {.haskell}
newtype FreeT f m a = FreeT
    { runFreeT :: m (Either a (f (FreeT f m a))) }
```

At the top level, `FreeT` is an action in the underlying monad (just like
`MaybeT`, `ExceptT`, `StateT`, etc.). Let's take that into account and write our
implementation (with a hefty bit of help from the typechecker and typed holes)!
Remember our plan: for `f .| g`, *start unrolling `g`* until it needs anything,
and then ask `f` when it does.

``` {.haskell}
comp
    :: Monad m
    => Pipe a b m x         -- ^ pipe from a -> b
    -> Pipe b c m y         -- ^ pipe from b -> c
    -> Pipe a c m y         -- ^ pipe from a -> c
comp pf pg = do
    gRes <- lift $ runFreeT pg          -- 1
    case gRes of
      Pure x            -> pure x       -- 2
      Free (YieldF o x) -> do           -- 3
        yield o
        pf `comp` x
      Free (AwaitF g  ) -> do           -- 4
        fRes <- lift $ runFreeT pf
        case fRes of
          Pure _            -> pure () `comp` g Nothing     -- 5
          Free (YieldF o y) -> y       `comp` g (Just o)    -- 6
          Free (AwaitF f  ) -> do                           -- 7
            i <- await
            f i `comp` FreeT (pure gRes)
```

Here are some numbered notes and comments:

1.  Start unrolling the downstream pipe `pg`, in the underlying monad `m`!
2.  If `pg` produced `Pure x`, it means we're done pulling anything. So just
    quit out with `pure x`.
3.  If `pg` produced `Free (YieldF o x)`, it means it's yielding an `o` and
    continuing on with `x`. So let's just yield that `o` and move on to the
    composition of `pf` with the next pipe `x`.
4.  If `pg` produced `Free (AwaitF g)`, now things get interesting. We need to
    unroll `pf` until it yields some `Maybe b`, and feed that to
    `g :: Maybe b -> Pipe b c m y`.
5.  If `pf` produced `Pure y`, that means it was done! So `g` gets a `Nothing`,
    and we move from there. Note we have to compose with a dummy pipe `pure ()`
    to make the types match up properly.
6.  If `pf` produced `YieldF o y`, then we have found our match! So give
    `g (Just o)`, and now we recursively compose the next pipe (`y`) with the
    that `g` gave us.
7.  If `pf` produced `AwaitF f`, then we're in a bind, aren't we? We now have
    two layers waiting for something further upstream. So, we return a new pipe
    that awaits it and feeds it to `f` when it gets it, give it to `f`, and then
    compose `f i :: Pipe a b m x` with `pg`'s result (wrapping up `gRes` back
    into a `FreeT`/`Pipe` so the types match up).

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
