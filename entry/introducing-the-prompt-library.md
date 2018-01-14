Introducing the "Prompt" library
================================

> Originally posted by [Justin Le](https://blog.jle.im/) on June 30, 2015.
> [Read online!](https://blog.jle.im/entry/introducing-the-prompt-library.html)

**Prompt**: [README](https://github.com/mstksg/prompt/blob/master/README.md) /
[hackage](http://hackage.haskell.org/package/prompt) /
[github](https://github.com/mstksg/prompt)

Have you ever wanted to specify a computation involving some limited form of IO
--- like querying a database, or asking stdio --- but didn't want a computation
in the `IO` monad, opening the entire can of worms that is arbitrary `IO`? Have
you ever looked at complicated `IO a` you wrote last week at 4am and prayed that
it didn't launch missiles if you decided to execute it? Do you want to be able
to run an effectful computation and explicitly *say* what IO it can or cannot
do?

Introducing the *[prompt](http://hackage.haskell.org/package/prompt)* library!
It's a small little lightweight library that allows you to specify and describe
computations involving forms of effects where you "ask" with a value and receive
a value in return (such as a database query, etc.), but not ever care about how
the effects are fulfilled --- freeing you from working directly with IO.

~~~haskell data Foo = Foo { fooBar :: String , fooBaz :: Int } deriving Show

-- ask with a String, receive a String as an answer promptFoo :: Prompt String
String Foo promptFoo = Foo &lt;$&gt; prompt "bar" &lt;\*&gt; fmap length (prompt
"baz") ~~~

Running
-------

You can now "run it" in IO, by talking to stdio ---

~~~haskell ghci&gt; runPromptM promptFoo $ \\str -&gt; putStrLn str &gt;&gt;
getLine bar -- stdout prompt

> hello! -- stdin response typed in baz -- stdout prompt i am baz -- stdin
> response typed in Foo "hello!" 8 -- result ~~~

(this is also just `interactP promptFoo`)

Or you can maybe request it from the environment variables:

~~~haskell ghci&gt; import System.Environment ghci&gt; setEnv "bar" "hello!"
ghci&gt; setEnv "baz" "i am baz" ghci&gt; runPromptM promptFoo getEnv Foo
"hello!" 8 ~~~

Or maybe you want to fulfill the prompts purely:

~~~haskell ghci&gt; import qualified Data.Map as M ghci&gt; let testMap =
M.fromList \[("bar", "hello!"), ("baz", "i am baz")\] ghci&gt; runPrompt
promptFoo (testMap M.!) Foo "hello!" 8 ~~~

With `Prompt`, specify the computation and your logic *without involving any
IO*, so you can write safe code without arbitrary side effects. If you ever
receive a `Prompt`, you know it can't wipe out your hard drive or do any IO
other than exactly what you allow it to do! I'd feel more safe running a
`Prompt a b r` than an `IO r`.

You can also do some cute tricks; `Prompt a () r` with a "prompt response
function" like `putStrLn` lets you do streaming logging, and defer *how* the
logging is done --- to IO, to a list?

~~~haskell ghci&gt; let logHelloWord = mapM\_ prompt \["hello", "world"\]
ghci&gt; runPromptM logHelloWorld putStrLn hello world ghci&gt; execWriter $
runPromptM logHelloWorld tell "helloworld" ~~~

`Prompt () b r` is like a fancy `ReaderT b m r`, where you "defer" the choice of
the Monad.

Combining with other effects
----------------------------

`Prompt` can be used as an underlying "effects" source for libraries like
*pipes*, *conduit*, and *auto*. If your effects are only ever asking and
prompting and receiving, there's really no need to put the entire power of `IO`
underneath your DSL as an effects source. That's just crazy!

`Prompt` can be used with monad transformers to give you safe underlying effect
sources, like `StateT s (Prompt a b) r`, which is a stateful computation which
can sometimes sequence "prompty" effects. `Prompt` is also itself a "Traversable
transformer", with `PrompT a b t r`. It can perform computations in the context
of a Traversable `t`, to be able to incorporate built-in short-circuiting and
logging, etc.

This is all abstracted over with `MonadPrompt`, `MonadError`, `MonadPlus`, etc.,
typeclasses ---

~~~haskell promptFoo2 :: (MonadPlus m, MonadPrompt String String m) =&gt; m Foo
promptFoo2 = do bar &lt;- prompt "bar" str &lt;- prompt "baz" case readMaybe str
of Just baz -&gt; return $ Foo bar baz Nothing -&gt; mzero

-- more polymorphic promptFoo :: MonadPrompt String String m =&gt; m Foo
promptFoo = Foo &lt;$&gt; prompt "bar" &lt;\*&gt; fmap length (prompt "baz") ~~~

You can run `promptFoo` as a `MaybeT (Prompt String String) Foo`, and manually
unwrap:

~~~haskell ghci&gt; interactP . runMaybeT $ promptFoo2 bar

> hello! baz i am baz Nothing ghci&gt; interactP . runMaybeT $ promptFoo2 bar
> hello! baz 19 Just (Foo "hello!" 19) ~~~

Or you can run it as a `PromptT String String MaybeT Foo`, to have `PromptT`
handle the wrapping/unwrapping itself:

~~~haskell ghci&gt; interactPT promptFoo2 bar

> hello! baz i am baz Nothing ghci&gt; interactPT $ promptFoo2 &lt;|&gt;
> promptFoo bar hello! baz i am baz bar -- failed to parse --- retrying with
> promptFoo! hello! baz i am baz Just (Foo "hello" 8) ~~~

The previous example of `logHelloWorld`?

~~~haskell ghci&gt; runPromptT (logHelloWorld :: PromptT String () (Writer
String) ()) tell "helloworld" ~~~

Runners
-------

The "runners" are:

~~~haskell interactP :: Prompt String String r -&gt; IO r interactPT ::
Applicative t =&gt; PromptT String String t r -&gt; IO (t r)

runPrompt :: Prompt a b r -&gt; (a -&gt; b) -&gt; r runPromptM :: Monad m =&gt;
Prompt a b r -&gt; (a -&gt; m b) -&gt; m r

runPromptT :: PromptT a b t r -&gt; (a -&gt; t b) -&gt; t r runPromptTM :: Monad
m =&gt; PromptT a b t r -&gt; (a -&gt; m (t b)) -&gt; m (t r) ~~~

Note that `runPromptM` and `runPromptTM` can run in monads (like `IO`) that are
*completely unrelated* to the `Prompt` type itself. It sequences them all "after
the fact". It's also interesting to note that `runPrompt` is just a glorified
`Reader (a -> b) r`.

With `runPromptTM`, you can incorporate `t` in your "prompt response" function,
too. Which brings us to our grand finale -- environment variable parsing!

~~~haskell import Control.Monad.Error.Class import Control.Monad.Prompt import
Text.Read import qualified Data.Map as M

type Key = String type Val = String

data MyError = MENoParse Key Val | MENotFound Key deriving Show

promptRead :: (MonadError MyError m, MonadPrompt Key Val m, Read b) =&gt; Key
-&gt; m b -- promptRead :: Read b =&gt; Key -&gt; PromptT Key Val (Either
MyError) b promptRead k = do resp &lt;- prompt k case readMaybe resp of Nothing
-&gt; throwError $ MEParse k resp Just v -&gt; return v

promptFoo3 :: MonadPrompt Key Val m =&gt; m Foo -- promptFoo3 :: Applicative t
=&gt; PromptT Key Val t Foo promptFoo3 = Foo &lt;$&gt; prompt "bar" &lt;\*&gt;
promptRead "baz"

-- -- running!

-- Lookup environment variables, and "throw" an error if not found throughEnv ::
IO (Either MyError Foo) throughEnv = runPromptTM parseFoo3 $ \\k -&gt; do env
&lt;- lookupEnv k return $ case env of Nothing -&gt; Left (MENotFound k) Just v
-&gt; Right v

-- Fulfill the prompt through user input throughStdIO :: IO (Either MyError Foo)
throughStdIO = interactPT parseFoo3

-- Fulfill the prompt through user input; count blank responses as "not found"
throughStdIOBlankIsError :: IO (Either MyError Foo) throughStdIOBlankIsError =
runPromptTM parseFoo3 $ \\k -&gt; do putStrLn k resp &lt;- getLine return $ if
null resp then Left (MENotFound k) else Right resp

-- Fulfill the prompt purely through a Map lookup throughMap :: M.Map Key Val
-&gt; Either MyError Foo throughMap m = runPromptT parseFoo3 $ \\k -&gt; case
M.lookup k m of Nothing -&gt; Left (MENotFound k) Just v -&gt; Right v ~~~

Hope you enjoy! Please feel free to leave a comment, find me on
[twitter](https://twitter.com/mstk "Twitter"), leave an issue on the
[github](https://github.com/mstksg/prompt), etc. --- and I'm usually on
freenode's *\#haskell* as *jle\`* if you have any questions!

Comparisons
-----------

To lay it all on the floor,

~~~haskell newtype PromptT a b t r = PromptT { runPromptTM :: forall m. Monad m
=&gt; (a -&gt; m (t b)) -&gt; m (t r) } ~~~

There is admittedly a popular misconception that I've seen going around that
equates this sort of type to `Free` from the *free* package. However, `Free`
doesn't really have anything significant to do with this. Sure, you might be
able to generate this type by using `FreeT` over a specifically chosen Functor,
but...this is the case for literally any Monad ever, so that doesn't really mean
much :)

It's also unrelated in this same manner to `Prompt` from the *MonadPrompt*
package, and `Program` from *operational* too.

One close relative to this type is `forall m. ReaderT (a -> m b) m r`, where
`prompt k = ReaderT ($ k)`. This is more or less equivalent to `Prompt`, but
still can't do the things that `PromptT` can do without a special instance of
Monad.

This type is also similar in structure to `Bazaar`, from the *lens* package. The
biggest difference that makes `Bazaar` unusable is because the RankN constraint
is only `Applicative`, not `Monad`, so a `Monad` instance is impossible.
Ignoring that (or if it's okay for you to only use the `Applicative` instance),
`Bazaar` forces the "prompting effect" to take place in the same context as the
`Traversable` `t`...which really defeats the purpose of this whole thing in the
first place (the idea is to be able to separate your prompting effect from your
application logic). If the `Traversable` you want to transform has a "monad
transformer" version, then you can somewhat simulate `PromptT` for that specifc
`t` with the transformer version.

It's also somewhat similar to the `Client` type from *pipes*, but it's also a
bit tricky to use that with a different effect type than the logic
`Traversable`, as well...so it has a lot of the same difference as `Bazaar`
here.

But this type is common/simple enough that I'm sure someone has it somewhere in
a library that I haven't been able to find. If you find it, let me know!
