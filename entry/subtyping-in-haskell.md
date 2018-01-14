Subtyping in Haskell
====================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/subtyping-in-haskell.html)

It is often said that Haskell does not have subtyping. While it is indeed true
that Haskell doesn't have *ad-hoc* subtyping, you can achieve something similar
with Haskell, RankN types, and some key choice of data types or typeclasses.
And, in many situations, you can build programs around it!

As a simple example, let's redesign the API of the *Control.Monad.Trans.State*
module from the *transformers* package. Here, they define

\`\`\`haskell data StateT s m a = StateT (s -&gt; m (a, s))

runStateT :: StateT s m a -&gt; s -&gt; m (a, s) evalStateT :: StateT s m a
-&gt; s -&gt; m a execStateT :: StateT s m a -&gt; s -&gt; m s

get :: StateT s m s put :: s -&gt; StateT s m () modify :: (s -&gt; s) -&gt;
StateT s m () state :: (s -&gt; (a, s)) -&gt; StateT s m a

type State s = StateT s Identity

runState :: State s a -&gt; s -&gt; (a, s) evalState :: State s a -&gt; s -&gt;
a execState :: State s a -&gt; s -&gt; s

\`\`\`
