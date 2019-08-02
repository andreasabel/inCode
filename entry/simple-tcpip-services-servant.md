Setting up a dead-simple TCP/IP service using servant
=====================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/simple-tcpip-services-servant.html)

In my time I've written a lot of throwaway binary TCP/IP services (servers and
services you can interact with over an internet connection, through command line
interface or GUI). For me, this involves designing a protocol from scratch every
time with varying levels of hand-rolled authentication and error detection (Send
this byte for this command, this byte for this other command, etc.). Once I
design the protocol, I then have to write both the client and the server ---
something I usually do from scratch over the raw TCP streams.

This process was fun (and informative) the first few times I did it, but
spinning it up from scratch again every time discouraged me from doing it very
often. However, thankfully, with the
*[servant](https://hackage.haskell.org/package/servant)* haskell library,
writing a TCP server/client pair for a TCP service becomes dead-simple --- the
barrier for creating one fades away that designing/writing a service becomes a
tool that I reach for immediately in a lot of cases without second thought.

*servant* is usually advertised as a tool for writing web servers, web
applications, and REST APIs, but it's easily adapted to write non-web things as
well (especially with the help of
*[servant-client](https://hackage.haskell.org/package/servant-client)* and
*[servant-cli](https://hackage.haskell.org/package/servant-cli)*). Let's dive in
and write a simple TCP/IP service (a todo list manager) to see how
straightforward the process is!

Todo API
--------

As an example, we'll work through building one of my favorite self-contained
mini-app projects, a [Todo list manager a la todo-mvc](http://todomvc.com/). Our
service will provide functionality for:

1.  Viewing all tasks and their status
2.  Adding a new task
3.  Setting a task's completion status
4.  Deleting a task
5.  Pruning all completed tasks

To facilitate doing this over an API, we'll assign each task a task ID when it
comes in, and so commands 3 and 4 will require a task ID.

To formally specify our API:

1.  `view`: View all tasks by their ID, status, and description. Optionally be
    able to filter for only incomplete tasks.
2.  `add`: Given a new task description, insert a new uncompleted task. Return
    the ID of the new task.
3.  `set`: Given a task ID and an updated status, update the task's status.
4.  `delete`: Given a task ID, delete the task.
5.  `prune`: Remove all completed tasks. Returns all the task IDs that where
    deleted.

We can state this using servant's type level DSL, using an `IntMap` to represent
the current tasks and an `IntSet` to represent a set of task IDs.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/servant-services/Api.hs

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Proxy
import           Data.Text (Text)
import           GHC.Generics
import           Servant.API

data Task = Task
    { taskStatus :: Bool
    , taskDesc   :: Text
    }
  deriving (Show, Generic)
instance ToJSON   Task
instance FromJSON Task

type TodoApi =
      "list"   :> QueryFlag "filtered"                      :> Get  '[JSON] (IntMap Task)
 :<|> "add"    :> QueryParam' '[Required] "desc" Text       :> Post '[JSON] Int
 :<|> "set"    :> Capture "id" Int :> Capture "status" Bool :> Post '[JSON] ()
 :<|> "delete" :> Capture "id" Int                          :> Post '[JSON] ()
 :<|> "prune"                                               :> Post '[JSON] IntSet

todoApi :: Proxy TodoApi
todoApi = Proxy
```

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
