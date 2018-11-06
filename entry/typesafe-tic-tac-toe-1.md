Type-safe Tic Tac Toe (Part 1)
==============================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/typesafe-tic-tac-toe-1.html)

One problem with adoption of dependent types in everyday programming, I think,
is that most examples out there are sort of small and self-contained. There
aren't *too* many larger-scale examples out there showing how dependent types
can permeate your whole program to make everything more robust and error-free.

So, this series will be implementing a type-safe *tic tac toe* game (a
medium-scale Haskell app) that can be played both on the console (using
Haskeline) and in the browser (using Miso), using some custom built AI. We will:

1.  Build up our core game engine, talking about what it really means to be type
    safe
2.  Use our type-safe engine to build type-safe controllers (AI, GUI)

This series will also be a mini-tutorial on the
*[decidable](https://hackage.haskell.org/package/decidable)* package that I just
recently released :) We will also be heavily using the
*[singletons](https://hackage.haskell.org/package/singletons)* library. Where
relevant, I will explain singletons concepts in brief. If you want a more
in-depth introduction to the *singletons* library, however, check out my
[Introduction to
Singletons](https://blog.jle.im/entries/series/+introduction-to-singletons.html)
series!

Type-Safety
-----------

First off, we should ask the question: what does it mean to be type-safe?

?????

The Specification
-----------------

We're going to create a type that represents a *valid* game state. The goal is
to make a GADT where you can only construct values whose types represent *valid*
game states. If we have a value of this type, then we know that the game state
must be valid.

A good way to start with this is by thinking of *induction rules* for defining a
valid state.

We'll say that there are two parts of a game state:

1.  The current board
2.  The current player

and that there are two ways of "constructing" a valid state:

1.  The empty board with player X is a valid state.
2.  If we have:

    -   A valid state with board *b* and current player *p*
    -   The game is still in play
    -   We can add a valid move by player *p* to board *b*

    Then the result of this move represents a new valid board *b*, with swapped
    player *p*.

This is a denotative way to talk about what it means for a state to be valid.

Note that our "type safety" is only as strong as the specification we just
wrote. Type safety using dependent types isn't omnipotent, and it can't read
your mind. However, there is a nice assurance that as long as your
*specification* is right, your program will work as expected. And hey, it's a
step up from the untyped case, where you can have a specification wrong, but
implement it incorrectly. With "type-safety", you cut out one huge area where
bugs come from: the implementation.

Alright, let's do this!

Valid State
-----------

First, we'll define the types we need to specify our state:

``` {.haskell}
$(singletons [d|
  data Piece = PX | PO
    deriving (Eq, Ord)

  type Board = [[Maybe Piece]]
  |])
```

A `Piece` will also represent our player -- either `PX` or `PO`. Our `Board`
will be a list of lists of `Maybe Piece`. If the spot contains `Nothing`, the
spot is unplayed; if the spot is `Just p`, then it means the spot has been
played by `p`.

And some values and functions we need to talk about empty boards and state
transformations:

``` {.haskell}
$(singletons [d|
  emptyBoard :: Board
  emptyBoard = [ [Nothing, Nothing, Nothing]
               , [Nothing, Nothing, Nothing]
               , [Nothing, Nothing, Nothing]
               ]

  altP :: Piece -> Piece
  altP PX = PO
  altP PO = PX
  |])
```

Let's just throw in a quick proof as a sanity check:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L62-L64

altP_cyclic :: Sing p -> AltP (AltP p) :~: p
altP_cyclic SPX = Refl @'PX
altP_cyclic SPO = Refl @'PO
```

With that in mind, we can write our valid state constructor. We'll do that with
two helper types that we will implement later. First, we'll use the
[decidable](https://hackage.haskell.org/package/decidable) library to talk about
the kind of a *type-level predicate*.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L66-L66

data InPlay :: Predicate Board
```

`InPlay` is a predicate that a given board is in-play; a value of type
`InPlay @@ b` is a witness or proof that a board is in play.

We also need to define a type for a valid update by a given player onto a given
board:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L81-L81

data Update :: Piece -> Board -> Board -> Type where
```

A value of type `Update p b1 b2` will represent a valid update to board `b1` by
player `p` to create a board `b2`.

And finally, our valid state constructor:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L68-L79

data GameState :: Piece -> Board -> Type where
    -- | The empty board is a valid state
    GSStart
        :: GameState 'PX EmptyBoard
    -- | We can also construct a valid game state if we have:
    GSUpdate
        :: forall p b1 b2. ()
        => InPlay          @@ b1     -- ^ a proof that b1 is in play
        -> Update    p        b1 b2  -- ^ a valid update
        -> GameState p        b1     -- ^ a proof that p, b1 are a valid state
        -- ---------------------------- then
        -> GameState (AltP p)    b2  -- ^ `AltP p`, b2 is a valid satte
```

And that's it --- a verified-correct representation of a game state, directly
transcribed from our plain-language denotative specification.

Now we just need to talk about `InPlay` and `Update`. In particular, we need:

1.  A definition of `Update`, and a way to turn user-input into a valid `Update`
    (or reject it if it isn't valid).
2.  A definition of `InPlay`, and a way to decide whether or not a given board
    `b` is `InPlay`. This is something that the appropriately named
    *[decidable](https://hackage.haskell.org/package/decidable)* library will
    help us with.

### Update

Let's go about what thinking about what defines a valid update. Remember, the
kind we wanted was:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L81-L81

data Update :: Piece -> Board -> Board -> Type where
```

An `Update p b1 b2` will be a valid update of `b1` by player `p` to produce
`b2`. So, we need to:

1.  Produce `b2` from `b1`
2.  Be sure that the move is valid --- namely, that it is placed in a clean spot
    so that it doesn't overwrite any previous moves.

Producing `b2` from `b1` is simple enough as a type family. In fact, we can just
use the *[lens-typelevel](https://hackage.haskell.org/package/lens-typelevel)*
library to update our nested list:

``` {.haskell}
$(singletonsOnly [d|
  placeBoard :: N -> N -> Piece -> Board -> Board
  placeBoard i j p = set (ixList i . ixList j) (Just p)
  |])
```

This is just lenses --- `set l x` is a function that sets the field specified by
`l` to `x`. Here, we set the jth item of the ith list to be `Just p`. That means
we can now produce `b2` from `b1` -- it's just `PlaceBoard i j p b1`.

Here, `N` is the peano nat type (a lot of libraries define it, but it's also
defined as a uility in *lens-typelevel*). It's essentially `[()]` (which makes
it useful as an index type), or:

``` {.haskell}
data N = Z | S N
```

A natural number is either zero, or the successor of another natural number.
`S (S Z)`, for example, would represent 2.

The trickier part is making sure that the spot at *(i, j)* isn't already taken.
For that, we'll introduce a common helper type to say *what* the piece at spot
*(i, j)* is:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L95-L95

data Coord :: (N, N) -> [[k]] -> k -> Type where
```

A `Coord '(i, j) xss x` is a data type that specifies that the jth item in the
ith list in `b` is `p`.

And we require `Update` to only be constructable if the spot at *(i, j)* is
`Nothing`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L81-L86

data Update :: Piece -> Board -> Board -> Type where
    MkUpdate
        :: forall i j p b. ()
        => Coord '(i, j) b 'Nothing         -- ^ If the item at (i, j) in b is Nothing
        -- ------------------------------------- then
        -> Update p b (PlaceBoard i j p b)  -- ^ Placing `Just p` at i, j is a valid update
```

`Update` is now defined so that, for `Update p b1 b2`, `b2` is the update via
placement of a piece `p` at some position in `b1`, where the placement does not
overwrite a previous piece. Note that our `MkUpdate` constructor only has four
"free" variables, `i`, `j`, `p`, and `b`. If we use `MkUpdate`, it means that
the "final board" is fully determined from only `i`, `j`, `p`, and `b`.

#### Coord

Now we need to define `Coord`. We're going to do that in terms of a simpler type
that is essentially the same for normal lists --- a type:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L88-L88

data Sel :: N -> [k] -> k -> Type where
```

A value of type `Sel n xs x` says that the nth item in `xs` is `x`.

We can define this type inductively, similar to the common
[`Index`](http://hackage.haskell.org/package/type-combinators-0.2.4.3/docs/Data-Type-Index.html)
data type. We can mention our induction rules:

1.  The first item in a list as at index 0 (`Z`)
2.  If an item is at index `n` in list `as`, then it is also at index `S n` in
    list `b ': as`.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L88-L93

data Sel :: N -> [k] -> k -> Type where
    -- | The first item in a list is at index ''Z'
    SelZ :: Sel 'Z (a ': as) a
    SelS :: Sel     n        as  a  -- ^ If item `a` is at index `n` in list `as`
         -- ---------------------------- then
         -> Sel ('S n) (b ': as) a  -- ^ Item `a` is at index `S n` in list `b : as`
```

For example, for the type-level list `'[10,5,2,8]`, we can make values:

``` {.haskell}
SelZ             :: Sel         'Z   '[10,5,2,8] 10
SelS SelZ        :: Sel     ('S 'Z)  '[10,5,2,8] 5
SelS (SelS SelZ) :: Sel ('S ('S 'Z)) '[10,5,2,8] 2
```

etc.

We can then use this to define `Coord`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L95-L100

data Coord :: (N, N) -> [[k]] -> k -> Type where
    (:$:) :: forall i j rows row p. ()
          => Sel i rows row         -- ^ If the ith list in `rows` is `row`
          -> Sel j row  p           -- ^ And the jth item in `row` is `p`
          -- --------------------------- then
          -> Coord '(i, j) rows p   -- ^ The item at (i, j) is `p`
```

A `Coord '(i, j) rows piece` contains a selection into the ith list in `rows`,
to get `row`, and a selection into the jth item in `row`, to get `piece`.

### Trying it out

That's it! Let's see if we can generate some sensible `Update`s, and maybe even
play a sample game.

We'll start with the `EmptyBoard`, and let's add a piece by `PX` at the middle
spot, index (1,1). This means we want `SelS SelZ :$: SelS SelZ` (a `Coord` with
two indexes into spots 1 and 1) applied to `MkUpdate`. We'll use
*-XTypeApplications* to specify the type variables `p` and `b`:

``` {.haskell}
ghci> :t MkUpdate @_ @_ @'PX @EmptyBoard (SelS SelZ :$: SelS SelZ)
Update
  'PX
  '[ '[ 'Nothing, 'Nothing , 'Nothing],
     '[ 'Nothing, 'Nothing , 'Nothing],
     '[ 'Nothing, 'Nothing , 'Nothing]
   ]
  '[ '[ 'Nothing, 'Nothing , 'Nothing],
     '[ 'Nothing, 'Just 'PX, 'Nothing],
     '[ 'Nothing, 'Nothing , 'Nothing]
  ]
```

Nice! This update produces exactly he board expected.

Let's see if we can see if this prevents us from creating an illegal board.
We'll take the result board and see if we can place a `PO` piece there:

``` {.haskell}
ghci> let NewBoard = '[ '[ 'Nothing, 'Nothing , 'Nothing ]
                      , '[ 'Nothing, 'Just 'PX, 'Nothing ]
                      , '[ 'Nothing, 'Nothing , 'Nothing ]
                      ]
ghci> :k MkUpdate @_ @_ @'PO @NewBoard (SelS SelZ :$: SelS SelZ)
    • Couldn't match type ‘'Nothing’ with ‘'Just 'PX’
```

Right! That's because `SelS SelZ :&: SelS SellZ`, applied to `NewBoard`, gives
`Coord '('S 'Z, 'S 'Z) NewBoard ('Just 'PX)`. However, in order to be used with
`MkUpdate`, the final field has to be `'Nothing`, not `'Just 'PX`. So, type
error.

### Type-safe Play

At the end of this all, we finally have enough to write a truly type-safe `play`
function that allows us to play a round of our game!

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L102-L108

play
    :: forall i j p b. ()
    => InPlay @@ b
    -> Coord '(i, j) b 'Nothing
    -> GameState p b
    -> GameState (AltP p) (PlaceBoard i j p b)
play r c = GSUpdate r (MkUpdate c)
```

`play` is basically the entirety of our game engine! (Minus defining `InPlay`,
which we will take care of later). It'll take our new move and a proof that the
game is still in play, and return a updated new game state. Our entire game is
done, and type-safe! It's impossible to play a game in an incorrect way! (once
we define `InPlay`).

Let's try out a few rounds in ghci, using `undefined` instead of a proper
`InPlay` for now:

``` {.haskell}
ghci> g1 = play undefined (SelS SelZ :$: SelS SelZ) GSStart   -- X plays (1,1)
ghci> :t g1
GameState 'PO
    '[ '[ 'Nothing, 'Nothing , 'Nothing]
     , '[ 'Nothing, 'Just 'PX, 'Nothing]
     , '[ 'Nothing, 'Nothing , 'Nothing]
     ]

ghci> g2 = play undefined (SelZ :$: SelS SelZ) g1   -- O plays (0,1)
ghci> :t g2
GameState 'PX
    '[ '[ 'Nothing, 'Just 'PO, 'Nothing]
     , '[ 'Nothing, 'Just 'PX, 'Nothing]
     , '[ 'Nothing, 'Nothing , 'Nothing]
     ]

ghci> g3 = play undefined (SelZ :$: SelS SelZ) g2   -- X plays (1,0)
ghci> :t g3
GameState 'PO
    '[ '[ 'Nothing , 'Just 'PO, 'Nothing]
     , '[ 'Just 'PX, 'Just 'PX, 'Nothing]
     , '[ 'Nothing , 'Nothing , 'Nothing]
     ]

ghci> g4 = play undefined (SelS SelZ :$: SelS SelZ) g3   -- O plays (1,1)
    • Couldn't match type ‘'Just 'PX’ with ‘'Nothing’

ghci> g4 = play undefined (SelS (SelS (SelS SelZ)) :$: SelZ) g3  -- O plays (3,0)
    • Couldn't match type ‘'[]’ with ‘'Nothing ': as’
```

`play` enforces:

1.  Turns are always alternating X, then O
2.  We cannot place a piece in a previously-played spot
3.  We cannot place a piece out-of-bounds.

Decision Functions and Views
----------------------------

This seems nice, but we're forgetting an important part. `play` requires us to
only give valid inputs, and enforces that the inputs are valid. However, how do
we *create* valid inputs, in a way that satisfies `play`?

As we'll see, this is one of the core problems that dependently typed
programming gives us tools to solve.

At this point, we've reached the important part of any "type-safe" application:
*decision functions* and dependent *views*. *Decision functions* let you slowly
refine your more general values (types) into more specific valid types. *Views*
let you sort out your our values into more "useful" perspectives.

We're going to allow for users to pick to move at any natural number pair
(`(N, N)`), but only *some* of those natural numbers can become valid updates.
In particular, we only allow an `Update` to be made if `(N, N)` represent valid
updates.

What are two ways this can go wrong? Well, if we allow the user to enter any two
natural numbers, here are all of the potential outcomes:

1.  We might get a coordinate that is out of bounds in x
2.  We might get a coordinate that is in bounds in x, but out of bounds in y
3.  We might get a coordinate that is in bounds in x, in bounds in y, but
    referencing a position that has already been played.
4.  We might get a coordinate that is in bounds in x, in bounds in y, and
    references a blank position. This is the only "success" case.

Note that we could also just have a "success or nor success" situation, but,
because we might want to provide feedback to the user, it is helpful to not be
"[decision-blind](https://twitter.com/cattheory/status/887760004622757890)" (a
cousin of [boolean
blindness](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/)).

We'll call these potential "views" out of `(N, N)` with respect to some board
`b`. Let's create a data type representing all of these possibilities (using
`OutOfBounds` as a placeholder predicate for an out-of-bounds coordinate):

``` {.haskell}
-- | Placeholder predicate if a given number `n` is out of bounds for a given
-- list
data OutOfBounds n :: Predicate [k]

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L115-L125

data Pick :: (N, N, Board) -> Type where
    -- | We are out of bounds in x
    PickOoBX   :: OutOfBounds i @@ b                         -> Pick '(i, j, b)
    -- | We are in-bounds in x, but out of bounds in y
    PickOoBY   :: Sel i b row        -> OutOfBounds j @@ row -> Pick '(i, j, b)
    -- | We are in-bounds in x, in-bounds in y, but spot is taken by `p`.
    -- We include `Sing p` in this constructor to potentially provide
    -- feedback to the user on what piece is already in the spot.
    PickPlayed :: Coord '(i, j) b ('Just p) -> Sing p        -> Pick '(i, j, b)
    -- | We are in-bounds in x, in-bounds in y, and spot is clear
    PickValid  :: Coord '(i, j) b 'Nothing                   -> Pick '(i, j, b)
```

(A value of type `OutOfBounds n @@ xs` is a witness that `xs` satisfies the
`OutOfBounds n` --- that is, `n` is out of bounds of `xs`. More on this later!)

So, if we have an `(N, N, Board)`, we should be able to categorize it into one
of each of these potential views.

### Proving functions

This is the job of a "decision function"; in this case, actually, a "proving
function", or a "viewing function". We need to be able to write a function:

``` {.haskell}
pick :: Sing '(i, j, b)
     -> Pick '(i, j, b)
```

That is, given any coordinate and board, we should be able to *totally*
categorize it to one of the four categories (think of them like four
perspectives/classifications), without exception.

This can be considered the boundary between the unsafe and the safe world. And,
to me, this is the "hard part" about dependently typed programming :)

If we want to take any `i`, `j`, and `b`, and turn it into a valid `Pick`,
remember that a valid `Pick '(i, j, b)` contains a `Coord '(i, j) b 'Nothing`,
which contains a `Sel i b row` and a `Sel j row 'Nothing`. So we need to
"convert" some `i`, `j` into a `Sel i b row` and `Sel j row 'Nothing`.

### Existential return types

Essentially, we want a function:

``` {.haskell}
sel :: Sing i
    -> Sing xs
    -> Sel i xs ??????
```

where `????` is whatever value is in `xs` at index `i`. It's something we might
not know directly from the input types, necessarily, because it might not even
exist (the list might be too short).

This pattern --- where we don't know the type of something until after we
receive the function inputs --- is something you might recognize as an
*existential type*, implementable using a *dependent pair* (or dependent sum).

We could write our own dependent pair from scratch, but this is a good
opportunity to practice using *singletons* library's versatile "anonymous
dependent pair" type, `Σ` (or `Sigma`), from *Data.Singletons.Sigma*.

``` {.haskell}
data Sigma k :: Predicate k -> Type where
    (:&:) :: Sing x -> (p @@ x) -> Sigma k f

type Σ k = Sigma k
```

A value of type `Sigma k p` contains an `p @@ x` (a witness that `x` satisfies
`x`), existentially *hiding* the `x`, and also `x` itself (as `Sing x`; remember
that `Sing x` is essentially a value-level representation of type `x`).

We can use this to return a `Sel n xs ????`, *hiding* the `???`. We can't
directly give `Sel n xs` to `Sigma` (because it expects a `Predicate k`, not a
`k -> Type`), but we can turn a type constructor into a `Predicate` using
`TyPred`, a convenient combinator from the *decidable* library:

``` {.haskell}
TyPred :: (k -> Type) -> Predicate k
```

``` {.haskell}
ghci> :k Pick
(N, N, Board) -> Type

ghci> :k TyPred Pick
Predicate (N, N, Board)

ghci> :k Sel 'Z EmptyBoard
[Maybe Piece] -> Type

ghci> :k TyPred (Sel 'Z EmptyBoard)
Predicate [Maybe Piece]
```

Let's make sure this type works like we expect it to. We want a
`Σ k (TyPred (Sel n xs))` to contain the `x` at position `n` in `xs`, *and* the
`Sel` into that position.

First, to make things a little less verbose, let's define a type synonym for
`Σ k (TyPred (Sel n xs))`, `SelFound n xs`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L139-L139

type SelFound n (xs :: [k]) = Σ k (TyPred (Sel n xs))
```

And making some sample witnesses to ensure we are thinking about things
correctly:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L141-L143

selFoundTest1 :: SelFound 'Z '[ 'True, 'False ]
selFoundTest1 = STrue :&: SelZ
                       -- ^ Sel 'Z '[ 'True, 'False ] 'True
```

Note that `SFalse :&: SelZ` would be a type error, because the second half
`SelZ` would be `Sel :: 'Z '[ 'True, 'False ] 'True` (because `'True` is the 0th
item in the list), so we have to have the first half match `'True`, with
`STrue`.

We can write a witness for `SelFound ('S 'Z) '[ 'True, 'False ]`, as well, by
giving the value of the list at index 1, `'False`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L145-L147

selFoundTest2 :: SelFound ('S 'Z) '[ 'True, 'False ]
selFoundTest2 = SFalse :&: SelS SelZ
                        -- ^ Sel ('S 'Z) '[ 'True, 'False ] 'False
```

Before moving on, I recommend trying to write some values of type
`SelFound n xs` for different `n :: N` and `xs :: [a]`, to see what type-checks
and what doesn't. It'll help you get a feel of the types we are working with,
which might be more advanced than types you might encounter in everyday Haskell
programming.

Now, we now have enough tools to write the type of the function we would like:

``` {.haskell}
sel :: Sing n
    -> Sing xs
    -> SelFound n xs
```

Remember, a `SelFound n xs` contains both a `Sel n xs x` *and* a `Sing x`: a
selection into the `i`th item in `xs` (the `Sel n xs x`), and also the item
itself (the `Sing x`).

We can start writing this, but the type system will soon show you where you run
into problems. And that's one of the best things about type systems! They help
you realize when you're trying to do something that doesn't make sense.

``` {.haskell}
sel :: Sing n
    -> Sing xs
    -> SelFound n xs
sel = \case
    SZ -> \case
      SNil -> _ :&: _
```

Things start out pretty standard. We want to match on all potential constructors
of `N` and `[a]`: `N` has `Z` and `S`, so we match on singleton constructors
`SZ` and `SS`; `[a]` has `[]` and `(:)`, so we match on singleton constructors
`SNil` and `SCons`.

If you ask ghc what goes in the typed holes, it'll say that we need a `Sing x`
and a `Sel 'Z '[] x` (which is because matching on `SZ` tells us we are in `'Z`,
and matching on `SNil` tells us we are in `'[]`).

Remember that the `x` is supposed to be the `n`th item in `xs`. Here, in this
pattern match branch, we want the zeroth (first) item in `[]`. This doesn't
exist! That's because there is no item in `[]`, so there is nothing we can put
for the `Sing x`...there's also nothing we could put for the `Sel`, since there
is no constructor of `Sel` that returns a `Sel n '[]` (the constructors of `Sel`
all return `x ': xs`, never `Nil`).

So, this branch is impossible to fulfil. We know now that we made a large
conceptual error (aren't types great?)

The problem? Well, indexing into item `n` in list `xs` *might not always
succeed*. We might try to index into an empty list, so we can't ever get a
result!

### Decision Functions

What we need is not a *proving function*, but, rather, a *decision* function. A
decision function for a predicate `P` is a function:

``` {.haskell}
decidePred :: Sing x
           -> Decision (P @@ x)
```

That is, instead of producing a `P @@ x` directly, we produce a
`Decision (P @@ x)`. Here, `Decision` is:

``` {.haskell}
data Decision a
    = Proved     a                -- ^ `a` is provably true
    | Disproved (a -> Void)       -- ^ `a` is provably false

-- | The type with no constructors.  If we have a function `a -> Void`, it must
-- mean that no value of type `a` exists.
data Void
```

A decision function means that, for any `x`, we can say that either `P @@ x` can
be proven true or can be proven false. See \[this section\]\[singletons-decide\]
for a deeper discussion on why `Decision` has both the `Proved` and `Disproved`
branch. Essentially, we keep track of "provably false" because we can use it
later to build other useful decision functions and proving functions.

We use decision functions when we want to "conditionally prove" something --- it
might be true, or it might not be (but definitely one or the other). It might
exist, or it might not. We can construct the view, or we can't. Whatever the
perspective, it's always one or the other.

`sel` fits this category: for a `Sel n xs ????`, either `n` is "in bounds" of
`as` (and we can prove this with the item `x` in `xs`), or `n` is "out of
bounds". Either we get the `x` out of `xs` at slot `n`, or we prove that no
possible `x` exists in `xs` at slot `n`.

#### Deciding SelFound

Enough talk, let's get to it!

Let's write our first dependently typed function. We start the same way --- by
looking at every possible constructor of `N` and `[a]`:

``` {.haskell}
selFound
    :: Sing n
    -> Sing xs
    -> Decision (SelFound n xs)
selFound = \case
    SZ -> \case
      SNil         -> _
      x `SCons` xs -> _
    SS n -> \case
      SNil         -> _
      x `SCons` xs -> _
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
