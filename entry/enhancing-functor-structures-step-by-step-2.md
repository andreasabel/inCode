Enhancing Functor Structures Step-By-Step (Part 2)
==================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/enhancing-functor-structures-step-by-step-2.html)

Parsing and Serializing Invariantly
-----------------------------------

At this point, we have:

1.  Started with a simple ADT representing the structure we want to be able to
    express
2.  Enhanced that simple ADT with Covariant Functor capabilities, in order to
    interpret it as a parser
3.  Enhanced that original simple ADT with Contravariant Functor, in order to
    interpret it as a serializer.

From this, it seems the next logical step would be to add *both* enhancements to
the same structure!

There are some clear benefits to this --- for example, we can now ensure that
our "serialization" and "parsing" functions are always "in sync". If we defined
a separate process/type for serializing and a separate process/type for parsing,
then it's possible we might accidentally make errors in keeping them in
sync...one might use a different tag, or we might make changes to one but not
the other during refactoring.

Like before, the main thing we need to change at the fundamental level is
`Primitive`:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L45-L48

data Primitive a =
      PString (a -> String)     (String     -> Maybe a)
    | PNumber (a -> Scientific) (Scientific -> Maybe a)
    | PBool   (a -> Bool)       (Bool       -> Maybe a)
```

We're just basically combining the additions we made to enable parsing with the
additions we made to enable serialization. Our new `Primitive` type gives us the
capability to do both!

We call this new `Primitive` an ["Invariant"
Functor](https://hackage.haskell.org/package/invariant/docs/Data-Functor-Invariant.html):
these are functors that give you "both" capabilities: interpreting covariantly
*and* contravariantly.

### DivAp and DecAlt

By now, we know the drill. We also need to change our `RecordType` and `SumType`
constructors to get the right type of container.

``` {.haskell}
-- Covariant Schema
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L27-L31

data Schema a =
      RecordType  (Ap Field a)
    | SumType     (ListF Choice a)
    | SchemaLeaf  (Primitive a)
  deriving Functor

-- Contravariant Schema
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L27-L30

data Schema a =
      RecordType  (Div Field a)
    | SumType     (Dec Choice a)
    | SchemaLeaf  (Primitive a)
```

For the covariant `RecordType`, we used `Ap Field a`. For the contravariant
`RecordType`, we used `Div Field a`. Is there a type that combines *both* `Ap`
and `Div`?

Ah, we're in luck! We have
*[DivAp](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Invariant-DivAp.html)*
from the *functor-combinatotrs* library...which is named to invoke the idea of
having both `Ap` and `Div` capabilities, combined together.

For the covariant `SumType`, we used `ListF Choice a`. For the contravariant
`SumType`, we used `Dec Choice a`. Is there a type that combines *both* `ListF`
and `Dec`?

Ah hah, if we look nearby `DivAp`, we see the answer:
*[DecAlt](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Invariant-DecAlt.html)*!
It combines both `ListF` and `Dec`.

### Building an Invariant Schema

Now let's wire it up:

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L30-L48

data Schema a =
      RecordType  (DivAp   Field  a)
    | SumType     (DecAlt Choice a)
    | SchemaLeaf  (Primitive a)

data Field a = Field
    { fieldName  :: String
    , fieldValue :: Schema a
    }

data Choice a = Choice
    { choiceName  :: String
    , choiceValue :: Schema a
    }

data Primitive a =
      PString (a -> String)     (String     -> Maybe a)
    | PNumber (a -> Scientific) (Scientific -> Maybe a)
    | PBool   (a -> Bool)       (Bool       -> Maybe a)
```

Writing a schema using this type is going to be very similar to writing the
contravariant version.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L61-L76

customerSchema :: Schema Customer
customerSchema = SumType $
    swerve (\case CPerson x y -> Left (x,y); CBusiness x -> Right x) (uncurry CPerson) CBusiness
        (inject Choice
          { choiceName  = "Person"
          , choiceValue = RecordType $ gathered
              (inject Field { fieldName = "Name", fieldValue = SchemaLeaf pString })
              (inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    })
          }
        )
        (inject Choice
          { choiceName  = "Business"
          , choiceValue = RecordType $
              inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt }
          }
        )
```

The main difference is, while `decide` expects the `a -> Either b c` splitting
function, `swerve` (the invariant `DecAlt` equivalent) expects also the
functions to "recombine" the `b` and `c` back to `a`. We also note that the
invariant version of `divided` is `gathered`.

``` {.haskell}
swerve
    :: (a -> Either b c)    -- ^ break into branches
    -> (b -> a)             -- ^ put the branch back into the original input
    -> (c -> a)             -- ^ put the branch back into the original input
    -> DecAlt f b           -- ^ handle first branch
    -> DecAlt f c           -- ^ handle second branch
    -> DecAlt f a           -- ^ overall handler

swerve
    :: (Customer -> Either (String, Int) Int)   -- ^ break into branches
    -> ((String, Int) -> Customer)              -- ^ put the CPerson branch back into a Customer
    -> (Int -> Customer)                        -- ^ put the CBusiness branch back into a Customer
    -> DecAlt Choice (String, Int)              -- ^ handle CPerson branch
    -> DecAlt Choice Int                        -- ^ handle CBusiness branch
    -> DecAlt Choice Customer
```

And `gathered` works like:

``` {.haskell}
gathered
    :: DivAp f a          -- ^ first handler
    -> DivAp f b          -- ^ second handler
    -> DivAp f (a, b)     -- ^ merged handler

gathered
    :: DivAp Field String          -- ^ handle the cpName field
    -> DivAp Field Int             -- ^ handle the cpAge field
    -> DivAp Field (String, Int)   -- ^ handle both together
```

### Using Invariant Schema

It looks like we mostly did all the work already. Writing `schemaDoc`,
`schemaParser`, and `schemaToValue`, we can re-use pretty much all of our code!
The main (unfortunate) difference is that instead of using `interpret` in every
case, we can use `runCoDivAp` to run our `DivAp` in a covariant setting, and
`runContraDivAp` to run our `DivAp` in a contravariant setting (similarly for
`runCoDecAlt` and `runContraDecAlt`). Another small difference is that
`icollect` doesn't quite work properly on `DivAp`/`DecAlt`, so we have to
convert them to `Ap` and `Dec` first.[^1]

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L78-L154

schemaDoc
    :: String       -- ^ name
    -> Schema x     -- ^ schema
    -> PP.Doc a
schemaDoc title = \case
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep $
          icollect (\fld -> "*" PP.<+> PP.indent 2 (fieldDoc fld)) (divApAp fs)
      ]
    SumType cs    -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep $
          icollect choiceDoc (decAltDec cs)
      ]
    SchemaLeaf p  -> PP.pretty (title <> ":")
              PP.<+> primDoc p
  where
    fieldDoc :: Field x -> PP.Doc a
    fieldDoc Field{..} = schemaDoc fieldName fieldValue
    choiceDoc :: Choice x -> PP.Doc a
    choiceDoc Choice{..} = schemaDoc choiceName choiceValue
    primDoc :: Primitive x -> PP.Doc a
    primDoc = \case
      PString _ _ -> "string"
      PNumber _ _ -> "number"
      PBool   _ _ -> "bool"

schemaParser
    :: Schema a
    -> A.Parse String a
schemaParser = \case
    RecordType fs -> runCoDivAp fieldParser fs
    SumType    cs -> runCoDecAlt choiceParser cs
    SchemaLeaf p  -> primParser p
  where
    choiceParser :: Choice b -> A.Parse String b
    choiceParser Choice{..} = do
      tag <- A.key "tag" A.asString
      unless (tag == choiceName) $
        A.throwCustomError "Tag does not match"
      A.key "contents" $ schemaParser choiceValue
    fieldParser :: Field b -> A.Parse String b
    fieldParser Field{..} = A.key (T.pack fieldName) (schemaParser fieldValue)
    primParser :: Primitive b -> A.Parse String b
    primParser = \case
      PString _ f -> A.withString $
        maybe (Left "error validating string") Right . f
      PNumber _ f -> A.withScientific $
        maybe (Left "error validating number") Right . f
      PBool _ f -> A.withBool $
        maybe (Left "error validating bool") Right . f

schemaToValue
    :: Schema a
    -> a
    -> Aeson.Value
schemaToValue = \case
    RecordType fs -> Aeson.object
                   . getOp (runContraDivAp fieldToValue fs)
    SumType    cs -> getOp (runContraDecAlt choiceToValue cs)
    SchemaLeaf p  -> primToValue p
  where
    choiceToValue :: Choice x -> Op Aeson.Value x
    choiceToValue Choice{..} = Op $ \x -> Aeson.object
      [ "tag"      Aeson..= T.pack choiceName
      , "contents" Aeson..= schemaToValue choiceValue x
      ]
    fieldToValue :: Field x -> Op [Aeson.Pair] x
    fieldToValue Field{..} = Op $ \x ->
        [T.pack fieldName Aeson..= schemaToValue fieldValue x]
    primToValue :: Primitive x -> x -> Aeson.Value
    primToValue = \case
      PString f _ -> Aeson.String . T.pack . f
      PNumber f _ -> Aeson.Number . f
      PBool   f _ -> Aeson.Bool . f
```

And there we have it --- a fully functional bidirectional parser schema type
that we assembled step-by-step, adding each piece incrementally and exploring
the space until we found something useful for us.

A cute function we could write to tie things together would be one that does a
round-trip, serializing and then parsing, to make sure things worked properly.

``` {.haskell}
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L156-L160

testRoundTrip
    :: Schema a
    -> a
    -> Either (A.ParseError String) a
testRoundTrip sch = A.parseValue (schemaParser sch) . schemaToValue sch
```

``` {.haskell}
ghci> testRoundTrip customerSchema (CPerson "Sam" 40)
Right (CPerson {cpName = "Sam", cpAge = 40})
```

An Alternative Invariant Strategy
---------------------------------

The thought process "I want to use both `Div` and `Ap`, let's just look for
`DivAp`" is kind of nice and straightforward, but it *is* a little convenient
that this type existed in the first place. If we look into the construction of
`Ap` and `Div`, we probably *could* be able to invent a data type of our own if
we wanted to...but that's a lot of heavy lifting.

There's a major downside in using `DivAp` and `DecAlt` too, that make their
ergonomics not so great when building them up. A major part about what makes
`Ap` and `ListF` (and, to an extent, `Div` and `Dec`) so nice to use is that are
instances of popular Haskell typeclasses like `Applicative` and `Alternative`
(or, `Plus`, as we use it) and using `Applicative` and `Alternative` interfaces
are pretty common in Haskell. Because of this, they are pretty comfortable for
most Haskellers to use.

However, `DivAp` and `DecAlt` aren't really instances of any commonly used
typeclass (aside from `Invariant`). There *could* be a typeclass for
"combination of `Applicative` and `Divisible`" and "combination of `Plus` and
`Conclude`", but that doesn't really exist in popular Haskell. So you really
don't have any nice interface for them other than just using functions
specifically written for them, like `gather` and `swerve`, which may feel ad-hoc
or unintuitive.

Luckily, there's another way to achieve the same goals and also be able to take
advantage of our favorite familiar interfaces. We can "add Contravariance"
directly into `Ap` itself, using
[`Pre`](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor-Route.html#t:Pre).
A value of type:

``` {.haskell}
Ap (Pre a Field) b
```

will "produce" `b`s covariantly...but will "consume" `a`s contravariantly. You
can think of the `Pre a` as adding an "tunnel" to guide the `a` to each `Field`
in the `Ap`.

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

[^1]: These are unfortunate consequences of the fact that there is no general
    typeclass that contains both `Applicative` and `Divisible` together, or no
    typeclass that contains both `Plus` and `Conclude` together. If these
    existed, we could just use `interpret` for all four of those functions, and
    `icollect` would work fine as well.
