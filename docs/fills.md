# Writing Fills with helper functions

Fills are how you fill-in the Blanks in your [templates](templates).

There are lots of helper functions for writing fills, listed
below. You should be able to do almost everything you want with
these helper functions! But if you want to do something special, you
can [write fills "from scratch"](fills-fromscratch). It's a little more complicated, so if
you have questions don't hesitate to open an issue and ask.

Larceny helper function tend to have at least two variants: a pure version and a
stateful, potentially side-effecting version. The variants are exactly the same
except the latter takes a `StateT s IO` version of whatever the pure version
takes. (This will make more sense when you see the type signatures.)

## `textFill :: Text -> Fill s`

A plain text fill, HTML escaped.
```
textFill "This text will be escaped and displayed in place of the blank"
```

## `rawTextFill :: Text -> Fill s`

A plain text fill, without HTML escaping.

```
rawtextFill "This text will be displayed in place of the blank, <em>unescaped</em>"
```

## `textFill' :: StateT s IO Text -> Fill s` 

Use state or IO, then fill in some text.

```
-- getTextFromDatabase :: StateT () IO Text
textFill' getTextFromDatabase
```

## `rawTextFill' :: StateT s IO Text -> Fill s`

Use state or IO, then fill in some unescaped text.

```
-- getTextFromDatabase :: StateT () IO Text
rawtextFill' getTextFromDatabase
```

## `mapSubs :: (a -> Substitutions s) -> [a] -> Fill s`

Create substitutions for each element in a list and fill the child nodes with
those substitutions.

```
<members><name /></members>

("members", mapSubs (\name -> subs [("name", textFill name)])
                     ["Bonnie Thunders", "Donna Matrix", "Beyonslay"]
```

Result:

`> Bonnie Thunders Donna Matrix Beyonslay`

## `fillChildren :: Fill s`

Fill in the child nodes of the blank with substitutions already available.

```
<no-op><p>Same</p></no-op>

("no-op", fillChildren)
```

Result:

`> <p>Same</p>`

## `fillChildrenWith :: Substitutions s -> Fill s`

Fill in the child nodes of the blank with new substitutions.

```
<member><name /></member>

("skater", fillChildrenWith (subs $ [("name", textFill "Bonnie Thunders")]))
```

Result:

`> Beyonslay`

## `fillChildrenWith' :: StateT s IO (Substitutions s) -> Fill s`

Use substitutions with State and IO.

```
<changeTheWorld><results /></changeTheWorld>

-- doABunchOfStuffAndGetSubstitutions :: StateT () IO (Substitutions ())
("changeTheWorld", fillChildrenWith' doStuffAndGetSubstitutions)
```

Result (perhaps):

`> This template did IO!`

## `maybeFillChildrenWith :: Maybe (Substitutions s) -> Fill s`

Fill with substitutions if those substitutions are provided.

```
<ifDisplayUser><userName /></ifDisplayUser>

("ifDisplayUser", maybeFillChildrenWith
                    (Just $ subs' ("userName", textFill "Bonnie Thunders")))
```
Result:

`> Bonnie Thunders`

## `maybeFillChildrenWith' :: StateT s IO (Maybe (Substitutions s)) -> Fill s`

Use state and IO and maybe fill in with some substitutions.

```
<ifLoggedIn>Logged in as <userName /></ifLoggedIn>

("ifLoggedIn", maybeFillChildrenWith' $ do
                  mUser <- getLoggedInUser -- returns (Just "Bonnie Thunders")
                  case mUser of
                    Just user -> Just $ subs' ("userName", textFill user)
                    Nothing   -> Nothing)
```

# Working with attributes

Often you may want to use attributes as "inputs" to your fills. These functions
may be helpful! The type signatures are ommitted because those are *not*
actually helpful, IMO.

## `useAttrs`

Use attributes from the the Blank as arguments to the Fill.

```
<desc length="10" />

("desc", useAttrs (a"length") descriptionFill)

descriptionFill len = textFill $ T.take len
                                  "A really long description"
                                  <> "..."))
```

Result:

`> A really l...`

## `a`

Prepend `a` to the name of an attribute to pass the value of that attribute to
the fill.

The type of the attribute is whatever type the fill expects. If `a` can't parse
the value, then there will be an error when the template is rendered.

## `%`

Use with `a` to use multiple attributes in the fill.

```
<desc length="10" />

("desc", useAttrs (a"length" % a"ending") descriptionFill)

descriptionFill len maybeEnding =
  let ending = fromMaybe "..." maybeEnding in
  textFill $ T.take n
              "A really long description"
              <> ending))
```

Result:

`> A really l...`


