# Writing Fills from Scratch

Fills are how you fill-in the Blanks in your [templates](templates).

There are lots of helper functions for writing fills, listed
[here](here). You should be able to do almost everything you want with
these helper functions! But if you want to do something special, you
can write fills "from scratch". It's a little more complicated, so if
you have questions don't hesitate to open an issue and ask, or email
larceny@positiondev.com.

## The type of a Fill

A Fill is *how* you fill in the Blank, so it's a function.

```
newtype Fill s = Fill { unFill :: Attributes
                               -> (Path, Template s)
                               -> Library s
                               -> StateT s IO Text }
```

The function has 3 arguments:
1. `Attributes` -- these are the attributes of the Blank as an HTML
tag.
2. `(Path, Template s)` -- the child elements of the Blank and
the template's path.
3. `Library s` -- The template library

And given these ingredients, the function will give you `StateT s IO
Text`. This complicated type is so that you can store and change state
(like what's been rendered so far in the template) and you can do side
effects (like read from a database). You can think of `StateT s IO
Text` as combination of state, text, and side effects.

When you use `evalStateT` from [Control.Monad.State](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html) on a Fill given
all its ingredients, and give it the current state, it will return
some `IO Text`.

Example:

```
import Control.Monad.State (evalStateT)

type MyState = [Text]



initialState :: [Text]
initialState = ["hello"]

someFill :: Fill MyState
someFill = Fill myFill
  where myFill attributes (path, template) library =
          return "some text"

fillToText :: Fill MyState -> StateT MyState IO Text
fillToText someFill = unFill someFill attr (pth, template) library
```

### Attributes

Attributes are a [Map](https://hackage.haskell.org/package/containers-0.5.10.2) from attribute names to attribute
values.

If you had a Blank like this: `<description length="10" />`, its
Attributes would be something like this: `fromList [("length", "10")]`.

### (Path, Template s)

This is a tuple of a Path and a Template. The Path is the path to the
Template that's being rendered right now. But the Template is not that
whole template. Instead, it's just the bit that is *inside* of the
Blank.

Here's an example. This is a template called "hello.tpl" inside of a
directory called "pages".

(pages/hello.tpl)
```
<greeting>
  Hello!
</greeting>
```

If you were writing a Fill for this `<greeting>` Blank, it would have
the Path `["pages", "hello"]`.

The `Template s` is the unrendered stuff *inside* of
`<greeting>`. It's not just text, because it might have Blanks inside
of it that you want to get rendered.

#### Example

Let's say that we want to make a Fill that reverses all the text
within a Blank.  This isn't necessarily a great idea but it's
possible.

We want this:

```
<reverse>Reverse this text!</reverse>
```

to render like this:

```
!txet siht esrseveR
```

What does that look like?

```
reverseFill :: Fill s
reverseFill _attrs (pth, tpl) lib =
  state <- get
  renderedText <- evalStateT (runTemplate pth mempty lib) () state
  return $ reverse text
```

We use `get`(link) to get the current state. Then we run the
template and use `evalStateT` to get the text. Then, return
the reversed text.

The reason why this isn't a great idea is that `text` could include
HTML tags! So this:

```
<reverse>Reverse <em>this</em> text!</reverse>
```

will result in this:

```
!txet <me/>siht<me> esreveR
```

Which is not valid HTML. :)

If you wanted to avoid that, you could parse `text`'s HTML with
Blaze or something, then run `reverse` over the text nodes of
the HTML tree. I'll leave that as an exercise for the reader!

### Library s

This is the collection of templates that have been loaded ahead of time. You
might use this to dynamically include other templates in a Fill.
