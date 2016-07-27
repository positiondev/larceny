# Larceny

Larceny is Haskell templating based on [Heist](heist).

With Larceny, you write templates that look like this:

```
<bind tag=\"sport\">
  Roller Derby
</bind>
<h1>
  <name/> <sport/>
</h1>
<ul>
  <skaters>
    <li>
      <h2><name/></h2>
      <p><position/></p>
      <p><bio length="30" text="${longBio}"/></p>
      <p><a href="skater/${id}/">Read more!</a></p>
    </li>
  </skaters>
</ul>
```

And "substitutions" that look like this:

```
teamPageSubs :: Substitutions ()
teamPageSubs =
  subs [ ("name", text "Gotham Girls")
       , ("skaters", mapSubs
                     (\(i,n,p,b) ->
                       subs [ ("id", text i)
                            , ("name", text n)
                            , ("position", text p)
                            , ("longBio", text b)])
                    [ ("1", "Bonnie Thunders", "jammer", longBio)
                    , ("2", "Donna Matrix", "blocker", longBio)
                    , ("3", "V-Diva", "jammer", longBio) ] )
       , ("bio", useAttrs (a"length" %
                            a"text")
                           shortenerFill))
  where longBio = "Some example bio that is really long!"
        shortenerFill maybeNumber fullBio = textFill $
          case maybeNumber of
            Just numChars -> T.take numChars fullBio <> "..."
            Nothing -> fullBio
```

You end up with HTML like this:

```
<h1>
  Gotham Girls Roller Derby
</h1>
<ul>
  <li>
    <h2>Bonnie Thunders</h2>
    <p>jammer</p>
    <p>Some example bio that is rea...</p>
    <p><a href="skaters/1/">Read more!</a></p>
  </li>
  <li>
    <h2>Donna Matrix</h2>
    <p>blocker</p>
    <p>Some example bio that is rea...</p>
    <p><a href="skaters/2/">Read more!</a></p>
  </li>
  <li>
    <h2>V-Diva</h2>
    <p>jammer</p>
    <p>Some example bio that is rea...</p>
    <p><a href="skaters/3/">Read more!</a></p>
  </li>
```

## Why another templating language? Why not Heist?

Position Dev loves Heist templates!

But then we needed unescaped HTML in our templates... so we had to
use compiled Heist. Compiled Heist is really hard to undestand.

We wrote Larceny as an alternative to compiled Heist that is easier to
understand and use (if slower).

## Differences from Heist

Larceny is different from Interpreted Heist (but similar to Compiled
Heist) in that it doesn't escape any text.

In Heist, `<bind>`s inside of nested template application can be used
in the outer templates. We found that confusing, so Larceny doesn't
allow that.
