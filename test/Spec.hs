{-# LANGUAGE OverloadedStrings #-}

import           Control.DeepSeq     (force)
import           Control.Exception   (Exception, evaluate)
import           Control.Monad.State (evalStateT, get, modify)
import           Control.Monad.Trans (liftIO)
import qualified Data.Map            as M
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Examples
import           Test.Hspec
import           Web.Larceny

main :: IO ()
main = spec

tpl4Output :: Text
tpl4Output = "\
\        <body>                         \
\          <h1>                         \
\            Gotham Girls Roller Derby  \
\          </h1>                        \
\          <ul>                         \
\            <li>                       \
\              <h2>Bonnie Thunders</h2> \
\              <p>jammer</p>            \
\              <p>A really long...</p>  \
\            </li>                      \
\            <li>                       \
\              <h2>Donna Matrix</h2>    \
\              <p>blocker</p>           \
\              <p>A really long...</p>  \
\            </li>                      \
\            <li>                       \
\              <h2>V-Diva</h2>          \
\              <p>jammer</p>            \
\              <p>A really long...</p>  \
\            </li>                      \
\          </ul>                        \
\        </body>"

removeWhitespace :: Text -> Text
removeWhitespace = T.replace " " ""

shouldRender :: ([Text], Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRender (pth, t', s, l) output = do
  rendered <- evalStateT (runTemplate (parse (LT.fromStrict t')) pth s l) ()
  removeWhitespace rendered `shouldBe` removeWhitespace output

shouldRenderDef :: (Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRenderDef (t', s, l) output = do
    rendered <- evalStateT (runTemplate (parse (LT.fromStrict t')) ["default"] s l) ()
    removeWhitespace rendered `shouldBe` removeWhitespace output

shouldRenderCustom :: (Text, Substitutions (), Library (), Overrides) -> Text -> Expectation
shouldRenderCustom (t', s, l, o) output = do
    rendered <- evalStateT (runTemplate (parseWithOverrides o (LT.fromStrict t')) ["default"] s l) ()
    removeWhitespace rendered `shouldBe` removeWhitespace output

shouldRenderContaining :: ([Text], Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRenderContaining (pth, t, s, l) excerpt = do
  rendered <- evalStateT (runTemplate (parse (LT.fromStrict t)) pth s l) ()
  (excerpt `T.isInfixOf` rendered) `shouldBe` True

shouldErrorPath :: (Exception a, Eq a) =>(Path, Text, Substitutions (), Library ()) -> a -> Expectation
shouldErrorPath (pth, t', s, l) e =
    (do renderAttempt <- evalStateT (runTemplate (parse (LT.fromStrict t')) pth s l) ()
        (evaluate . force) renderAttempt) `shouldThrow` (== e)

shouldErrorDef :: (Exception a, Eq a) =>(Text, Substitutions (), Library ()) -> a -> Expectation
shouldErrorDef (t', s, l) e = do
  (do renderAttempt <- evalStateT (runTemplate (parse (LT.fromStrict t')) ["default"] s l) ()
      (evaluate . force) renderAttempt) `shouldThrow` (== e)

spec :: IO ()
spec = hspec $ do
  describe "parse" $ do
    it "should parse HTML into a Template" $ do
      (tpl4, subst, mempty) `shouldRenderDef` tpl4Output
    it "should allow attributes" $ do
      ("<p id=\"hello\">hello</p>", mempty, mempty) `shouldRenderDef` "<p id=\"hello\">hello</p>"

  describe "add" $ do
    it "should allow overriden tags" $ do
      ("<name /><skater><name /></skater>", subst, mempty) `shouldRenderDef` "Gotham Girls Amy Roundhouse"
  describe "apply" $ do
    it "should allow templates to be included in other templates" $ do
      ("<apply template=\"hello\" />",
       mempty,
       M.fromList [(["hello"], parse "hello")]) `shouldRenderDef` "hello"
    it "should allow templates with unfilled holes to be included in other templates" $ do
      ("<apply template=\"skater\" />",
       subs [("alias", textFill "Fifi Nomenom")],
       M.fromList [(["skater"], parse "<alias />")]) `shouldRenderDef` "Fifi Nomenom"
    it "should allow templates to be included in other templates" $ do
      ("<apply template=\"skater\">V-Diva</apply>",
       mempty,
       M.fromList [(["skater"], parse "<apply-content />")]) `shouldRenderDef` "V-Diva"
    it "should allow compicated templates to be included in other templates" $ do
      ("<apply template=\"_base\"><p>The Smacktivist</p></apply>",
       subs [("siteTitle", textFill "Ohio Roller Girls")],
       M.fromList [(["_base"], parse "<h1><siteTitle /></h1>\
                                            \<apply-content />")])
        `shouldRenderDef` "<h1>Ohio Roller Girls</h1>\
                       \<p>The Smacktivist</p>"
    it "should look higher in tree for matching template" $
       (["foo","bar"], "<apply template=\"base\" />",
        mempty,
        M.fromList [(["base"], parse "hello")]) `shouldRender` "hello"
    it "should look first look in same directory for matching template" $
       (["foo","bar"], "<apply template=\"base\" />",
        mempty,
        M.fromList [(["base"], parse "hello")
                   ,(["foo", "base"], parse "goodbye")]) `shouldRender` "goodbye"
    it "should traverse down via slashes" $
       (["default"], "<apply template=\"foo/base\" />",
        mempty,
        M.fromList [(["base"], parse "hello")
                   ,(["foo", "base"], parse "goodbye")]) `shouldRender` "goodbye"

    it "should only truncate parts from current path, not specified template path" $
       (["foo"], "<apply template=\"bar/baz\" />",
        mempty,
        M.fromList [(["baz"], parse "hello")]) `shouldErrorPath` ApplyError ["bar","baz"] ["foo"]

    it "should use the path to the applied template when looking" $
         (["default"], "<apply template=\"foo/bar/baz\" />",
          mempty,
          M.fromList [(["base"], parse "hello")
                     ,(["foo", "bar", "base"], parse "goodbye")
                     ,(["foo", "bar", "baz"], parse "<apply template=\"base\"/>")]) `shouldRender` "goodbye"
    it "should use the path to the applied template when looking" $
         (["default", "hello"], "<apply template=\"foo/bar/baz\"><apply template=\"x\"/></apply>",
          mempty,
          M.fromList [(["default", "x"], parse "hello")
                     ,(["foo", "bar", "baz"], parse "<apply-content/>")]) `shouldRender` "hello"

  describe "overriding HTML tags" $ do
    it "should allow overriden Html tags" $ do
      ("<html><div></div></html>",
       subs [("div", textFill "notadivatall")],
       mempty,
       Overrides mempty ["div"]) `shouldRenderCustom` "<html>not a div at all</html>"
    it "should allow (nested) overriden Html tags" $ do
      ("<html><custom><div></div></custom></html>",
       subs [("div", textFill "notadivatall")
            ,("custom", fillChildrenWith mempty)],
       mempty,
       Overrides mempty ["div"]) `shouldRenderCustom` "<html>not a div at all</html>"
    it "should not need fills for manually added plain nodes" $ do
      ("<html><blink>retro!!</blink></html>",
       mempty,
       mempty,
       Overrides ["blink"] mempty) `shouldRenderCustom` "<html><blink>retro!!</blink></html>"

  describe "bind" $ do
    it "should let you bind tags to fills within templates" $ do
      ("<bind tag=\"sport\">Roller derby</bind><sport />",
       mempty,
       mempty) `shouldRenderDef` "Roller derby"
    it "should let you use binds within binds" $ do
      ("<bind tag=\"sport\"><bind tag=\"adjective\">awesome</bind>Roller derby is <adjective /></bind><sport />",
        mempty,
        mempty) `shouldRenderDef` "Roller derby is awesome"
    it "should let you bind with nested blanks" $ do
      ("<bind tag=\"sport\">Roller derby is <adjective /></bind><sport />",
        subs [("adjective", textFill "awesome")],
        mempty) `shouldRenderDef` "Roller derby is awesome"

    it "should apply binds to applied templates" $ do
      ("<bind tag=\"foo\">         \
       \  Fill this in             \
       \</bind>                    \
       \<apply template=\"blah\">  \
       \  <foo />                  \
       \</apply>",
       mempty,
       M.fromList [(["blah"], parse "<apply-content /><foo />")])
        `shouldRenderDef` "Fill this inFill this in"

    it "should not let binds escape the apply-content tag" $ do
      ("<apply template=\"blah\"> \
       \  <bind tag=\"foo\">      \
       \    Fill this in          \
       \  </bind>                 \
       \  <foo />                 \
       \</apply>",
       mempty,
       M.fromList [(["blah"], parse "<apply-content /><foo />")])
         `shouldErrorDef` MissingBlanks [Blank "foo"] ["blah"]

  describe "mapSubs" $ do
    it "should map the subs over a list" $ do
      (tpl4, subst, mempty) `shouldRenderDef` tpl4Output
    it "should map the subs over a list" $ do
      (tpl4, subst, mempty) `shouldRenderDef` tpl4Output

  describe "writing functions" $ do
    it "should allow you to write functions for fills" $ do
      ("<desc length=\"10\" />",
       subs [("desc", Fill $ \m _t _l -> return $ T.take (read $ T.unpack (m M.! "length"))
                                         "A really long description"
                                         <> "...")],
        mempty) `shouldRenderDef` "A really l..."


    it "should allow you to use IO in fills" $ do
      ("<desc length=\"10\" />",
       subs [("desc", Fill $
                        \m _t _l -> do liftIO $ putStrLn "***********\nHello World\n***********"
                                       return $ T.take (read $ T.unpack (m M.! "length"))
                                         "A really long description"
                                         <> "...")],
        mempty) `shouldRenderDef` "A really l..."

  describe "useAttrs" $ do
    it "should allow you to *easily* write functions for fills" $ do
      ("<desc length=\"10\" />",
       subs [("desc", useAttrs (a"length")
                               (\n -> textFill $ T.take n
                                      "A really long description"
                                      <> "..."))],
        mempty) `shouldRenderDef` "A really l..."

    it "should allow you use multiple args" $ do
      ("<desc length=\"10\" text=\"A really long description\" />",
       subs [("desc", useAttrs (a"length" % a"text")
                               (\n d -> textFill $ T.take n d <> "..."))],
        mempty) `shouldRenderDef` "A really l..."

    it "should allow you use child elements" $ do
      let descTplFill =
            useAttrs (a"length")
                     (\n -> Fill $ \_attrs (_pth, tpl) _l -> liftIO $ do
                         t' <- evalStateT (runTemplate tpl ["default"] mempty mempty) ()
                         return $ T.take n t' <> "...")
      ("<desc length=\"10\">A <adverb /> long description</desc>",
       subs [ ("adverb", textFill "really")
             , ("desc", descTplFill)],
        mempty) `shouldRenderDef` "A really l..."

    it "should allow optional attributes by giving a Maybe type (not using the optional)" $ do
      ("<desc length=\"10\">A really long description</desc>",
       subs [("desc", descFill)],
       mempty) `shouldRenderDef` "A really l..."

    it "should allow optional attributes by giving a Maybe type (using optional)" $ do
      ("<desc length=\"10\" ending=\" and such \">A really long description</desc>",
       subs [("desc", descFill)],
       mempty) `shouldRenderDef` "A really l and such"

    it "should give a nice error message if attribute is missing" $ do
      ("<desc />",
       subs [("desc", useAttrs (a"length")
                               (\n -> textFill $ T.take n
                                      "A really long description"
                                      <> "..."))],
        mempty) `shouldErrorDef` AttrMissing "length"

    it "should give a nice error message if attribute is unparsable" $ do
      ("<desc length=\"infinite\" />",
       subs [("desc", useAttrs (a"length")
                               (\n -> textFill $ T.take n
                                      "A really long description"
                                      <> "..."))],
        mempty) `shouldErrorDef` AttrUnparsable "Int" "length"

  describe "attributes" $ do
    it "should apply substitutions to attributes as well" $ do
      ("<p id=\"${skater}\"><skater /></p>",
       subs [("skater", textFill "Beyonslay")],
       mempty) `shouldRenderDef` "<p id=\"Beyonslay\">Beyonslay</p>"

    it "should apply substitutions to attributes inside of blanks" $ do
      ("<skater name=\"${name}\"><skater />",
       subs [("skater", useAttrs (a"name") (\name -> textFill $ "Skater: " <> name))
            ,("name", textFill "Beyonslay")],
       mempty) `shouldRenderDef` "Skater: Beyonslay"

    it "should substitute blanks that are only part of attributes" $ do
      ("<skater name=\"the great ${name}\"><skater />",
       subs [("skater", useAttrs (a"name") (\name -> textFill $ "Skater: " <> name))
            ,("name", textFill "Beyonslay")],
       mempty) `shouldRenderDef` "Skater: the great Beyonslay"

    it "should substitute multiple blanks in an attribute" $ do
      ("<skater name=\"the ${adj} ${name}\"><skater />",
       subs [("skater", useAttrs (a"name") (\name -> textFill $ "Skater: " <> name))
            ,("name", textFill "Beyonslay")
            ,("adj", textFill "great")],
       mempty) `shouldRenderDef` "Skater: the great Beyonslay"

    it "should keep special characters in attribute" $ do
      ("<a href=\"/s/${token}/${magazine}-${number}.pdf\">Issue 5</a>",
       subs [("token", textFill "123")
            ,("magazine", textFill "BloodAndThunder")
            ,("number", textFill "5")],
       mempty) `shouldRenderDef` "<a href=\"/s/123/BloodAndThunder-5.pdf\">Issue 5</a>"

  describe "a large HTML file" $ do
    it "should render large HTML files" $ do
      (["default"], tpl6, subst, positionTplLib) `shouldRenderContaining` "Verso Books"

  describe "statefulness" $ do
    it "a fill should be able to affect subsequent fills" $ do
       renderWith (M.fromList [(["default"], parse "<x/><x/>")])
                  (subs [("x", Fill $ \_ _ _ ->
                                 do modify ((+1) :: Int -> Int)
                                    s <- get
                                    return (T.pack (show s)))])
                  0
                  ["default"]
       `shouldReturn` Just "12"

  describe "escaping" $ do
    it "should not escape html" $ do
      ("<p><someHtml /></p>",
       subs [("someHtml", textFill "<strong>Some HTML</strong>")],
       mempty) `shouldRenderDef`
       "<p><strong>Some HTML</strong></p>"

descFill :: Fill ()
descFill =
  useAttrs (a"length" % a"ending") descFunc

descFunc :: Int -> Maybe Text -> Fill ()
descFunc n e = Fill $
  do
    let ending = maybe "..." id e
    \_attrs (_pth, tpl) _l -> liftIO $ do
      renderedText <- evalStateT (runTemplate tpl ["default"] mempty mempty) ()
      return $ T.take n renderedText <> ending

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
