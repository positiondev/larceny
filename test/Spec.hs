{-# LANGUAGE OverloadedStrings #-}

import           Control.DeepSeq     (force)
import           Control.Exception   (evaluate)
import           Control.Monad.State (evalStateT, get, modify)
import           Control.Monad.Trans (liftIO)
import qualified Data.Map            as M
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Examples
import           Larceny
import           Test.Hspec

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
\            </li>                      \
\            <li>                       \
\              <h2>Donna Matrix</h2>    \
\              <p>blocker</p>           \
\            </li>                      \
\            <li>                       \
\              <h2>V-Diva</h2>          \
\              <p>jammer</p>            \
\            </li>                      \
\          </ul>                        \
\        </body>"

shouldRender :: ([Text], Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRender (pth, t', s, l) output = do
  rendered <- evalStateT (runTemplate (parse (LT.fromStrict t')) pth s l) ()
  T.replace " " "" rendered `shouldBe`
    T.replace " " "" output

shouldRenderDef :: (Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRenderDef (t', s, l) output = do
    rendered <- evalStateT (runTemplate (parse (LT.fromStrict t')) ["default"] s l) ()
    T.replace " " "" rendered `shouldBe`
      T.replace " " "" output

shouldRenderContaining :: ([Text], Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRenderContaining (pth, t, s, l) excerpt = do
  rendered <- evalStateT (runTemplate (parse (LT.fromStrict t)) pth s l) ()
  (excerpt `T.isInfixOf` rendered) `shouldBe` True

shouldErrorDef :: (Text, Substitutions (), Library ()) -> String -> Expectation
shouldErrorDef (t', s, l) output = do
    renderAttempt <- evalStateT (runTemplate (parse (LT.fromStrict t')) ["default"] s l) ()
    (evaluate . force) renderAttempt `shouldThrow` (errorCall output)

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
       fills [("alias", text "Fifi Nomenom")],
       M.fromList [(["skater"], parse "<alias />")]) `shouldRenderDef` "Fifi Nomenom"
    it "should allow templates to be included in other templates" $ do
      ("<apply template=\"skater\">V-Diva</apply>",
       mempty,
       M.fromList [(["skater"], parse "<apply-content />")]) `shouldRenderDef` "V-Diva"
    it "should allow compicated templates to be included in other templates" $ do
      ("<apply template=\"_base\"><p>The Smacktivist</p></apply>",
       fills [("siteTitle", text "Ohio Roller Girls")],
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
        fills [("adjective", text "awesome")],
        mempty) `shouldRenderDef` "Roller derby is awesome"

  describe "mapFills" $ do
    it "should map the fills over a list" $ do
      (tpl4, subst, mempty) `shouldRenderDef` tpl4Output

  describe "writing functions" $ do
    it "should allow you to write functions for fills" $ do
      ("<desc length=\"10\" />",
       fills [("desc", \m _t _l -> return $ T.take (read $ T.unpack (m M.! "length"))
                               "A really long description"
                               <> "...")],
        mempty) `shouldRenderDef` "A really l..."


    it "should allow you to use IO in fills" $ do
      ("<desc length=\"10\" />",
       fills [("desc", \m _t _l -> do liftIO $ putStrLn "***********\nHello World\n***********"
                                      return $ T.take (read $ T.unpack (m M.! "length"))
                                               "A really long description"
                                               <> "...")],
        mempty) `shouldRenderDef` "A really l..."

  describe "useAttrs" $ do
    it "should allow you to *easily* write functions for fills" $ do
      ("<desc length=\"10\" />",
       fills [("desc", useAttrs (a"length" (\n _t -> return $ T.take n
                                            "A really long description"
                                            <> "...")))],
        mempty) `shouldRenderDef` "A really l..."

    it "should allow you use multiple args" $ do
      ("<desc length=\"10\" text=\"A really long description\" />",
       fills [("desc", useAttrs ((a"length" %
                                  a"text")
                                 (\n d _t -> return $ T.take n d <> "...")))],
        mempty) `shouldRenderDef` "A really l..."

    it "should allow you use child elements" $ do
      ("<desc length=\"10\">A <adverb /> long description</desc>",
       fills [ ("adverb", text "really")
             , ("desc", useAttrs ((a"length")
                                  (\n t -> return $ T.take n t <> "...")))],
        mempty) `shouldRenderDef` "A really l..."

    it "should allow optional attributes by giving a Maybe type" $ do
      let descFill n e t =
            let ending = maybe "..." id e in
            return $ T.take n t <> ending
      ("<desc length=\"10\">A really long description</desc>",
       fills [("desc", useAttrs $ (a"length" % a"ending") descFill)],
       mempty) `shouldRenderDef` "A really l..."

    it "should give a nice error message if attribute is missing" $ do
      ("<desc />",
       fills [("desc", useAttrs (a"length" (\n _t -> return $ T.take n
                                            "A really long description"
                                            <> "...")))],
        mempty) `shouldErrorDef` "Attribute error: Unable to find attribute \"length\"."

    it "should give a nice error message if attribute is unparsable" $ do
      ("<desc length=\"infinite\" />",
       fills [("desc", useAttrs (a"length" (\n _t -> return $ T.take n
                                            "A really long description"
                                            <> "...")))],
        mempty) `shouldErrorDef` "Attribute error: Unable to parse attribute \"length\" as type Int."

  describe "attributes" $ do
    it "should apply substitutions to attributes as well" $ do
      ("<p id=\"${skater}\"><skater /></p>",
       fills [("skater", text "Beyonslay")],
       mempty) `shouldRenderDef` "<p id=\"Beyonslay\">Beyonslay</p>"

  describe "a large HTML file" $ do
    it "should render large HTML files" $ do
      (["default"], tpl6, subst, positionTplLib) `shouldRenderContaining` "Verso Books"

  describe "statefulness" $ do
    it "a fill should be able to affect subsequent fills" $ do
       renderWith (M.fromList [(["default"], parse "<x/><x/>")])
                  (fills [("x", \_ _ _ -> do modify ((+1) :: Int -> Int)
                                             s <- get
                                             return (T.pack (show s)))])
                  0
                  ["default"]
       `shouldReturn` Just "12"

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
