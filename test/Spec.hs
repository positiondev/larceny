{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map     as M
import           Data.Monoid  ((<>))
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Examples
import           Larceny
import           Test.Hspec
import qualified Text.XmlHtml as X

main :: IO ()
main = spec

tpl4Output :: Text
tpl4Output = "\
\        <body>                         \
\          <h1>Gotham Girls roster</h1> \
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

shouldRender :: (Text, BlankFills, Library) -> Text -> Expectation
shouldRender (t', s, l) output = do
  rendered <- runTemplate (parse t') s l
  T.replace " " "" rendered `shouldBe`
    T.replace " " "" output

spec :: IO ()
spec = hspec $ do
  describe "parse" $ do
    it "should parse HTML into a Template" $ do
      (tpl4, subst, mempty) `shouldRender` tpl4Output
    it "should allow attributes" $ do
      ("<p id=\"hello\">hello</p>", mempty, mempty) `shouldRender` "<p id=\"hello\">hello</p>"

  describe "add" $ do
    it "should allow overriden tags" $ do
      ("<name /><skater><name /></skater>", subst, mempty) `shouldRender` "Gotham Girls roster Amy Roundhouse"

  describe "apply" $ do
    it "should allow templates to be included in other templates" $ do
      ("<apply template=\"hello\" />",
       mempty,
       M.fromList [("hello", parse "hello")]) `shouldRender` "hello"
    it "should allow templates with unfilled holes to be included in other templates" $ do
      ("<apply template=\"skater\" />",
       fills [("alias", text "Fifi Nomenom")],
       M.fromList [("skater", parse "<alias />")]) `shouldRender` "Fifi Nomenom"
    it "should allow templates to be included in other templates" $ do
      ("<apply template=\"skater\">V-Diva</apply>",
       mempty,
       M.fromList [("skater", parse "<content />")]) `shouldRender` "V-Diva"
    it "should allow compicated templates to be included in other templates" $ do
      ("<apply template=\"_base\"><p>The Smacktivist</p></apply>",
       fills [("siteTitle", text "Ohio Roller Girls")],
       M.fromList [("_base", parse "<h1><siteTitle /></h1>\
                                   \<content />")])
        `shouldRender` "<h1>Ohio Roller Girls</h1>\
                       \<p>The Smacktivist</p>"

  describe "mapFills" $ do
    it "should map the fills over a list" $ do
      (tpl4, subst, mempty) `shouldRender` tpl4Output

  describe "writing functions" $ do
    it "should allow you to write functions for fills" $ do
      ("<desc length=\"10\" />",
       fills [("desc", \m _t _l -> return $ T.take (read $ T.unpack (m M.! "length"))
                               "A really long description"
                               <> "...")],
        mempty) `shouldRender` "A really l..."


    it "should allow you to use IO in fills" $ do
      ("<desc length=\"10\" />",
       fills [("desc", \m _t _l -> do putStrLn "***********\nHello World\n***********"
                                      return $ T.take (read $ T.unpack (m M.! "length"))
                                               "A really long description"
                                               <> "...")],
        mempty) `shouldRender` "A really l..."

  describe "useAttrs" $ do
    it "should allow you to *easily* write functions for fills" $ do
      ("<desc length=\"10\" />",
       fills [("desc", useAttrs (a"length" (\n _t -> return $ T.take n
                                            "A really long description"
                                            <> "...")))],
        mempty) `shouldRender` "A really l..."

    it "should allow you use multiple args" $ do
      ("<desc length=\"10\" text=\"A really long description\" />",
       fills [("desc", useAttrs ((a"length" %
                                  a"text")
                                 (\n d _t -> return $ T.take n d <> "...")))],
        mempty) `shouldRender` "A really l..."

    it "should allow you use child elements" $ do
      ("<desc length=\"10\">A <adverb /> long description</desc>",
       fills [ ("adverb", text "really")
             , ("desc", useAttrs ((a"length")
                                  (\n t -> return $ T.take n t <> "...")))],
        mempty) `shouldRender` "A really l..."

  describe "attributes" $ do
    it "should apply substitutions to attributes as well" $ do
      ("<p id=\"${skater}\"><skater /></p>",
       fills [("skater", text "Beyonslay")],
       mempty) `shouldRender` "<p id=\"Beyonslay\">Beyonslay</p>"

  describe "findUnbound" $ do
    it "should find stuff matching the pattern ${blah}" $ do
      findUnbound [X.Element "p" [("foo", "${blah}")] []] `shouldBe` ["blah"]

  describe "findUnboundAttrs" $ do
    it "should find stuff matching the pattern ${blah}" $ do
      findUnboundAttrs [("foo", "${blah}")] `shouldBe` ["blah"]

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
