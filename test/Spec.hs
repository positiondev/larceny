{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map     as M
import           Data.Monoid  ((<>))
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Larceny
import           Test.Hspec
import qualified Text.XmlHtml as X

main :: IO ()
main = spec

page :: Text
page = "<body>                     \
\          <h1>                    \
\            <name/>               \
\          </h1>                   \
\          <ul>                    \
\            <skaters>             \
\              <li>                \
\                <h2><name/></h2>  \
\                <p><position/></p>\
\              </li>               \
\            </skaters>            \
\          </ul>                   \
\        </body>"

page' :: Text
page' = "<body>                         \
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

subst :: Substitution
subst = sub [ ("site-title", text "Gotham Girls roster")
            , ("name", text "Gotham Girls roster")
            , ("skater", fill $ sub [("name", text "Amy Roundhouse")])
            , ("skaters", mapSub
                          (\(n, p) -> sub [("name", text n)
                                          ,("position", text p)])
                          [ ("Bonnie Thunders", "jammer")
                          , ("Donna Matrix", "blocker")
                          , ("V-Diva", "jammer") ] )]

shouldRender :: (Text, Substitution, Library) -> Text -> Expectation
shouldRender (t', s, l) output =
  T.replace " " "" (runTemplate (parse t') s l) `shouldBe`
  T.replace " " "" output

spec :: IO ()
spec = hspec $ do
  describe "parse" $ do
    it "should parse HTML into a Template" $ do
      (page, subst, mempty) `shouldRender` page'
    it "should allow attributes" $ do
      ("<p id=\"hello\">hello</p>", mempty, mempty) `shouldRender` "<p id=\"hello\">hello</p>"

  describe "add" $ do
    it "should allow overriden tags" $ do
      ("<name /><skater><name /></skater>", subst, mempty) `shouldRender` "Gotham Girls roster Amy Roundhouse"

  describe "apply" $ do
    it "should allow templates to be included in other templates" $ do
      ("<apply name=\"hello\" />",
       mempty,
       M.fromList [("hello", parse "hello")]) `shouldRender` "hello"
    it "should allow templates with unfilled holes to be included in other templates" $ do
      ("<apply name=\"skater\" />",
       sub [("alias", text "Fifi Nomenom")],
       M.fromList [("skater", parse "<alias />")]) `shouldRender` "Fifi Nomenom"
    it "should allow templates to be included in other templates" $ do
      ("<apply name=\"skater\">V-Diva</apply>",
       mempty,
       M.fromList [("skater", parse "<content />")]) `shouldRender` "V-Diva"
    it "should allow compicated templates to be included in other templates" $ do
      ("<apply name=\"_base\"><p>The Smacktivist</p></apply>",
       sub [("siteTitle", text "Ohio Roller Girls")],
       M.fromList [("_base", parse "<h1><siteTitle /></h1>\
                                   \<content />")])
        `shouldRender` "<h1>Ohio Roller Girls</h1>\
                       \<p>The Smacktivist</p>"

  describe "mapHoles" $ do
    it "should map a substitution over a list" $ do
      (page, subst, mempty) `shouldRender` page'

  describe "funky fills" $ do
    it "should allow you to write functions for fills" $ do
      ("<desc length=\"10\" />",
       sub [("desc", \m _t _l -> T.take (read $ T.unpack (m M.! "length"))
                               "A really long description"
                               <> "...")],
        mempty) `shouldRender` "A really l..."

    it "should allow you to *easily* write functions for fills" $ do
      ("<desc length=\"10\" />",
       sub [("desc", funFill (a"length" (\n -> T.take n
                                         "A really long description"
                                         <> "...")))],
        mempty) `shouldRender` "A really l..."

    it "should allow you use multiple args" $ do
      ("<desc length=\"10\" text=\"A really long description\" />",
       sub [("desc", funFill ((a"length" %
                               a"text")
                              (\n d -> T.take n d <> "...")))],
        mempty) `shouldRender` "A really l..."

  describe "attributes" $ do
    it "should apply substitutions to attributes as well" $ do
      ("<p id=\"${skater}\"><skater /></p>",
       sub [("skater", text "Beyonslay")],
       mempty) `shouldRender` "<p id=\"Beyonslay\">Beyonslay</p>"

  describe "findUnbound" $ do
    it "should find stuff matching the pattern ${blah}" $ do
      findUnbound [X.Element "p" [("foo", "${blah}")] []] `shouldBe` ["blah"]

  describe "findUnboundAttrs" $ do
    it "should find stuff matching the pattern ${blah}" $ do
      findUnboundAttrs [("foo", "${blah}")] `shouldBe` ["blah"]

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
