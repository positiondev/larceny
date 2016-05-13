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

page' :: Text
page' = "<body>\
\          <site-title/>\
\          <people>\
\            <p><name/></p>\
\            <site-title/>\
\          </people>\
\        </body>"

page'' :: Text
page'' = "<body>\
              \ My site\
              \   <p>Daniel</p>\
              \   My site\
              \   <p>Matt</p>\
              \   My site\
              \   <p>Cassie</p>\
              \   My site\
              \   <p>Libby</p>\
              \   My site\
              \</body>"

subst :: Substitution
subst = sub [ ("site-title", text "My site")
             , ("name", text "My site")
             , ("person", fill $ sub [("name", text "Daniel")])
             , ("people", mapSub (\n -> sub $ [("name", text n)])
                          ["Daniel", "Matt", "Cassie", "Libby"]) ]

shouldRender :: (Text, Substitution, Library) -> Text -> Expectation
shouldRender (t', s, l) output =
  T.replace " " "" (runTemplate (parse t') s l) `shouldBe`
  T.replace " " "" output

spec :: IO ()
spec = hspec $ do
  describe "parse" $ do
    it "should parse HTML into a Template" $ do
      (page', subst, mempty) `shouldRender` page''
    it "should allow attributes" $ do
      ("<p id=\"hello\">hello</p>", mempty, mempty) `shouldRender` "<p id=\"hello\">hello</p>"

  describe "add" $ do
    it "should allow overriden tags" $ do
      ("<name /><person><name /></person>", subst, mempty) `shouldRender` "My siteDaniel"

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
      ("<apply name=\"person\">Libby</apply>",
       mempty,
       M.fromList [("person", parse "<content />")]) `shouldRender` "Libby"
    it "should allow compicated templates to be included in other templates" $ do
      ("<apply name=\"person\"><p>Libby</p></apply>",
       sub [("food", text "pizza")],
       M.fromList [("person", parse "<food /><content />")])
        `shouldRender` "pizza<p>Libby</p>"

  describe "mapHoles" $ do
    it "should map a substitution over a list" $ do
      (page', subst, mempty) `shouldRender` page''

  describe "funky fills" $ do
    it "should allow you to write functions for fills" $ do
      ("<desc length=\"10\" />",
       sub [("desc", \m _t _l -> T.take (read $ T.unpack (m M.! "length") :: Int)
                               "A really long description"
                               <> "...")],
        mempty) `shouldRender` "A really l..."

    it "should allow you to *easily* write functions for fills" $ do
      ("<desc length=\"10\" />",
       sub [("desc", funFill (i"length" (\n -> T.take n
                                               "A really long description"
                                               <> "...")))],
        mempty) `shouldRender` "A really l..."

    it "should allow you use multiple args" $ do
      ("<desc length=\"10\" text=\"A really long description\" />",
       sub [("desc", funFill ((i"length" %
                               t"text")
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
