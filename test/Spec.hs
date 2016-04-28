{-# LANGUAGE OverloadedStrings #-}

import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (catMaybes, fromJust, isJust)
import           Data.Monoid        ((<>))
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Larceny
import           Test.Hspec
import qualified Text.XmlHtml       as X

main = spec

page' = "<body>\
         \ <site-title/>\
              \ <people>\
              \   <p><name/></p>\
              \   <site-title/>\
              \ </people>\
              \</body>"

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

render' = runTemplate (parse page') subst

shouldRender :: (Text, Substitution, Library) -> Text -> Expectation
shouldRender (template, subst, lib) output =
  T.replace " " "" (runTemplate (parse template) subst lib) `shouldBe`
  T.replace " " "" output

spec = hspec $ do
  describe "parse" $ do
    it "should parse HTML into a Template" $ do
      (page', subst, mempty) `shouldRender` page''
    it "should allow attributes" $ do
      ("<p id=\"hello\">hello</p>", mempty, mempty) `shouldRender` "<p id=\"hello\">hello</p>"
      ("<img src=\"hello\" alt=\"hello\" />", mempty, mempty) `shouldRender` "<img src=\"hello\" alt=\"hello\" />"
  describe "add" $ do
    it "should allow overriden tags" $ do
      ("<name /><person><name /></person>", subst', mempty) `shouldRender` "My siteDaniel"
  describe "apply" $ do
    it "should allow templates to be included in other templates" $ do
      ("<apply name=\"hello\" />", mempty, M.fromList [("hello", parse "hello")]) `shouldRender` "hello"
    it "should allow templates with unfilled holes to be included in other templates" $ do
      ("<apply name=\"person\" />", sub [("name", text "Daniel")], M.fromList [("person", parse "<name />")]) `shouldRender` "Daniel"
    it "should allow templates to be included in other templates" $ do
      ("<apply name=\"person\">Libby</apply>", mempty, M.fromList [("person", parse "<content />")]) `shouldRender` "Libby"
    it "should allow even more compicated templates to be included in other templates" $ do
      ("<apply name=\"person\"><p>Libby</p></apply>", sub [("food", text "pizza")], M.fromList [("person", parse "<food /><content />")]) `shouldRender` "pizza<p>Libby</p>"
  describe "attributes" $ do
    it "should apply substitutions to attributes as well" $ do
      ("<p id=\"${name}\"><name /></p>", sub [("name", text "McGonagall")], mempty) `shouldRender` "<p id=\"McGonagall\">McGonagall</p>"
  describe "findUnbound" $ do
    it "should find stuff matching the pattern ${blah}" $ do
      findUnbound [X.Element "p" [("blah", "${blah}")] []] `shouldBe` ["blah"]
  describe "findUnboundAttrs" $ do
    it "should find stuff matching the pattern ${blah}" $ do
      findUnboundAttrs [("blah", "${blah}")] `shouldBe` ["blah"]
