{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE TypeSynonymInstances #-}


import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (Exception, throw, try)
import           Lens.Micro
import           Control.Monad.State     (StateT (..), evalStateT, get, modify,
                                          runStateT)
import qualified Control.Monad.State     as S
import           Control.Monad.Trans     (liftIO)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import           Data.Typeable
import           Examples
import           Test.Hspec
import qualified Test.Hspec.Core.Spec    as H
import           Web.Larceny

infix  4 .=
(.=) :: S.MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

data LarcenyState =
  LarcenyState { _lPath      :: [Text]
               , _lSubs      :: Substitutions ()
               , _lLib       :: Library ()
               , _lOverrides :: Overrides }

lPath :: Lens' LarcenyState [Text]
lPath = lens _lPath (\ls p -> ls { _lPath = p })
lSubs :: Lens' LarcenyState (Substitutions ())
lSubs = lens _lSubs (\ls s -> ls { _lSubs = s })
lLib :: Lens' LarcenyState (Library ())
lLib = lens _lLib (\ls l -> ls { _lLib = l })
lOverrides :: Lens' LarcenyState Overrides
lOverrides = lens _lOverrides (\ls o -> ls { _lOverrides = o })

type LarcenyHspecM = StateT LarcenyHspecState IO

data LarcenyHspecState =
  LarcenyHspecState { _hResult       :: H.Result
                    , _hLarcenyState :: LarcenyState }

hResult :: Lens' LarcenyHspecState H.Result
hResult = lens _hResult (\hs r -> hs { _hResult = r })
hLarcenyState :: Lens' LarcenyHspecState LarcenyState
hLarcenyState = lens _hLarcenyState (\hs ls -> hs { _hLarcenyState = ls })

instance H.Example (LarcenyHspecM ()) where
  type Arg (LarcenyHspecM ()) = LarcenyHspecState
  evaluateExample s _params actionWithToIO _progCallback =
    do mv <- newEmptyMVar
       actionWithToIO $ \st ->
         do r <- do ((), larcenyHspecState) <- runStateT s st
                    return (larcenyHspecState ^. hResult)
            putMVar mv r
       takeMVar mv

withLarceny :: SpecWith LarcenyHspecState
            -> Spec
withLarceny spec' =
  let larcenyHspecState =
        LarcenyHspecState (H.Result "" H.Success) (LarcenyState ["default"] mempty mempty mempty) in
  afterAll return $
    before (return larcenyHspecState) spec'

setResult :: H.ResultStatus -> LarcenyHspecM ()
setResult r = case r of
                H.Success -> hResult .= H.Result "" r
                _ -> throw r

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

newtype SomeError = SomeError Text deriving (Eq, Show)

instance Exception SomeError

removeSpaces :: Text -> Text
removeSpaces = T.replace " " ""

renderM :: Text -> LarcenyHspecM Text
renderM templateText = do
  (LarcenyHspecState _ (LarcenyState p s l o)) <- S.get
  let tpl = parseWithOverrides o (LT.fromStrict templateText)
  (a, s) <- liftIO $ runTemplate tpl p s l ()
  return a

shouldRenderM :: Text -> Text -> LarcenyHspecM ()
shouldRenderM templateText output = do
  rendered <- renderM templateText
  if removeSpaces rendered == removeSpaces output
    then setResult H.Success
    else let msg = T.unpack $ rendered <> " doesn't match " <> output in
         setResult (H.Failure Nothing (H.Reason msg))

shouldRenderContainingM :: Text -> Text -> LarcenyHspecM ()
shouldRenderContainingM templateText excerpt = do
  rendered <- renderM templateText
  if excerpt `T.isInfixOf` rendered
  then setResult H.Success
  else let msg = T.unpack $ excerpt <> " not found in " <> templateText in
       setResult (H.Failure Nothing (H.Reason msg))

shouldErrorM :: (Exception a, Eq a) => Text -> Selector a -> LarcenyHspecM ()
shouldErrorM templateText p =
   do hspecState <- S.get
      let renderAttempt = evalStateT (renderM templateText) hspecState
      result <- liftIO $ do
        let forceRenderAttempt = do !result <- renderAttempt
                                    return result
        r <- try forceRenderAttempt
        case r of
          Right _ ->
               return $ H.Failure Nothing $
                 H.Reason ("rendered successfully instead of throwing expected exception: " <>
                 exceptionType)
          Left e ->
            if p e then return H.Success
                   else return $ H.Failure Nothing $ H.Reason ("did not get expected exception: " <>
                        exceptionType <> ", got this exeption instead: " <> show e)
      setResult result
  where exceptionType = (show . typeOf . instanceOf) p
        instanceOf :: Selector a -> a
        instanceOf _ = error "Test.Hspec.Expectations.shouldThrow: broken Typeable instance"

main :: IO ()
main = spec

spec :: IO ()
spec = hspec $ do
  withLarceny $ do
    describe "parse" $ do
      it "should parse HTML into a Template" $ do
        hLarcenyState.lSubs .= subst
        hLarcenyState.lLib .= mempty
        tpl4 `shouldRenderM` tpl4Output

      it "should allow self-closing tags" $ do
        "<br />" `shouldRenderM` "<br />"

      it "should not remove opening tag brackets in strings" $ do
        -- This was a bug in html-conduit: https://github.com/snoyberg/xml/issues/139
        let customComponentScript =
              "<script>console.log(\"<custom-component />\")console.log(\"<custom-component />\")</script>"
        customComponentScript `shouldRenderM` customComponentScript

    describe "add" $ do
      it "should allow overriden tags" $ do
        hLarcenyState.lSubs .= subst
        "<name /><skater><name /></skater>" `shouldRenderM` "Gotham Girls Amy Roundhouse"

    describe "apply" $ do
      it "should allow templates to be included in other templates" $ do
        hLarcenyState.lLib .= M.fromList [(["hello"], parse "hello")]
        "<apply template=\"hello\" />" `shouldRenderM` "hello"

      it "should allow templates with unfilled holes to be included in other templates" $ do
        hLarcenyState.lSubs .= subs [("alias", textFill "Fifi Nomenom")]
        hLarcenyState.lLib .= M.fromList [(["skater"], parse "<alias />")]
        "<apply template=\"skater\" />" `shouldRenderM` "Fifi Nomenom"

      it "should allow templates to be included in other templates" $ do
        hLarcenyState.lLib .= M.fromList [(["skater"], parse "<apply-content />")]
        "<apply template=\"skater\">V-Diva</apply>" `shouldRenderM` "V-Diva"

      it "should allow compicated templates to be included in other templates" $ do
        let lib = M.fromList [(["_base"], parse "<h1><siteTitle /></h1>\
                                              \<apply-content />")]
        hLarcenyState.lSubs .= subs [("siteTitle", textFill "Ohio Roller Girls")]
        hLarcenyState.lLib .= lib
        "<apply template=\"_base\"><p>The Smacktivist</p></apply>" `shouldRenderM`
          "<h1>Ohio Roller Girls</h1>\
          \<p>The Smacktivist</p>"

      it "should look higher in tree for matching template" $ do
        hLarcenyState.lPath .= ["foo","bar"]
        hLarcenyState.lLib .= M.fromList [(["base"], parse "hello")]
        "<apply template=\"base\" />" `shouldRenderM` "hello"

      it "should look first look in same directory for matching template" $ do
         let lib = M.fromList [(["base"], parse "hello")
                              ,(["foo", "base"], parse "goodbye")]
         hLarcenyState.lLib .= lib
         hLarcenyState.lPath .= ["foo","bar"]
         "<apply template=\"base\" />" `shouldRenderM` "goodbye"

      it "should traverse down via slashes" $ do
         let lib = M.fromList [(["base"], parse "hello")
                              ,(["foo", "base"], parse "goodbye")]
         hLarcenyState.lLib .= lib
         "<apply template=\"foo/base\" />" `shouldRenderM` "goodbye"

      it "should only truncate parts from current path, not specified template path" $ do
         hLarcenyState.lLib .= M.fromList [(["baz"], parse "hello")]
         hLarcenyState.lPath .= ["foo"]
         "<apply template=\"bar/baz\" />" `shouldErrorM` (== (ApplyError ["bar","baz"] ["foo"]))

      it "should use the path to the applied template when looking" $ do
         let lib = M.fromList [(["base"], parse "hello")
                              ,(["foo", "bar", "base"], parse "goodbye")
                              ,(["foo", "bar", "baz"], parse "<apply template=\"base\"/>")]
         hLarcenyState.lLib .= lib
         "<apply template=\"foo/bar/baz\" />" `shouldRenderM` "goodbye"

      it "should use the path to the applied template when looking" $ do
         let lib = M.fromList [(["default", "x"], parse "hello")
                             ,(["foo", "bar", "baz"], parse "<apply-content/>")]
         hLarcenyState.lLib .= lib
         hLarcenyState.lPath .= ["default", "hello"]
         "<apply template=\"foo/bar/baz\"><apply template=\"x\"/></apply>" `shouldRenderM` "hello"

      it "should allow blanks in the the template name" $ do
        let lib = M.fromList [(["zone1-currentIssue"], parse "Current Issue")]
        hLarcenyState.lLib .= lib
        hLarcenyState.lSubs .= subs [("zone1", textFill "zone1-currentIssue")]
        "<apply template=\"${zone1}\" />" `shouldRenderM` "Current Issue"

    describe "overriding HTML tags" $ do
      it "should allow overriden Html tags" $ do
        hLarcenyState.lSubs .= subs [("div", textFill "notadivatall")]
        hLarcenyState.lOverrides .= Overrides mempty ["div"] mempty
        "<html><div></div></html>" `shouldRenderM` "<html>not a div at all</html>"

      it "should allow (nested) overriden Html tags" $ do
        hLarcenyState.lSubs .= subs [("div", textFill "notadivatall")
                                    ,("custom", fillChildrenWith mempty)]
        hLarcenyState.lOverrides .= Overrides mempty ["div"] mempty
        "<html><custom><div></div></custom></html>"
          `shouldRenderM` "<html>not a div at all</html>"

      it "should not need fills for manually added plain nodes" $ do
        hLarcenyState.lOverrides .= Overrides ["blink"] mempty mempty
        "<html><blink>retro!!</blink></html>"
          `shouldRenderM` "<html><blink>retro!!</blink></html>"

      it "should allow custom self-closing tags" $ do
        hLarcenyState.lOverrides .= Overrides ["blink"] mempty ["blink"]
        "<blink />" `shouldRenderM` "<blink />"

    describe "bind" $ do
      it "should let you bind tags to fills within templates" $
        "<bind tag=\"sport\">Roller derby</bind><sport />" `shouldRenderM` "Roller derby"

      it "should let you use binds within binds" $ do
         "<bind tag=\"sport\"> \
         \  <bind tag=\"adjective\">awesome</bind> \
         \  Roller derby is <adjective /> \
         \</bind> \
         \<sport />" `shouldRenderM` "Roller derby is awesome"

      it "should let you bind with nested blanks" $ do
        hLarcenyState.lSubs .= subs [("adjective", textFill "awesome")]
        "<bind tag=\"sport\">Roller derby is <adjective /></bind><sport />"
          `shouldRenderM` "Roller derby is awesome"

      it "should apply binds to applied templates" $ do
        let lib = M.fromList [(["blah"], parse "<apply-content /><foo />")]
        hLarcenyState.lLib .=  lib
        "<bind tag=\"foo\">         \
         \  Fill this in             \
         \</bind>                    \
         \<apply template=\"blah\">  \
         \  <foo />                  \
         \</apply>" `shouldRenderM` "Fill this in Fill this in"

      it "should not let binds escape the apply-content tag" $ do
        hLarcenyState.lSubs .= fallbackSub (Fill $ \_ _ _ -> throw $ SomeError "not found!")
        let lib = M.fromList [(["blah"], parse "<apply-content /><foo />")]
        hLarcenyState.lLib .= lib
        "<apply template=\"blah\"> \
         \  <bind tag=\"foo\">      \
         \    Fill this in          \
         \  </bind>                 \
         \  <foo />                 \
         \</apply>"
           `shouldErrorM` (== SomeError "not found!")

      it "shouldn't matter if there's no `tag` attribute" $ do
        "<bind>This won't ever be rendered!!</bind>\
        \<p>Since the non-existent tag is never referenced \
        \or rendered, it won't blow up.</p>"
          `shouldRenderM`
          "<p>Since the non-existent tag is never referenced \
          \or rendered, it won't blow up.</p>"

    describe "mapSubs" $ do
      it "should map the subs over a list" $ do
        hLarcenyState.lSubs .= subst
        tpl4 `shouldRenderM` tpl4Output

    describe "writing functions" $ do
      it "should allow you to write functions for fills" $ do
        let subs' =
              subs [("desc",
                     Fill $ \m _t _l s -> return (T.take (read $ T.unpack (m M.! "length"))
                                           "A really long description"
                                            <> "...", s))]
        hLarcenyState.lSubs .= subs'
        "<l:desc length=\"10\" />" `shouldRenderM` "A really l..."

      it "should allow you to use IO in fills" $ do
        let subs' =
              subs [("desc", Fill $
                          \m _t _l s -> do putStrLn "***********\nHello World\n***********"
                                           return (T.take (read $ T.unpack (m M.! "length"))
                                             "A really long description"
                                             <> "...", s))]
        hLarcenyState.lSubs .= subs'
        "<l:desc length=\"10\" />" `shouldRenderM` "A really l..."

    describe "attributes" $ do
      it "should apply substitutions to attributes as well" $ do
        hLarcenyState.lSubs .= subs [("skater", textFill "Beyonslay")]
        "<p id=\"${skater}\"><skater /></p>"
          `shouldRenderM` "<p id=\"Beyonslay\">Beyonslay</p>"

      it "should apply substitutions to attributes inside of blanks" $ do
        hLarcenyState.lSubs .= subs [("skater", useAttrs (a"name")
                                       (\name -> textFill $ "Skater: " <> name))
                                    ,("name", textFill "Beyonslay")]
        "<skater name=\"${name}\"><skater />" `shouldRenderM` "Skater: Beyonslay"

      it "should substitute blanks that are only part of attributes" $ do
        hLarcenyState.lSubs .= subs [("skater", useAttrs (a"name")
                                    (\name -> textFill $ "Skater: " <> name))
                                    ,("name", textFill "Beyonslay")]
        "<skater name=\"the great ${name}\"><skater />"
          `shouldRenderM` "Skater: the great Beyonslay"

      it "should substitute multiple blanks in an attribute" $ do
        hLarcenyState.lSubs .=
          subs [("skater", useAttrs (a"name")
                           (\name -> textFill $ "Skater: " <> name))
               ,("name", textFill "Beyonslay")
               ,("adj", textFill "great")]
        "<skater name=\"the ${adj} ${name}\"><skater />"
          `shouldRenderM` "Skater: the great Beyonslay"

      it "should keep special characters in attribute" $ do
        hLarcenyState.lSubs .= subs [("token", textFill "123")
                                    ,("magazine", textFill "BloodAndThunder")
                                    ,("number", textFill "5")]
        "<a href=\"/s/${token}/${magazine}-${number}.pdf\">Issue 5</a>"
          `shouldRenderM` "<a href=\"/s/123/BloodAndThunder-5.pdf\">Issue 5</a>"

      it "should strip whitespace from beginning and end" $
         "<bind tag=\"someAttr\">\n\
         \        lots of space  \n\
         \</bind> <p class=\"${someAttr}\"></p>"
           `shouldRenderM` "<p class=\"lots of space\"></p>"

      it "should know what the template path is" $ do
        let fill = Fill $ \_ (p, _) _ s -> return (head p, s)
        hLarcenyState.lSubs .= subs [("template", fill)]
        "<p class=\"${template}\"></p>"
          `shouldRenderM` "<p class=\"default\"></p>"

    describe "a large template" $ do
      it "should render large HTML files" $ do
        hLarcenyState.lSubs .= subst
        hLarcenyState.lLib .= positionTplLib
        tpl6 `shouldRenderContainingM` "Verso Books"

    describe "escaping" $ do
      it "should escape html in textFill" $ do
        hLarcenyState.lSubs .=
         subs [("someHtml", textFill "<strong>Some HTML</strong>")]
        "  <p><someHtml /></p>" `shouldRenderM`
           "<p>&lt;strong&gt;Some HTML&lt;/strong&gt;</p>"

      it "should not escape html with rawTextFill" $ do
        hLarcenyState.lSubs .=
         subs [("someHtml", rawTextFill "<strong>Some HTML</strong>")]
        "<p><someHtml /></p>" `shouldRenderM`
         "<p><strong>Some HTML</strong></p>"

    describe "br" $ do
      it "should allow self-closing tags" $ do
        "<br />" `shouldRenderM` "<br />"

    describe "selected" $ do
      it "should allow attributes that aren't k-v pairs" $ do
        "<option selected>Hello</option>" `shouldRenderM` "<option selected>Hello</option>"

      it "should allow blanks in attributes that aren't k-v pairs" $ do
        hLarcenyState.lSubs .=
          subs [("selectedA", textFill ""), ("selectedB", textFill "selected")]
        "<option ${selectedA}>Option A</option> \
        \ <option ${selectedB}>Option B</option>" `shouldRenderM`
          "<option >Option A</option><option selected>Option B</option>"

    fallbackTests
    attrTests
    doctypeTests
    conditionalTests
    namespaceTests
  statefulTests

namespaceTests :: SpecWith LarcenyHspecState
namespaceTests =
  describe "namespaces" $ do
    it "should assume that tags with namespaces are blanks" $ do
      "<address><l:address /></address>"
        `shouldRenderM` "<address></address>"
      hLarcenyState.lSubs .=
        subs [("address", textFill "5 Jones St")]
      "<address><l:address /></address>"
        `shouldRenderM` "<address>5 Jones St</address>"
    it "doesn't parse namespaces in attributes" $ do
      hLarcenyState.lSubs .=
        subs [("l:class", textFill "some-class")]
      "<p class=\"${l:class}\">Hello</p>"
        `shouldRenderM` "<p class=\"some-class\">Hello</p>"
    it "should not parse all namespaces as blanks" $ do
      "<svg:svg><path></path></svg>"
        `shouldRenderM` "<svg:svg><path></path></svg:svg>"

statefulTests :: SpecWith ()
statefulTests =
  describe "statefulness" $ do
      it "a fill should be able to affect subsequent fills" $ do
         renderWith (M.fromList [(["default"], parse "<x/><x/>")])
                    (subs [("x", Fill $ \_ _ _ s ->
                                    do let s' = s + 1
                                       return (T.pack (show s'), s'))])
                    0
                    ["default"]
         `shouldReturn` Just "12"
       {- The following test was prompted by a bug where I refuktored the
          bind tag handling to be inside the case statement in `process`.
          The bind tag processor itself calls bind but doesn't return any
          text. This resulted in portions of the template being evaluated
          over and over again.
       -}
      it "should not be affected by binds" $ do
       let tpl = "<bind tag=\"test1\">test1</bind>\
                 \<bind tag=\"test2\">test2</bind>\
                 \<x/><x/>"
       renderWith (M.fromList [(["default"], parse tpl)])
                    (subs [("x", Fill $ \_ _ _ s ->
                                   do let s' = s + 1
                                      return (T.pack (show s'), s'))])
                    0
                    ["default"]
         `shouldReturn` Just "12"

doctypeTests :: SpecWith LarcenyHspecState
doctypeTests = do
  describe "doctypes" $ do
    it "should render blank doctypes" $ do
      "<doctype />" `shouldRenderM` "<!DOCTYPE html>"
    it "should render regular doctype" $ do
      "<!DOCTYPE html>" `shouldRenderM` "<!DOCTYPE html>"
    it "should render doctype in the correct place" $ do
      "<!DOCTYPE html><html><p>Hello world</p></html>"
      `shouldRenderM` "<!DOCTYPE html><html><p>Hello world</p></html>"

conditionalTests :: SpecWith LarcenyHspecState
conditionalTests = do
  describe "conditionals" $ do
    let template cond =
          "<if condition=\"" <> cond <> "\">\
          \  <then>It's true!</then>\
          \  <else>It's false!</else>\
          \</if>"
    describe "true condition" $ do
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template "True" `shouldRenderM` "It's true!"
      it "should work with a blank in the attribute" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("userIsLoggedIn", textFill "True")]
        template "${userIsLoggedIn}" `shouldRenderM` "It's true!"
    describe "false condition" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template "False" `shouldRenderM` "It's false!"
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("userIsLoggedIn", textFill "False")]
        template "${userIsLoggedIn}" `shouldRenderM` "It's false!"

  describe "exists" $ do
    let template =
          "<if exists=\"${existing}\">\
          \  <then>It <existing />!</then>\
          \  <else>It doesn't exist!</else>\
          \</if>"
    describe "the fill exists" $ do
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "exists")]
        template `shouldRenderM` "It exists!"
    describe "the fill is the string \"False\"" $
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "False")]
        template `shouldRenderM` "It False!"
    describe "the fill is an empty string" $
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "")]
        template `shouldRenderM` "It doesn't exist!"
    describe "the fill doesn't exist" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template `shouldRenderM` "It doesn't exist!"

  describe "using condition and exists at the same time" $ do
    let template cond =
          "<if condition=\"" <> cond <> "\" exists=\"${existing}\">\
          \  <then>It <existing />!</then>\
          \  <else>It doesn't exist and/or it's false!</else>\
          \</if>"
    describe "condition is true and tag exists" $ do
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "exists")]
        template "True" `shouldRenderM` "It exists!"
    describe "any other combination" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template "True" `shouldRenderM` "It doesn't exist and/or it's false!"
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "exists")]
        template "False" `shouldRenderM` "It doesn't exist and/or it's false!"
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template "False" `shouldRenderM` "It doesn't exist and/or it's false!"

  describe "using `exists` for lists and nested tags" $ do
    let template =
          "<bind tag=\"rendered\"><list><l:i /></list></bind>\
          \<if exists=\"${rendered}\">\
          \  <then>It is not empty! <list><l:i /></list></then>\
          \  <else>It is empty!</else>\
          \</if>"

    describe "list is not empty" $ do
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("list", mapSubs (\i -> subs [("i", textFill i)]) ["a", "b", "c"])]
        template `shouldRenderM` "It is not empty! abc"

    describe "list is empty" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("list", mapSubs (\i -> subs [("i", textFill i)]) [])]
        template `shouldRenderM` "It is empty!"

    describe "list is doesn't exist" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template `shouldRenderM` "It is empty!"


fallbackTests ::SpecWith LarcenyHspecState
fallbackTests = do
  describe "templates with missing blanks" $ do
    it "should render empty text by default" $ do
      "<p>missing: <missing /></p>" `shouldRenderM` "<p>missing: </p>"
    it "should work if the missing blank has children" $ do
      "<p>missing: <missing>some stuff</missing></p>" `shouldRenderM` "<p>missing: </p>"
  describe "setting custom fallbacks" $ do
    it "should custom fallbacks" $ do
      hLarcenyState.lSubs .= fallbackSub (rawTextFill "I'm a fallback.")
      "<p>missing: <missing /></p>" `shouldRenderM` "<p>missing: I'm a fallback.</p>"
    it "should allow errors to be thrown, e.g., in dev mode" $ do
        hLarcenyState.lSubs .= fallbackSub (Fill $ \_ _ _ -> throw $ SomeError "missing blank!")
        "<p>missing: <missing /></p>" `shouldErrorM` (== (SomeError "missing blank!"))

attrTests :: SpecWith LarcenyHspecState
attrTests =
  describe "useAttrs" $ do
      it "should allow you to *easily* write functions for fills" $ do
        hLarcenyState.lSubs .=
          subs [("desc", useAttrs (a"length")
                         (\n -> textFill $ T.take n
                                "A really long description"
                                <> "..."))]
        "<l:desc length=\"10\" />" `shouldRenderM` "A really l..."

      it "should allow you use multiple args" $ do
         let subs' = subs [("desc", useAttrs (a"length" % a"text")
                                    (\n d -> textFill $ T.take n d <> "..."))]
         hLarcenyState.lSubs .= subs'
         "<l:desc length=\"10\" text=\"A really long description\" />"
           `shouldRenderM` "A really l..."

      it "should allow you use child elements" $ do
        let descTplFill =
              useAttrs (a"length")
                       (\n -> Fill $ \_attrs (_pth, tpl) _l st -> liftIO $ do
                           (t', st') <- runTemplate tpl ["default"] mempty mempty st
                           return (T.take n t' <> "...", st'))
        hLarcenyState.lSubs .= subs [ ("adverb", textFill "really")
                                    , ("desc", descTplFill)]
        "<l:desc length=\"10\">A <adverb /> long description</desc>"
           `shouldRenderM` "A really l..."

      it "should allow optional attributes by giving a Maybe type (not using the optional)" $ do
        hLarcenyState.lSubs .= subs [("desc", descFill)]
        "<l:desc length=\"10\">A really long description</desc>"
          `shouldRenderM` "A really l..."

      it "should allow optional attributes by giving a Maybe type (using optional)" $ do
        hLarcenyState.lSubs .= subs [("desc", descFill)]
        "<l:desc length=\"10\" ending=\" and such \">A really long description</desc>"
          `shouldRenderM` "A really l and such"

      it "should give a nice error message if attribute is missing" $ do
        hLarcenyState.lSubs .=
          subs [("desc", useAttrs (a"length")
                  (\n -> textFill $ T.take n
                         "A really long description"
                         <> "..."))]
        "<l:desc />" `shouldErrorM` (== AttrMissing "length")

      it "should give a nice error message if attribute is unparsable" $ do
        hLarcenyState.lSubs .=
         subs [("desc", useAttrs (a"length")
                                 (\n -> textFill $ T.take n
                                        "A really long description"
                                        <> "..."))]
        "<l:desc length=\"infinite\" />" `shouldErrorM` (== AttrUnparsable "Int" "length")
  where descFill :: Fill ()
        descFill =
          useAttrs (a"length" % a"ending") descFunc

        descFunc :: Int -> Maybe Text -> Fill ()
        descFunc n e = Fill $
          do let ending = fromMaybe "..."  e
             \_attrs (_pth, tpl) _l st -> liftIO $ do
               (renderedText, st') <- runTemplate tpl ["default"] mempty mempty st
               return (T.take n renderedText <> ending, st')

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
