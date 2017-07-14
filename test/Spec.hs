{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE TypeSynonymInstances #-}


import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (Exception, catch, throw, try)
import           Lens.Micro
import           Control.Monad.State     (StateT (..), evalStateT, get, modify,
                                          runStateT)
import qualified Control.Monad.State     as S
import           Control.Monad.Trans     (liftIO)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
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
         do r <- catch
                   (do ((), larcenyHspecState) <- runStateT s st
                       return (larcenyHspecState ^. hResult))
                   return
            putMVar mv r
       takeMVar mv

withLarceny :: SpecWith LarcenyHspecState
            -> Spec
withLarceny spec' =
  let larcenyHspecState =
        LarcenyHspecState H.Success (LarcenyState ["default"] mempty mempty mempty) in
  afterAll return $
    before (return larcenyHspecState) spec'

setResult :: H.Result -> LarcenyHspecM ()
setResult r = case r of
                H.Success -> hResult .= r
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

removeSpaces :: Text -> Text
removeSpaces = T.replace " " ""

renderM :: Text -> LarcenyHspecM Text
renderM templateText = do
  (LarcenyHspecState _ (LarcenyState p s l o)) <- S.get
  let tpl = parseWithOverrides o (LT.fromStrict templateText)
  liftIO $ evalStateT (runTemplate tpl p s l) ()

shouldRenderM :: Text -> Text -> LarcenyHspecM ()
shouldRenderM templateText output = do
  rendered <- renderM templateText
  if removeSpaces rendered == removeSpaces output
    then setResult H.Success
    else let msg = T.unpack $ rendered <> " doesn't match " <> output in
         setResult (H.Fail Nothing msg)

shouldRenderContainingM :: Text -> Text -> LarcenyHspecM ()
shouldRenderContainingM templateText excerpt = do
  rendered <- renderM templateText
  if excerpt `T.isInfixOf` rendered
  then setResult H.Success
  else let msg = T.unpack $ excerpt <> " not found in " <> templateText in
       setResult (H.Fail Nothing msg)

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
               return $ H.Fail Nothing $
                 "rendered successfully instead of throwing expected exception: " <>
                 exceptionType
          Left e ->
            if p e then return H.Success
                   else return $ H.Fail Nothing $ "did not get expected exception: " <>
                        exceptionType <> ", got this exeption instead: " <> show e
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
        let lib = M.fromList [(["blah"], parse "<apply-content /><foo />")]
        hLarcenyState.lLib .= lib
        "<apply template=\"blah\"> \
         \  <bind tag=\"foo\">      \
         \    Fill this in          \
         \  </bind>                 \
         \  <foo />                 \
         \</apply>"
           `shouldErrorM` (== MissingBlanks [Blank "foo"] ["blah"])

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
                     Fill $ \m _t _l -> return $ T.take (read $ T.unpack (m M.! "length"))
                                        "A really long description"
                                        <> "...")]
        hLarcenyState.lSubs .= subs'
        "<desc length=\"10\" />" `shouldRenderM` "A really l..."

      it "should allow you to use IO in fills" $ do
        let subs' =
              subs [("desc", Fill $
                          \m _t _l -> do liftIO $ putStrLn "***********\nHello World\n***********"
                                         return $ T.take (read $ T.unpack (m M.! "length"))
                                           "A really long description"
                                           <> "...")]
        hLarcenyState.lSubs .= subs'
        "<desc length=\"10\" />" `shouldRenderM` "A really l..."

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

    attrTests

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


attrTests :: SpecWith LarcenyHspecState
attrTests =
  describe "useAttrs" $ do
      it "should allow you to *easily* write functions for fills" $ do
        hLarcenyState.lSubs .=
          subs [("desc", useAttrs (a"length")
                         (\n -> textFill $ T.take n
                                "A really long description"
                                <> "..."))]
        "<desc length=\"10\" />" `shouldRenderM` "A really l..."

      it "should allow you use multiple args" $ do
         let subs' = subs [("desc", useAttrs (a"length" % a"text")
                                    (\n d -> textFill $ T.take n d <> "..."))]
         hLarcenyState.lSubs .= subs'
         "<desc length=\"10\" text=\"A really long description\" />"
           `shouldRenderM` "A really l..."

      it "should allow you use child elements" $ do
        let descTplFill =
              useAttrs (a"length")
                       (\n -> Fill $ \_attrs (_pth, tpl) _l -> liftIO $ do
                           t' <- evalStateT (runTemplate tpl ["default"] mempty mempty) ()
                           return $ T.take n t' <> "...")
        hLarcenyState.lSubs .= subs [ ("adverb", textFill "really")
                                    , ("desc", descTplFill)]
        "<desc length=\"10\">A <adverb /> long description</desc>"
           `shouldRenderM` "A really l..."

      it "should allow optional attributes by giving a Maybe type (not using the optional)" $ do
        hLarcenyState.lSubs .= subs [("desc", descFill)]
        "<desc length=\"10\">A really long description</desc>"
          `shouldRenderM` "A really l..."

      it "should allow optional attributes by giving a Maybe type (using optional)" $ do
        hLarcenyState.lSubs .= subs [("desc", descFill)]
        "<desc length=\"10\" ending=\" and such \">A really long description</desc>"
          `shouldRenderM` "A really l and such"

      it "should give a nice error message if attribute is missing" $ do
        hLarcenyState.lSubs .=
          subs [("desc", useAttrs (a"length")
                  (\n -> textFill $ T.take n
                         "A really long description"
                         <> "..."))]
        "<desc />" `shouldErrorM` (== AttrMissing "length")

      it "should give a nice error message if attribute is unparsable" $ do
        hLarcenyState.lSubs .=
         subs [("desc", useAttrs (a"length")
                                 (\n -> textFill $ T.take n
                                        "A really long description"
                                        <> "..."))]
        "<desc length=\"infinite\" />" `shouldErrorM` (== AttrUnparsable "Int" "length")
  where descFill :: Fill ()
        descFill =
          useAttrs (a"length" % a"ending") descFunc

        descFunc :: Int -> Maybe Text -> Fill ()
        descFunc n e = Fill $
          do let ending = fromMaybe "..."  e
             \_attrs (_pth, tpl) _l -> liftIO $ do
               renderedText <- evalStateT (runTemplate tpl ["default"] mempty mempty) ()
               return $ T.take n renderedText <> ending

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
