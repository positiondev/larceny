{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}


import           Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar,
                                          newMVar, putMVar, readMVar, takeMVar,
                                          tryTakeMVar)
import           Control.DeepSeq         (force)
import           Control.Exception       (Exception, SomeException, catch,
                                          evaluate, throw, try)
import           Control.Lens
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

data LarcenyState =
  LarcenyState { _lPath      :: [Text]
               , _lTpl       :: Text
               , _lSubs      :: Substitutions ()
               , _lLib       :: Library ()
               , _lOverrides :: Overrides }

makeLenses ''LarcenyState

type LarcenyHspecM = StateT LarcenyHspecState IO

data LarcenyHspecState =
  LarcenyHspecState { _hResult       :: H.Result
                    , _hLarcenyState :: LarcenyState }

makeLenses ''LarcenyHspecState

instance Monoid LarcenyState where
  mempty =
    LarcenyState [] "" mempty mempty defaultOverrides
  mappend (LarcenyState p t s l o) (LarcenyState p' t' s' l' o')=
    LarcenyState (p <> p') (t <> t') (s <> s') (l <> l') (o <> o')

addSubs :: Substitutions () -> LarcenyHspecM ()
addSubs s = hLarcenyState.lSubs .= s

{-
addPath :: [Text] -> LarcenyHspecM ()
addPath pth = hLarcenyState.lPath .= pth

addLib :: Library () -> LarcenyHspecM ()
addLib lib = lLib .= lib

addOverrides :: Overrides -> LarcenyState -> LarcenyState
addOverrides or = lOverrides .= or -}

instance H.Example (LarcenyHspecM ()) where
  type Arg (LarcenyHspecM ()) = LarcenyHspecState
  evaluateExample s params actionWithToIO progCallback =
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
withLarceny spec =
  let larcenyHspecState =
        LarcenyHspecState H.Success (LarcenyState ["default"] "" mempty mempty mempty) in
  afterAll return $
    before (return larcenyHspecState) spec
{-
setLarcenyState :: (LarcenyState -> LarcenyState) -> LarcenyHspecM ()
setLarcenyState f = do
  (LarcenyHspecState r p t s l o) <- S.get
  let (LarcenyState p' t' s' l' o') = f (LarcenyState p t s l o)
  S.put (LarcenyHspecState r p' t' s' l' o')_-}

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
  (LarcenyHspecState _ (LarcenyState p _ s l o)) <- S.get
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
        r <- try ((evaluate . force) <$> renderAttempt)
        case r of
          Right _ -> return $ H.Fail Nothing $ "did not get expected exception: " ++ exceptionType
          Left e ->
            if p e then return H.Success
                   else return $ H.Fail Nothing $ "did not get expected exception: " ++ exceptionType ++ ", got this exeption instead: " ++ show e
      setResult result
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) p
      where
        instanceOf :: Selector a -> a
        instanceOf _ = error "Test.Hspec.Expectations.shouldThrow: broken Typeable instance"
{-
shouldRender :: ([Text], Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRender (pth, t', s, l) output = do
  rendered <- evalStateT (runTemplate (parse (LT.fromStrict t')) pth s l) ()
  removeSpaces rendered `shouldBe` removeSpaces output

shouldRenderDef :: (Text, Substitutions (), Library ()) -> Text -> Expectation
shouldRenderDef (t', s, l) output = do
    rendered <- evalStateT (runTemplate (parse (LT.fromStrict t')) ["default"] s l) ()
    removeSpaces rendered `shouldBe` removeSpaces output

shouldRenderCustom :: (Text, Substitutions (), Library (), Overrides) -> Text -> Expectation
shouldRenderCustom (t', s, l, o) output = do
    rendered <- evalStateT (runTemplate (parseWithOverrides o (LT.fromStrict t')) ["default"] s l) ()
    removeSpaces rendered `shouldBe` removeSpaces output

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
-}
main :: IO ()
main = spec

spec :: IO ()
spec = hspec $ do
  withLarceny $ do
    describe "parse" $ do
      it "should parse HTML into a Template" $ do
        hlarcenyState.lSubs .= subst
        hlarcenyState.lLib .= mempty
        tpl4 `shouldRenderM` tpl4Output

      it "should allow self-closing tags" $ do
        "<br />" `shouldRenderM` "<br />"
{-
    describe "add" $ do
      it "should allow overriden tags" $ do
        setLarcenyState (addSubs subst)
        "<name /><skater><name /></skater>" `shouldRenderM` "Gotham Girls Amy Roundhouse"

    describe "apply" $ do

      it "should allow templates to be included in other templates" $ do
        setLarcenyState (addLib (M.fromList [(["hello"], parse "hello")]))
        "<apply template=\"hello\" />" `shouldRenderM` "hello"

      it "should allow templates with unfilled holes to be included in other templates" $ do
        setLarcenyState $ addSubs (subs [("alias", textFill "Fifi Nomenom")])
                       <> addLib (M.fromList [(["skater"], parse "<alias />")])
        "<apply template=\"skater\" />" `shouldRenderM` "Fifi Nomenom"

      it "should allow templates to be included in other templates" $ do
        setLarcenyState $ addLib (M.fromList [(["skater"], parse "<apply-content />")])
        "<apply template=\"skater\">V-Diva</apply>" `shouldRenderM` "V-Diva"

      it "should allow compicated templates to be included in other templates" $ do
        let lib = M.fromList [(["_base"], parse "<h1><siteTitle /></h1>\
                                              \<apply-content />")]
        setLarcenyState $ addSubs (subs [("siteTitle", textFill "Ohio Roller Girls")])
                       <> addLib lib
        "<apply template=\"_base\"><p>The Smacktivist</p></apply>" `shouldRenderM`
          "<h1>Ohio Roller Girls</h1>\
          \<p>The Smacktivist</p>"

      it "should look higher in tree for matching template" $ do
         setLarcenyState $ addPath ["foo","bar"]
                         <> addLib (M.fromList [(["base"], parse "hello")])
         "<apply template=\"base\" />" `shouldRenderM` "hello"

      it "should look first look in same directory for matching template" $ do
         let lib = M.fromList [(["base"], parse "hello")
                              ,(["foo", "base"], parse "goodbye")]
         setLarcenyState $ addPath ["foo","bar"]
                         <> addLib lib
         "<apply template=\"base\" />" `shouldRenderM` "goodbye"

      it "should traverse down via slashes" $ do
         let lib = M.fromList [(["base"], parse "hello")
                              ,(["foo", "base"], parse "goodbye")]
         setLarcenyState $ addLib lib
         "<apply template=\"foo/base\" />" `shouldRenderM` "goodbye"

      it "should only truncate parts from current path, not specified template path" $ do
         setLarcenyState $ addLib (M.fromList [(["baz"], parse "hello")])
                         <> addPath ["foo"]
         "<apply template=\"bar/baz\" />" `shouldErrorM` (== (ApplyError ["bar","baz"] ["foo"]))

      it "should use the path to the applied template when looking" $ do
        let lib = M.fromList [(["base"], parse "hello")
                             ,(["foo", "bar", "base"], parse "goodbye")
                             ,(["foo", "bar", "baz"], parse "<apply template=\"base\"/>")]
        setLarcenyState $ addLib lib
        "<apply template=\"foo/bar/baz\" />" `shouldRenderM` "goodbye"

      it "should use the path to the applied template when looking" $ do
        let lib = M.fromList [(["default", "x"], parse "hello")
                             ,(["foo", "bar", "baz"], parse "<apply-content/>")]
        setLarcenyState $ addLib lib <> addPath ["default", "hello"]
        "<apply template=\"foo/bar/baz\"><apply template=\"x\"/></apply>" `shouldRenderM` "hello"

      it "should allow blanks in the the template name" $ do
        let lib = M.fromList [(["zone1-currentIssue"], parse "Current Issue")]
        setLarcenyState $ addLib lib
                       <> addSubs (subs [("zone1", textFill "zone1-currentIssue")])
        "<apply template=\"${zone1}\" />" `shouldRenderM` "Current Issue"

    describe "overriding HTML tags" $ do

      it "should allow overriden Html tags" $ do
        setLarcenyState $ addSubs (subs [("div", textFill "notadivatall")])
                       <> addOverrides (Overrides mempty ["div"] mempty)
        "<html><div></div></html>" `shouldRenderM` "<html>not a div at all</html>"

      it "should allow (nested) overriden Html tags" $ do

        setLarcenyState $ addSubs (subs [("div", textFill "notadivatall")
                                        ,("custom", fillChildrenWith mempty)])
                       <> addOverrides (Overrides mempty ["div"] mempty)
        "<html><custom><div></div></custom></html>"
          `shouldRenderM` "<html>not a div at all</html>"

      it "should not need fills for manually added plain nodes" $ do
        setLarcenyState $ addOverrides (Overrides ["blink"] mempty mempty)
        "<html><blink>retro!!</blink></html>"
          `shouldRenderM` "<html><blink>retro!!</blink></html>"

      it "should allow custom self-closing tags" $ do
        setLarcenyState $ addOverrides (Overrides ["blink"] mempty ["blink"])
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
        setLarcenyState $ addSubs (subs [("adjective", textFill "awesome")])
        "<bind tag=\"sport\">Roller derby is <adjective /></bind><sport />"
          `shouldRenderM` "Roller derby is awesome"

      it "should apply binds to applied templates" $ do
        let lib = M.fromList [(["blah"], parse "<apply-content /><foo />")]
        setLarcenyState $ addLib lib
        "<bind tag=\"foo\">         \
         \  Fill this in             \
         \</bind>                    \
         \<apply template=\"blah\">  \
         \  <foo />                  \
         \</apply>" `shouldRenderM` "Fill this in Fill this in"
{-
      it "should not let binds escape the apply-content tag" $ do
        let lib = M.fromList [(["blah"], parse "<apply-content /><foo />")]
        ("<apply template=\"blah\"> \
         \  <bind tag=\"foo\">      \
         \    Fill this in          \
         \  </bind>                 \
         \  <foo />                 \
         \</apply>",
         mempty,
           `shouldErrorDef` MissingBlanks [Blank "foo"] ["blah"] -}

    describe "mapSubs" $ do
      it "should map the subs over a list" $ do
        setLarcenyState (addSubs subst)
        tpl4 `shouldRenderM` tpl4Output

    describe "writing functions" $ do
      it "should allow you to write functions for fills" $ do
        let subs' =
              subs [("desc",
                     Fill $ \m _t _l -> return $ T.take (read $ T.unpack (m M.! "length"))
                                        "A really long description"
                                        <> "...")]
        setLarcenyState $ addSubs subs'
        "<desc length=\"10\" />" `shouldRenderM` "A really l..."


      it "should allow you to use IO in fills" $ do
        let subs' =
              subs [("desc", Fill $
                          \m _t _l -> do liftIO $ putStrLn "***********\nHello World\n***********"
                                         return $ T.take (read $ T.unpack (m M.! "length"))
                                           "A really long description"
                                           <> "...")]
        setLarcenyState $ addSubs subs'
        "<desc length=\"10\" />" `shouldRenderM` "A really l..."

    describe "attributes" $ do
      it "should apply substitutions to attributes as well" $ do
        setLarcenyState $ addSubs (subs [("skater", textFill "Beyonslay")])
        "<p id=\"${skater}\"><skater /></p>"
          `shouldRenderM` "<p id=\"Beyonslay\">Beyonslay</p>"

      it "should apply substitutions to attributes inside of blanks" $ do
        setLarcenyState $
          addSubs (subs [("skater", useAttrs (a"name")
                                    (\name -> textFill $ "Skater: " <> name))
                        ,("name", textFill "Beyonslay")])
        "<skater name=\"${name}\"><skater />" `shouldRenderM` "Skater: Beyonslay"

      it "should substitute blanks that are only part of attributes" $ do
        setLarcenyState $
          addSubs (subs [("skater", useAttrs (a"name")
                                    (\name -> textFill $ "Skater: " <> name))
                        ,("name", textFill "Beyonslay")])
        "<skater name=\"the great ${name}\"><skater />"
          `shouldRenderM` "Skater: the great Beyonslay"

      it "should substitute multiple blanks in an attribute" $ do
        setLarcenyState $
          addSubs (subs [("skater", useAttrs (a"name")
                                    (\name -> textFill $ "Skater: " <> name))
                        ,("name", textFill "Beyonslay")
                        ,("adj", textFill "great")])
        "<skater name=\"the ${adj} ${name}\"><skater />"
          `shouldRenderM` "Skater: the great Beyonslay"

      it "should keep special characters in attribute" $ do
        setLarcenyState $ addSubs (subs [("token", textFill "123")
                                        ,("magazine", textFill "BloodAndThunder")
                                        ,("number", textFill "5")])
        "<a href=\"/s/${token}/${magazine}-${number}.pdf\">Issue 5</a>"
         `shouldRenderM` "<a href=\"/s/123/BloodAndThunder-5.pdf\">Issue 5</a>"

      it "should strip whitespace from beginning and end" $
         "<bind tag=\"someAttr\">\n\
         \        lots of space  \n\
         \</bind> <p class=\"${someAttr}\"></p>"
           `shouldRenderM` "<p class=\"lots of space\"></p>"

    describe "a large template" $ do
      it "should render large HTML files" $ do
        setLarcenyState $ addSubs subst
                       <> addLib positionTplLib
        tpl6 `shouldRenderContainingM` "Verso Books"

    describe "escaping" $ do
      it "should not escape html" $ do
        setLarcenyState $
          addSubs (subs [("someHtml", textFill "<strong>Some HTML</strong>")])
        "<p><someHtml /></p>" `shouldRenderM`
          "<p><strong>Some HTML</strong></p>"

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
         setLarcenyState $
           addSubs (subs [("desc", useAttrs (a"length")
                                   (\n -> textFill $ T.take n
                                          "A really long description"
                                          <> "..."))])
         "<desc length=\"10\" />" `shouldRenderM` "A really l..."

      it "should allow you use multiple args" $ do
         let subs' = subs [("desc", useAttrs (a"length" % a"text")
                                    (\n d -> textFill $ T.take n d <> "..."))]
         setLarcenyState $ addSubs subs'
         "<desc length=\"10\" text=\"A really long description\" />"
           `shouldRenderM` "A really l..."

      it "should allow you use child elements" $ do
        let descTplFill =
              useAttrs (a"length")
                       (\n -> Fill $ \_attrs (_pth, tpl) _l -> liftIO $ do
                           t' <- evalStateT (runTemplate tpl ["default"] mempty mempty) ()
                           return $ T.take n t' <> "...")
        setLarcenyState $ addSubs (subs [ ("adverb", textFill "really")
                                        , ("desc", descTplFill)])
        "<desc length=\"10\">A <adverb /> long description</desc>"
           `shouldRenderM` "A really l..."

      it "should allow optional attributes by giving a Maybe type (not using the optional)" $ do
        setLarcenyState $ addSubs (subs [("desc", descFill)])
        "<desc length=\"10\">A really long description</desc>"
          `shouldRenderM` "A really l..."

      it "should allow optional attributes by giving a Maybe type (using optional)" $ do
        setLarcenyState $ addSubs (subs [("desc", descFill)])
        "<desc length=\"10\" ending=\" and such \">A really long description</desc>"
          `shouldRenderM` "A really l and such"
{-
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
          mempty) `shouldErrorDef` AttrUnparsable "Int" "length" -}
  where descFill :: Fill ()
        descFill =
          useAttrs (a"length" % a"ending") descFunc

        descFunc :: Int -> Maybe Text -> Fill ()
        descFunc n e = Fill $
          do let ending = fromMaybe "..."  e
             \_attrs (_pth, tpl) _l -> liftIO $ do
               renderedText <- evalStateT (runTemplate tpl ["default"] mempty mempty) ()
               return $ T.take n renderedText <> ending
-}
{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
