{-# LANGUAGE OverloadedStrings #-}

import           Blaze.ByteString.Builder
import           Control.Lens
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Criterion.Main
import           Criterion.Types
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (unpack)
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.HashMap.Strict        as H
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Examples
import           Heist
import           Heist.Internal.Types
import qualified Heist.Interpreted          as HI
import qualified Text.XmlHtml               as X

import           Larceny

main :: IO ()
main =
  defaultMainWith (defaultConfig {reportFile = Just "report.html"}) [
      bgroup "runTemplate" [ bench "no blanks" $ nf runTpl tpl1
                         , bench "simple blank" $ nf runTpl tpl2
                         , bench "applyTemplate" $ nf runTpl tpl3
                         , bench "mapFills" $ nf runTpl tpl4
                         , bench "funFill" $ nf runTpl tpl5]
    , bgroup "heist" [ bench "no blanks" $ nfIO (doHeist "tpl1" tpl1)
                     , bench "simple blank" $ nfIO (doHeist "tpl2" tpl2)
                     , bench "mapFills" $ nfIO (doHeist "tpl4" tpl4)
                     ]
    ]

runTpl :: Text -> Text
runTpl x = runTemplate (parse x) subst tplLib

splices :: MonadIO m => Splices (HI.Splice m)
splices = do "site-title" ## siteTitleSplice
             "name"       ## siteTitleSplice
             "skater"     ## skaterSplices
             "skaters"    ## skatersSplice
    --         "desc"       ## descSplice

siteTitleSplice :: MonadIO m => HI.Splice m
siteTitleSplice = return [X.TextNode "Gotham Girls roster"]

textSplice :: MonadIO m => Text -> HI.Splice m
textSplice t' = return [X.TextNode t']

skaterSplices :: MonadIO m => HI.Splice m
skaterSplices = HI.runChildrenWith $ "name" ## textSplice "Amy Roundhouse"

skatersSplice :: MonadIO m => HI.Splice m
skatersSplice = HI.mapSplices namePositionSplice
                [("Bonnie Thunders", "jammer")
                ,("Donna Matrix", "blocker")
                ,("V-Diva", "jammer")]

namePositionSplice :: MonadIO m => (Text, Text) -> HI.Splice m
namePositionSplice (n, p) =
  HI.runChildrenWith $ do
    "name" ## textSplice n
    "position" ## textSplice p

{-
descSplice :: MonadIO m => HI.Splice m
descSplice = undefined
-}

doHeist :: BS.ByteString -> Text -> IO Text
doHeist tplName htpl = do
  eitherHs <- runEitherT $ initHeist (templateHC tplName htpl)
  let hs = case eitherHs of
            Left e -> error $ concat e
            Right x -> x
  mBuilderMIME <- HI.renderTemplate hs tplName
  case mBuilderMIME of
   Nothing -> error "blah"
   Just (h,_) -> return $ decodeUtf8 $ toStrict $ toLazyByteString h

templateHC :: BS.ByteString -> Text -> HeistConfig IO
templateHC tplName htpl = HeistConfig sc "" False
  where
    sc = mempty &
         scInterpretedSplices .~ splices &
         scTemplateLocations .~ [EitherT $ return (hTplRepo tplName htpl)]

hTplRepo :: BS.ByteString -> Text -> Either [String] TemplateRepo
hTplRepo tplName htpl =
  case parsed of
    Left e -> Left [e]
    Right d -> Right (H.fromList [([tplName], d)])
  where parsed = fmap (\x -> DocumentFile x (Just (unpack tplName))) (X.parseHTML (unpack tplName) (encodeUtf8 htpl))
