{-# LANGUAGE OverloadedStrings #-}

module Web.Larceny.Legacy ( textFill
                          , textFill'
                          , rawTextFill
                          , rawTextFill'
                          , mapSubs
                          , mapSubs'
                          , fillChildren
                          , fillChildrenWith
                          , fillChildrenWith'
                          , maybeFillChildrenWith
                          , maybeFillChildrenWith'
                          , ifFill
                          , useAttrs
                          , a
                          , (%)) where

import           Control.Monad.State (StateT)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified HTMLEntities.Text   as HE
------------
import           Web.Larceny.Fills   (a, (%))
import           Web.Larceny.Types

ifFill :: Fill s
ifFill =
  useAttrs (a "condition" % a "exists") ifFill'
  where ifFill' :: Maybe Bool -> Maybe Text -> Fill s
        ifFill' mCondition mExisting =
          let condition = fromMaybe True mCondition
              existing = fromMaybe "exist" mExisting
              bool = condition && existing /= ""
              thenElseSubs = subs [("then", thenFill bool)
                                  ,("else", thenFill (not bool))] in
          fillChildrenWith thenElseSubs
        thenFill True = fillChildren
        thenFill False = textFill ""

textFill :: Text -> Fill s
textFill t = textFill' (return t)

rawTextFill :: Text -> Fill s
rawTextFill t = rawTextFill' (return t)

textFill' :: StateT s IO Text -> Fill s
textFill' t = Fill $ \_a _t -> HE.text <$> toLarcenyState t

rawTextFill' :: StateT s IO Text -> Fill s
rawTextFill' t = Fill $ \_a _t -> toLarcenyState t

mapSubs :: (a -> Substitutions s)
        -> [a]
        -> Fill s
mapSubs f xs = Fill $ \_attrs tpl -> do
    T.concat <$>  mapM (\n -> runTemplate tpl (f n)) xs

mapSubs' :: (a -> StateT s IO (Substitutions s)) -> [a] -> Fill s
mapSubs' f xs = Fill $
  \_m tpl -> do
    T.concat <$>  mapM (\x -> do
                           s' <- toLarcenyState $ f x
                           runTemplate tpl s') xs

fillChildren :: Fill s
fillChildren = fillChildrenWith mempty

fillChildrenWith :: Substitutions s -> Fill s
fillChildrenWith m = maybeFillChildrenWith (Just m)

fillChildrenWith' :: StateT s IO (Substitutions s) -> Fill s
fillChildrenWith' m = maybeFillChildrenWith' (Just <$> m)

maybeFillChildrenWith :: Maybe (Substitutions s) -> Fill s
maybeFillChildrenWith Nothing = textFill ""
maybeFillChildrenWith (Just s) = Fill $ \_attrs tpl -> do
  runTemplate tpl s

maybeFillChildrenWith' :: StateT s IO (Maybe (Substitutions s)) -> Fill s
maybeFillChildrenWith' sMSubs = Fill $ \_s (Template tpl) -> do
  mSubs <- toLarcenyState sMSubs
  case mSubs of
    Nothing -> return ""
    Just s  -> tpl s

useAttrs :: (Attributes -> k -> Fill s)
         ->  k
         ->  Fill s
useAttrs k fill= Fill $ \atrs tpl ->
  unFill (k atrs fill) atrs tpl
