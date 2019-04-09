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
                          , (%)
                          , fill ) where

import           Control.Exception
import           Control.Monad       (foldM)
import           Control.Monad.State (StateT, runStateT)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified HTMLEntities.Text   as HE
------------
import           Web.Larceny.Types
import           Web.Larceny.Fills hiding (textFill', rawTextFill', mapSubs', fillChildrenWith', maybeFillChildrenWith')


fill :: (Attributes -> (Path, Template s) -> Library s -> StateT s IO Text) -> Fill s
fill f = Fill $ \attrs t lib st -> runStateT (f attrs t lib) st

-- | Use state or IO, then fill in some text.
--
-- @
-- -- getTextFromDatabase :: StateT () IO Text
-- textFill' getTextFromDatabase
-- @
textFill' :: StateT s IO Text -> Fill s
textFill' t = Fill $ \_m _t _l st -> do
   (t, st') <- runStateT t st
   return (HE.text t, st')

-- | Use state or IO, then fill in some text.
--
-- @
-- -- getTextFromDatabase :: StateT () IO Text
-- textFill' getTextFromDatabase
-- @
rawTextFill' :: StateT s IO Text -> Fill s
rawTextFill' t = Fill $ \_m _t _l -> runStateT t

-- | Create substitutions for each element in a list (using IO/state if
-- needed) and fill the child nodes with those substitutions.
mapSubs' :: (a -> StateT s IO (Substitutions s)) -> [a] -> Fill s
mapSubs' f xs = Fill $
  \_m (pth, tpl) lib st ->
    foldM
      (\(text, st) item -> do
        (s', st' ) <- runStateT (f item) st
        (t , st'') <- runTemplate tpl pth s' lib st'
        return (text <> t, st''))
      ("", st)
      xs

-- | Use substitutions with State and IO.
--
-- @
-- \<changeTheWorld>\<results \/>\<\/changeTheWorld>
-- -- doABunchOfStuffAndGetSubstitutions :: StateT () IO (Substitutions ())
-- ("changeTheWorld", fillChildrenWith' doStuffAndGetSubstitutions)
-- @
--
-- > This template did IO!
fillChildrenWith' :: StateT s IO (Substitutions s) -> Fill s
fillChildrenWith' m = maybeFillChildrenWith' (Just <$> m)

-- | Use state and IO and maybe fill in with some substitutions.
--
-- @
-- \<ifLoggedIn>Logged in as \<userName \/>\<\/ifLoggedIn>
-- ("ifLoggedIn", maybeFillChildrenWith' $ do
--                  mUser <- getLoggedInUser -- returns (Just "Bonnie Thunders")
--                  case mUser of
--                    Just user -> Just $ subs' ("userName", textFill user)
--                    Nothing   -> Nothing)
-- @
--
-- > Bonnie Thunders
maybeFillChildrenWith' :: StateT s IO (Maybe (Substitutions s)) -> Fill s
maybeFillChildrenWith' sMSubs = Fill $ \_s (pth, Template tpl) l st -> do
  (mSubs, newState) <- runStateT sMSubs st
  case mSubs of
    Nothing -> return ("", newState)
    Just s  -> tpl pth s l newState