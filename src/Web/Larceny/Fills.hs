{-# LANGUAGE OverloadedStrings #-}

module Web.Larceny.Fills ( textFill
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

import           Control.Exception
import           Control.Monad.State (StateT)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified HTMLEntities.Text   as HE
------------
import           Web.Larceny.Types


-- | A conditional fill.
--
-- There are two options: `if` can test if the \"condition\" attribute
-- (a Bool) is True, or it can test if the \"exists\" attribute contains
-- non-empty Text.
--
-- If the conditions provided are true, the `then` block will be filled
-- in. If the conditions are not true, then the `else` block will be filled in.
--
-- @
-- \<if condition=\"True\">
--    \<then>It's true!\<\/then>
--    \<else>It's false!\<\/else>
-- \<\/if>
-- @
-- > It's true!
--
-- @
-- \<if exists=\"some text\">
--    \<then>It exists!\<\/then>
--    \<else>It doesn't exist!\<\/else>
-- \<\/if>
-- @
-- > It exists!
--
-- You can also use exists to see if a list or nested tag is empty, in
-- combination with `bind`.
--
-- @
-- \<bind tag=\"renderedList\">\<list>\<listItem />\<\/list>\<\/bind>
-- \<if exists=\"${renderedList}\">
--   \<then>This list is not empty.\<\/then>
--   \<else>This list is empty!\</else>
-- <\/if>
-- @
-- > This list is not empty.
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

-- | A plain text fill.
--
-- @
-- textFill "This text will be escaped and displayed in place of the blank"
-- @
textFill :: Text -> Fill s
textFill t = textFill' (return t)

-- | A plain text fill.
--
-- @
-- textFill "This text will be displayed in place of the blank, <em>unescaped</em>"
-- @
rawTextFill :: Text -> Fill s
rawTextFill t = rawTextFill' (return t)

-- | Use state or IO, then fill in some text.
--
-- @
-- -- getTextFromDatabase :: StateT () IO Text
-- textFill' getTextFromDatabase
-- @
textFill' :: StateT s IO Text -> Fill s
textFill' t = Fill $ \_a _t -> HE.text <$> toLarcenyState t

-- | Use state or IO, then fill in some text.
--
-- @
-- -- getTextFromDatabase :: StateT () IO Text
-- textFill' getTextFromDatabase
-- @
rawTextFill' :: StateT s IO Text -> Fill s
rawTextFill' t = Fill $ \_a _t -> toLarcenyState t

-- | Create substitutions for each element in a list and fill the child nodes
-- with those substitutions.
--
-- @
-- \<members>\<name \/>\<\/members>
-- ("members", mapSubs (\name -> subs [("name", textFill name)])
--                     ["Bonnie Thunders", "Donna Matrix", \"Beyonslay\"]
-- @
--
-- > Bonnie Thunders Donna Matrix Beyonslay
mapSubs :: (a -> Substitutions s)
        -> [a]
        -> Fill s
mapSubs f xs = Fill $ \_attrs tpl -> do
    T.concat <$>  mapM (\n -> runTemplate tpl (f n)) xs

-- | Create substitutions for each element in a list (using IO/state if
-- needed) and fill the child nodes with those substitutions.
mapSubs' :: (a -> StateT s IO (Substitutions s)) -> [a] -> Fill s
mapSubs' f xs = Fill $
  \_m tpl -> do
    T.concat <$>  mapM (\x -> do
                           s' <- toLarcenyState $ f x
                           runTemplate tpl s') xs

-- | Fill in the child nodes of the blank with substitutions already
-- available.
--
-- @
-- \<no-op>\<p>Same\<\/p>\<\/no-op>
-- ("no-op", fillChildren)
-- @
--
-- > <p>Same</p>
fillChildren :: Fill s
fillChildren = fillChildrenWith mempty

-- | Fill in the child nodes of the blank with new substitutions.
--
-- @
-- \<member>\<name \/>\<\/member>
-- ("skater", fillChildrenWith (subs $ [("name", textFill "Bonnie Thunders")]))
-- @
--
-- > Beyonslay
fillChildrenWith :: Substitutions s -> Fill s
fillChildrenWith m = maybeFillChildrenWith (Just m)

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

-- | Fill with substitutions if those substitutions are provided.
--
-- @
-- \<ifDisplayUser>\<userName \/>\<\/ifDisplayUser>
-- ("ifDisplayUser", maybeFillChildrenWith
--                     (Just $ subs' ("userName", textFill "Bonnie Thunders")))
-- @
--
-- > Bonnie Thunders
maybeFillChildrenWith :: Maybe (Substitutions s) -> Fill s
maybeFillChildrenWith Nothing = textFill ""
maybeFillChildrenWith (Just s) = Fill $ \_attrs tpl -> do
  runTemplate tpl s

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
maybeFillChildrenWith' sMSubs = Fill $ \_s (Template tpl) -> do
  mSubs <- toLarcenyState sMSubs
  case mSubs of
    Nothing -> return ""
    Just s  -> tpl s

-- | Use attributes from the the blank as arguments to the fill.
--
-- @
-- \<desc length=\"10\" \/>
-- ("desc", useAttrs (a"length") descriptionFill)
-- descriptionFill len = textFill $ T.take len
--                                  "A really long description"
--                                  <> "..."))
-- @
--
-- > A really l...
--
-- `useAttrs` takes two arguments. The first is a way to get values of
-- attributes that you can use in Fills. You can use `a` and `%` to
-- create these. The second argument is a function that uses the
-- values of those attributes to create a Fill.
useAttrs :: (Attributes -> k -> Fill s)
         ->  k
         ->  Fill s
useAttrs k fill= Fill $ \atrs tpl ->
  unFill (k atrs fill) atrs tpl

-- | Prepend `a` to the name of an attribute to pass the value of that
-- attribute to the fill.
--
-- The type of the attribute is whatever type the fill expects. If `a`
-- can't parse the value, then there will be an error when the template
-- is rendered.
a :: (FromAttribute a) => Text -> Attributes -> (a -> b) -> b
a attrName attrs k =
  let mAttr = M.lookup attrName attrs in
  k (either (\e -> throw $ e attrName) id (fromAttribute mAttr))

-- | Use with `a` to use multiple attributes in the fill.
--
-- @
-- \<desc length=\"10\" \/>
-- ("desc", useAttrs (a"length" % a"ending") descriptionFill)
-- descriptionFill len maybeEnding =
--   let ending = fromMaybe "..." maybeEnding in
--   textFill $ T.take n
--              "A really long description"
--              <> ending))
-- @
--
-- > A really l...
(%) :: (Attributes -> a -> b)
    -> (Attributes -> b -> c)
    ->  Attributes -> a -> c
(%) f1 f2 attrs k = f2 attrs (f1 attrs k)
