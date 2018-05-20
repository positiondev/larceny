{-# LANGUAGE OverloadedStrings #-}

module Web.Larceny.Types ( Blank(..)
                         , Fill(..)
                         , Attributes
                         , Name(..)
                         , Substitutions
                         , subs
                         , fallbackSub
                         , Template(..)
                         , Path
                         , Library
                         , Overrides(..)
                         , defaultOverrides
                         , FromAttribute(..)
                         , AttrError(..)
                         , ApplyError(..)
                         , LarcenyState(..)
                         , Node(..)
                         , Element(..)) where

import           Control.Exception
import           Control.Monad.State (StateT)
import           Data.Hashable       (Hashable, hash, hashWithSalt)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Read           (readMaybe)

data Node = NodeElement Element
          | NodeContent Text
          | NodeComment Text

data Element = PlainElement Name Attributes [Node]
             | ApplyElement Attributes [Node]
             | BindElement Attributes [Node]
             | BlankElement Name Attributes [Node]
             | DoctypeElement

data LarcenyState s =
  LarcenyState { _lPath      :: [Text]
               , _lSubs      :: Substitutions s
               , _lLib       :: Library s
               , _lOverrides :: Overrides
               , _lLogger    :: (Text -> IO ())
               , _lAppState  :: s }

-- | Corresponds to a "blank" in the template that can be filled in
-- with some value when the template is rendered.  Blanks can be tags
-- or they can be all or parts of attribute values in tags.
--
-- Example blanks:
--
-- @
-- \<skater>                           \<- "skater"
-- \<p class=${name}>                  \<- "name"
-- \<skater name="${name}">            \<- both "skater" and "name"
-- \<a href="teams\/${team}\/{$number}"> \<- both "team" and number"
-- @
data Blank = Blank Text | FallbackBlank  deriving (Eq, Show, Ord)

instance Hashable Blank where
  hashWithSalt s (Blank tn) = s + hash tn
  hashWithSalt s FallbackBlank = s + hash ("FallbackBlank" :: Text)

-- | A  Fill is how to fill in a Blank.
--
-- In most cases, you can use helper functions like `textFill` or
-- `fillChildrenWith` to create your fills. You can also write Fills
-- from scratch.
--
-- @
-- Fill $ \attrs _tpl _lib ->
--          return $ T.pack $ show $ M.keys attrs)
-- @
--
-- With that Fill, a Blank like this:
--
-- > <displayAttrs attribute="hello!" another="goodbye!"/>
--
-- would be rendered as:
--
-- > ["attribute", "another"]
--
-- Fills (and Substitutions and Templates) have the type `StateT s IO
-- Text` in case you need templates to depend on IO actions (like
-- looking something up in a database) or store state (perhaps keeping
-- track of what's already been rendered).
newtype Fill s = Fill { unFill :: Attributes
                               -> (Path, Template s)
                               -> Library s
                               -> StateT s IO Text }

-- | The Blank's attributes, a map from the attribute name to
-- it's value.
type Attributes = Map Text Text

data Name = Name { nNamespace :: Maybe Text
                 , nName      :: Text } deriving (Eq, Ord, Show)

-- | A map from a Blank to how to fill in that Blank.
type Substitutions s = Map Blank (Fill s)

-- | Turn tuples of text and fills to Substitutions.
--
-- @
-- subs [("blank", textFill "the fill")
--      ,("another-blank", textFill "another fill")]
-- @
subs :: [(Text, Fill s)] -> Substitutions s
subs = M.fromList . map (\(x, y) -> (Blank x, y))

-- | Say how to fill in Blanks with missing Fills.
--
-- @
-- \<nonexistent \/>
-- fallbackSub (textFill "I'm a fallback.")
-- @
-- > I'm a fallback.
--
-- You can add the resulting Substitutions to your regular Substitutions using
-- `mappend` or `(<>)`
--
-- @
-- \<blank \/>, <nonexistent \/>
-- subs [("blank", textFill "a fill")] <> fallbackSub (textFill "a fallback")
-- @
-- > a fill, a fallback
fallbackSub :: Fill s -> Substitutions s
fallbackSub fill = M.fromList [(FallbackBlank, fill)]

-- | When you run a Template with the path, some substitutions, and the
-- template library, you'll get back some stateful text.
--
-- Use `loadTemplates` to load the templates from some directory
-- into a template library. Use the `render` functions to render
-- templates from a Library by path.
newtype Template s = Template { runTemplate :: Path
                                            -> Substitutions s
                                            -> Library s
                                            -> StateT s IO Text }

-- | The path to a template.
type Path = [Text]

-- | A collection of templates.
type Library s = Map Path (Template s)

-- | If no substitutions are given, Larceny only understands valid
-- HTML 5 tags. It won't attempt to "fill in" tags that are already
-- valid HTML 5. Use Overrides to use non-HTML 5 tags without
-- providing your own substitutions, or to provide your own fills for
-- standard HTML tags.
--
-- @
-- -- Use the deprecated "marquee" and "blink" tags and write your
-- -- own fill for the "a" tag.
-- Overrides ["marquee", "blink"] ["a"]
-- @
data Overrides = Overrides { customPlainNodes :: [Text]
                           , overrideNodes    :: [Text]
                           , selfClosingNodes :: [Text]}

instance Monoid Overrides where
  mempty = Overrides [] [] []
  mappend (Overrides p o sc) (Overrides p' o' sc') =
    Overrides (p <> p') (o <> o') (sc <> sc')

-- | Default uses no overrides.
defaultOverrides :: Overrides
defaultOverrides = Overrides mempty mempty mempty

type AttrName = Text

-- | If an attribute is required but missing, or unparsable, one of
-- these errors is thrown.
data AttrError = AttrMissing AttrName
               | AttrUnparsable Text AttrName
               | OtherAttrError Text AttrName deriving (Eq)
instance Exception AttrError

instance Show AttrError where
  show (AttrMissing name) = "Missing attribute \"" <> T.unpack name <> "\"."
  show (AttrUnparsable toType name) = "Attribute with name \""
    <> T.unpack name <> "\" can't be parsed to type \""
    <> T.unpack toType <> "\"."
  show (OtherAttrError e name) = "Error parsing attribute \""
    <> T.unpack name <> "\": " <> T.unpack e

-- | A typeclass for things that can be parsed from attributes.
class FromAttribute a where
  fromAttribute :: Maybe Text -> Either (Text -> AttrError) a

instance FromAttribute Text where
  fromAttribute = maybe (Left AttrMissing) Right
instance FromAttribute Int where
  fromAttribute (Just attr) = maybe (Left $ AttrUnparsable "Int") Right $ readMaybe $ T.unpack attr
  fromAttribute Nothing = Left AttrMissing
instance FromAttribute a => FromAttribute (Maybe a) where
  fromAttribute = traverse $ fromAttribute . Just
instance FromAttribute Bool where
  fromAttribute (Just attr) = maybe (Left $ AttrUnparsable "Bool") Right $ readMaybe $ T.unpack attr
  fromAttribute Nothing = Left AttrMissing

data ApplyError = ApplyError Path Path deriving (Eq)
instance Show ApplyError where
  show (ApplyError tplPth pth) =
    "Couldn't find " <> show tplPth <> " relative to " <> show pth <> "."
instance Exception ApplyError

{-# ANN module ("HLint: ignore Use first" :: String) #-}
