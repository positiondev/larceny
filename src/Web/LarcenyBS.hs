{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

{-|

A long description with example use with Scotty.

It'll be something like:

Write some templates.

Here's _post_base.tpl:
@
<html>
  <head><title>Post: <pageTitle /></title></head>
  <body>
    <header>
      <h1><pageTitle /></h1>
    </header>

    <apply-content />

  </body>
</html>
@

And post.tpl:
@
<apply template="_base">
  <bind tag="pageTitle"><someTitle /></bind>
  <h
</apply>
@

Add a `Library` to your Scotty state with `loadTemplates`.

Write some `Substitutions`.

Write some Larceny/Scotty glue.

Use `render` and `renderWith` in your Scotty handlers.

Here's a `larcenyServe`.

Admire your lovely app!!

-}

module Web.LarcenyBS ( Blank(..)
                     , Fill(..)
                   , Attributes
                   , Substitutions
                   , Template(..)
                   , Path
                   , Library
                   , Overrides(..)
                   , defaultOverrides
                   , render
                   , renderWith
                   , renderRelative
                   , loadTemplates
                   , getAllTemplates
                   , subs
                   , textFill
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
                   , useAttrs
                   , FromAttribute(..)
                   , AttrError(..)
                   , MissingBlanks(..)
                   , ApplyError(..)
                   , a
                   , (%)
                   , parse
                   , parseWithOverrides) where

import           Control.Exception
import           Control.Monad       (filterM)
import           Control.Monad.State (StateT, evalStateT)
import           Data.Either
import           Data.Hashable       (Hashable)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as ST
import qualified Data.Text.Lazy.Builder as BLT
import qualified Data.Text.Lazy      as LT
import qualified HTMLEntities.Text   as HE
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.FilePath     (dropExtension, takeExtension)
import qualified Text.HTML.DOM       as D
import           Text.Read           (readMaybe)
import qualified Text.XML            as X
------------
import           Web.Larceny.Html    (html5Nodes, html5SelfClosingNodes)


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
newtype Blank = Blank Text deriving (Eq, Show, Ord, Hashable)

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
                               -> StateT s IO BLT.Builder }

-- | The Blank's attributes, a map from the attribute name to
-- it's value.
type Attributes = Map Text Text

-- | A map from a Blank to how to fill in that Blank.
type Substitutions s = Map Blank (Fill s)

-- | When you run a Template with the path, some substitutions, and the
-- template library, you'll get back some stateful text.
--
-- Use `loadTemplates` to load the templates from some directory
-- into a template library. Use the `render` functions to render
-- templates from a Library by path.
newtype Template s = Template { runTemplate :: Path
                                            -> Substitutions s
                                            -> Library s
                                            -> StateT s IO BLT.Builder }

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

-- | Render a template from the library by path.
--
-- @
-- render appTemplates appState ["path", "to", "template"]
-- @
render :: Library s -> s -> Path -> IO (Maybe Text)
render l = renderWith l mempty

-- | Render a template from the library by path, with some additional
-- substitutions.
--
-- @
-- renderWith appTemplates extraSubs appState ["path", "to", "template"]
-- @
renderWith :: Library s -> Substitutions s -> s -> Path -> IO (Maybe Text)
renderWith l sub s = renderRelative l sub s []

-- | Render a template found relative to current template's path.
--
-- This will attempt to find the target template starting at the same
-- level as the given path, then will traverse up the directory tree
-- until it finds a template with the target path.
--
-- For example: Given these templates: ["current"], ["current",
-- "dashboard"], ["current", "private", "dashboard"], ["private",
-- "dashboard"], `renderRelative` called with a given path of
-- ["current"] and target path of ["private", "dashboard"] will find
-- ["current", "private", "dashboard"]. If there /wasn't/ a ["current",
-- "private", "dashboard"], it would render ["private", "dashboard"].
renderRelative :: Library s -> Substitutions s -> s -> Path -> Path -> IO (Maybe Text)
renderRelative l sub s givenPath targetPath =
  case findTemplate l givenPath targetPath of
    (pth, Just (Template run)) -> Just <$> toText <$> evalStateT (run pth sub l) s
    (_, Nothing) -> return Nothing

toText = LT.toStrict . BLT.toLazyText

-- | Load all the templates in some directory into a Library.
loadTemplates :: FilePath -> Overrides -> IO (Library s)
loadTemplates path overrides =
  do tpls <- getAllTemplates path
     M.fromList <$>
       mapM (\file -> do content <- ST.readFile (path <> "/" <> file)
                         return (mkPath file,
                                 parseWithOverrides overrides (LT.fromStrict content)))
                         tpls
  where mkPath p = T.splitOn "/" $ T.pack $ dropExtension p

getAllTemplates :: FilePath -> IO [FilePath]
getAllTemplates path =
  do cExist <- doesDirectoryExist path
     cs <- if cExist then listDirectory path else return []
     let tpls = filter ((== ".tpl") . takeExtension) cs
     dirs <- filterM (doesDirectoryExist . (\d -> path <> "/" <> d)) cs
     rs <- mapM (\dir -> do r <- getAllTemplates (path <> "/" <> dir)
                            return $ map (\p -> dir <> "/" <> p) r) dirs
     return $ tpls ++ concat rs

-- | Turn tuples of text and fills to Substitutions.
--
-- @
-- subs [("blank", textFill "the fill")
--      ,("another-blank", textFill "another fill")]
-- @
subs :: [(Text, Fill s)] -> Substitutions s
subs = M.fromList . map (\(x, y) -> (Blank x, y))

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
textFill' t = Fill $ \_m _t _l -> BLT.fromText <$> HE.text <$> t

-- | Use state or IO, then fill in some text.
--
-- @
-- -- getTextFromDatabase :: StateT () IO Text
-- textFill' getTextFromDatabase
-- @
rawTextFill' :: StateT s IO Text -> Fill s
rawTextFill' t = Fill $ \_m _t _l -> BLT.fromText <$> t

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
mapSubs f xs = Fill $ \_attrs (pth, tpl) lib ->
    fromList <$> mapM (\n -> runTemplate tpl pth (f n) lib) xs

fromList :: [BLT.Builder] -> BLT.Builder
fromList [] = mempty
fromList (x:xs) = x <> fromList xs

-- | Create substitutions for each element in a list (using IO/state if
-- needed) and fill the child nodes with those substitutions.
mapSubs' :: (a -> StateT s IO (Substitutions s)) -> [a] -> Fill s
mapSubs' f xs = Fill $
  \_m (pth, tpl) lib ->
    fromList  <$>  mapM (\x -> do
                            s' <- f x
                            runTemplate tpl pth s' lib) xs

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
maybeFillChildrenWith (Just s) = Fill $ \_s (pth, Template tpl) l ->
  tpl pth s l

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
maybeFillChildrenWith' sMSubs = Fill $ \_s (pth, Template tpl) l -> do
  mSubs <- sMSubs
  case mSubs of
    Nothing -> return ""
    Just s  -> tpl pth s l

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
useAttrs k fill= Fill $ \atrs (pth, tpl) lib ->
  unFill (k atrs fill) atrs (pth, tpl) lib

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

-- | Turn lazy text into templates.
parse :: LT.Text -> Template s
parse = parseWithOverrides defaultOverrides

-- | Use overrides when parsing a template.
parseWithOverrides :: Overrides -> LT.Text -> Template s
parseWithOverrides o t =
  let (X.Document _ (X.Element _ _ nodes) _) = D.parseLT ("<div>" <> t <> "</div>")
  in mk o nodes

-- | Turn HTML nodes and overrides into templates.
mk :: Overrides -> [X.Node] -> Template s
mk o = f
  where allPlainNodes = (HS.fromList (customPlainNodes o) `HS.union` html5Nodes)
                        `HS.difference` HS.fromList (overrideNodes o)
        f nodes = let unbound = findUnbound allPlainNodes nodes in
          Template $ \pth m l -> BLT.fromText <$> 
                       need pth m unbound <$>
                       (toText <$> process (ProcessContext pth m l o unbound allPlainNodes f nodes))

fillIn :: Text -> Substitutions s -> Fill s
fillIn tn m = fromMaybe (textFill "") (M.lookup (Blank tn) m)

data ProcessContext s = ProcessContext { _pcPath          :: Path
                                       , _pcSubs          :: Substitutions s
                                       , _pcLib           :: Library s
                                       , _pcOverrides     :: Overrides
                                       , _pcUnbound       :: [Blank]
                                       , _pcAllPlainNodes :: HashSet Text
                                       , _pcMk            :: [X.Node] -> Template s
                                       , _pcNodes         :: [X.Node]}

data MissingBlanks = MissingBlanks [Blank] Path deriving (Eq)
instance Show MissingBlanks where
  show (MissingBlanks blanks pth) =
    let showBlank (Blank tn) = "\"" <> T.unpack tn <> "\"" in
    "Missing fill for blanks " <> concatMap showBlank blanks
    <> " in template " <> show pth <> "."
instance Exception MissingBlanks

need :: Path -> Map Blank (Fill s) -> [Blank] -> Text -> Text
need pth m keys rest =
  let d = S.difference (S.fromList keys) (M.keysSet m)
  in if S.null d
     then rest
     else throw $ MissingBlanks (S.toList d) pth

add :: Substitutions s -> Template s -> Template s
add mouter tpl =
  Template (\pth minner l -> runTemplate tpl pth (minner `M.union` mouter) l)

process :: ProcessContext s ->
           StateT s IO BLT.Builder
process (ProcessContext _ _ _ _ _ _ _ []) = return mempty
process pc@(ProcessContext _ _ _ _ _ _ _ (X.NodeElement (X.Element "bind" atr kids):ns)) =
  processBind (pc { _pcNodes = ns }) atr kids
process pc = do
  let (currentNode: nextNodes) = _pcNodes pc
      nextPc = pc { _pcNodes = nextNodes}
  processedNode <-
    case currentNode of
      X.NodeElement (X.Element "apply" atr kids) -> processApply nextPc atr kids
      X.NodeElement (X.Element tn atr kids) | HS.member (X.nameLocalName tn) (_pcAllPlainNodes pc)
                                                 -> processPlain nextPc tn atr kids
      X.NodeElement (X.Element tn atr kids)      -> processFancy nextPc tn atr kids
      X.NodeContent t                            -> return $ BLT.fromText t
      X.NodeComment c                            -> return $ BLT.fromText $ "<!--" <> c <> "-->"
      X.NodeInstruction _                        -> return $ mempty
  restOfNodes <- process nextPc
  return $ processedNode <> restOfNodes

blah :: [Text] -> [BLT.Builder]
blah list = map BLT.fromText list

-- Add the open tag and attributes, process the children, then close
-- the tag.
processPlain :: ProcessContext s ->
                X.Name ->
                Map X.Name Text ->
                [X.Node] ->
                StateT s IO BLT.Builder
processPlain pc tn atr kids = do
  atrs <- attrsToText pc atr
  processed <- process (pc { _pcNodes = kids })
  let tagName = X.nameLocalName tn
  return $ tagToText pc tagName atrs processed

selfClosing :: Overrides -> HS.HashSet Text
selfClosing (Overrides _ _ sc) =
  HS.fromList sc <> html5SelfClosingNodes

tagToText :: ProcessContext s
          -> Text
          -> Text
          -> BLT.Builder
          -> BLT.Builder
tagToText pc tagName atrs processed =
  if tagName `HS.member` selfClosing (_pcOverrides pc)
  then BLT.fromText $ "<" <> tagName <> atrs <> "/>"
  else (BLT.fromText $ "<" <> tagName <> atrs <> ">")
           `mappend` processed
           `mappend` (BLT.fromText $ "</" <> tagName <> ">")

listToBuilder :: [Text] -> BLT.Builder
listToBuilder [] = mempty
listToBulder (x:xs) = BLT.fromText x <> listToBuilder xs

attrsToText :: ProcessContext s -> Map X.Name Text -> StateT s IO Text
attrsToText pc attrs =
  T.concat <$> mapM attrToText (M.toList attrs)
  where attrToText (k,v) = do
          let (unboundK, unboundV) =  eUnboundAttrs (k,v)
          keys <- T.concat <$> mapM (fillAttr pc) unboundK
          vals <- T.concat <$> mapM (fillAttr pc) unboundV
          return $ toText (keys, vals)
        toText (k, "") = " " <> k
        toText (k, v) = " " <> k <> "=\"" <> T.strip v <> "\""

fillAttrs :: ProcessContext s -> Map X.Name Text -> StateT s IO (Map X.Name Text)
fillAttrs pc attrs =  M.fromList <$> mapM fill (M.toList attrs)
  where fill p = do
          let (unboundKeys, unboundValues) = eUnboundAttrs p
          keys <- T.concat <$> mapM (fillAttr pc) unboundKeys
          vals <- T.concat <$> mapM (fillAttr pc) unboundValues
          return (X.Name keys Nothing Nothing, vals)

fillAttr :: ProcessContext s -> Either Text Blank -> StateT s IO Text
fillAttr (ProcessContext _ m l _ _ _ mko _) eBlankText =
  case eBlankText of
    Right (Blank hole) -> LT.toStrict <$> BLT.toLazyText <$> unFill (fillIn hole m) mempty ([], mko []) l
    Left text -> return text


-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processFancy :: ProcessContext s ->
                X.Name ->
                Map X.Name Text ->
                [X.Node] ->
                StateT s IO BLT.Builder
processFancy pc@(ProcessContext pth m l _ _ _ mko _) tn atr kids =
  let tagName = X.nameLocalName tn in do
  filled <- fillAttrs pc atr
  unFill (fillIn tagName m)
    (M.mapKeys X.nameLocalName filled)
    (pth, add m (mko kids)) l

processBind :: ProcessContext s ->
               Map X.Name Text ->
               [X.Node] ->
               StateT s IO BLT.Builder
processBind (ProcessContext pth m l o unbound plain mko nodes) atr kids =
  let tagName = atr M.! "tag"
      newSubs = subs [(tagName, Fill $ \_a _t _l -> runTemplate (mko kids) pth m l)] in
  process (ProcessContext pth (newSubs `M.union` m) l o unbound plain mko nodes)

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library.
processApply :: ProcessContext s ->
                Map X.Name Text ->
                [X.Node] ->
                StateT s IO BLT.Builder
processApply pc@(ProcessContext pth m l _ _ _ mko _) atr kids = do
  filledAttrs <- fillAttrs pc atr
  let (absolutePath, tplToApply) = findTemplateFromAttrs pth l filledAttrs
  contentTpl <- runTemplate (mko kids) pth m l
  let contentSub = subs [("apply-content",
                         rawTextFill $ LT.toStrict $ BLT.toLazyText contentTpl)]
  runTemplate tplToApply absolutePath (contentSub `M.union` m) l

data ApplyError = ApplyError Path Path deriving (Eq)
instance Show ApplyError where
  show (ApplyError tplPth pth) =
    "Couldn't find " <> show tplPth <> " relative to " <> show pth <> "."
instance Exception ApplyError

findTemplateFromAttrs :: Path ->
                         Library s ->
                         Map X.Name Text ->
                         (Path, Template s)
findTemplateFromAttrs pth l atr =
  let tplPath = T.splitOn "/" $ fromMaybe (throw $ AttrMissing "template")
                                          (M.lookup "template" atr) in
  case findTemplate l (init pth) tplPath of
    (_, Nothing) -> throw $ ApplyError tplPath pth
    (targetPath, Just tpl) -> (targetPath, tpl)

findTemplate :: Library s -> Path -> Path -> (Path, Maybe (Template s))
findTemplate lib [] targetPath = (targetPath, M.lookup targetPath lib)
findTemplate lib pth' targetPath =
  case M.lookup (pth' ++ targetPath) lib of
    Just tpl -> (pth' ++ targetPath, Just tpl)
    Nothing  -> findTemplate lib (init pth') targetPath

findUnbound :: HashSet Text -> [X.Node] -> [Blank]
findUnbound _ [] = []
findUnbound plainNodes (X.NodeElement (X.Element name atr kids):ns) =
     let tn = X.nameLocalName name in
     if tn == "apply" || tn == "bind" || HS.member tn plainNodes
     then findUnboundAttrs atr ++ findUnbound plainNodes kids
     else Blank tn : findUnboundAttrs atr ++ findUnbound plainNodes ns
findUnbound plainNodes (_:ns) = findUnbound plainNodes ns

findUnboundAttrs :: Map X.Name Text -> [Blank]
findUnboundAttrs atrs =
  rights $ concatMap (uncurry (<>) . eUnboundAttrs) (M.toList atrs)

eUnboundAttrs :: (X.Name, Text) -> ([Either Text Blank], [Either Text Blank])
eUnboundAttrs (X.Name n _ _, value) = do
  let possibleWords = T.splitOn "${"
  let mWord w =
        case T.splitOn "}" w of
          [_] -> [Left w]
          ["",_] -> [Left ("${" <> w)]
          (word: rest) -> Right (Blank word) : map Left rest
          _ -> [Left w]
  ( concatMap mWord (possibleWords n)
    , concatMap mWord (possibleWords value))


{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use first" :: String) #-}
