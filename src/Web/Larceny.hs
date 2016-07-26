{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Web.Larceny ( Blank(..)
                   , Fill(..)
                   , AttrArgs
                   , Substitutions
                   , Template(..)
                   , Path -- ???
                   , Library
                   , Overrides(..)
                   , FromAttribute(..)
                   , render
                   , renderWith
                   , renderRelative
                   , loadTemplates
                   , defaultOverrides
                   , subs
                   , textFill
                   , textFill'
                   , mapSubs
                   , mapSubs'
                   , fillChildren
                   , fillChildrenWith
                   , fillChildrenWith'
                   , maybeFillChildrenWith
                   , maybeFillChildrenWith'
                   , useAttrs
                   , AttrError(..)
                   , a
                   , (%)
                   , parse
                   , parseWithOverrides) where

import           Control.Monad       (filterM)
import           Control.Monad.State (StateT, evalStateT)
import           Data.Either
import           Data.Hashable       (Hashable)
import qualified Data.HashSet        as HS
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import qualified Data.Text.Lazy.IO   as LT
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.FilePath     (dropExtension, takeExtension)
import qualified Text.HTML.DOM       as D
import           Text.Read           (readMaybe)
import qualified Text.XML            as X
------------
import           Web.Larceny.Html    (html5Nodes)



-- | The type of a "blank" in the template.
newtype Blank = Blank Text deriving (Eq, Show, Ord, Hashable)

-- | A  Fill is how to fill in a Blank.
newtype Fill s = Fill { unFill :: AttrArgs
                               -> (Path, Template s)
                               -> Library s
                               -> StateT s IO Text }

-- | Attributes (if the Blank is a tag)
type AttrArgs = Map Text Text

type Substitutions s = Map Blank (Fill s)

-- | When you run a template with the path, some substitutions, and the
-- template library, you'll get back some stateful text.
newtype Template s = Template { runTemplate :: Path
                                            -> Substitutions s
                                            -> Library s
                                            -> StateT s IO Text }

-- | The path to a template.
type Path = [Text]

-- | A collection of templates.
type Library s = Map Path (Template s)

data Overrides = Overrides { customPlainNodes   :: HS.HashSet Text
                           , overridePlainNodes :: HS.HashSet Text}

defaultOverrides :: Overrides
defaultOverrides = Overrides mempty mempty

-- | Render a template from the library by path.
render :: Library s -> s -> Path -> IO (Maybe Text)
render l = renderWith l mempty

-- | Render a template with some extra substitutions.
renderWith :: Library s -> Substitutions s -> s -> Path -> IO (Maybe Text)
renderWith l sub s = renderRelative l sub s []

-- | Render a template found relative to current template's path.
renderRelative :: Library s -> Substitutions s -> s -> Path -> Path -> IO (Maybe Text)
renderRelative l sub s currentPath targetPath =
  case findTemplate l currentPath targetPath of
    (pth, Just (Template run)) -> Just <$> evalStateT (run pth sub l) s
    (_, Nothing) -> return Nothing

-- | Load all the templates in some directory into a Library.
loadTemplates :: FilePath -> Overrides -> IO (Library s)
loadTemplates path overrides =
  do tpls <- getAllTemplates path
     M.fromList <$>
       mapM (\file -> do content <- LT.readFile (path <> "/" <> file)
                         return (mkPath file,
                                 parseWithOverrides overrides content))
                         tpls
  where mkPath p = T.splitOn "/" $ T.pack $ dropExtension p

getAllTemplates :: FilePath -> IO [FilePath]
getAllTemplates path =
  do cs <- listDirectory path
     let tpls = filter ((== ".tpl") . takeExtension) cs
     dirs <- filterM (doesDirectoryExist . (\d -> path <> "/" <> d)) cs
     rs <- mapM (\dir -> do r <- getAllTemplates (path <> "/" <> dir)
                            return $ map (\p -> dir <> "/" <> p) r) dirs
     return $ tpls ++ concat rs


-- | Turn tuples of text and fills to substitutions.
subs :: [(Text, Fill s)] -> Substitutions s
subs = M.fromList . map (\(x, y) -> (Blank x, y))

-- | A plain  text fill.
textFill :: Text -> Fill s
textFill t = textFill' (return t)

-- | An `StateT s IO Text` fill.
textFill' :: StateT s IO Text -> Fill s
textFill' t = Fill $ \_m _t _l -> t

-- | Create substitutions for each element in a list and use them in the
-- fill.
mapSubs :: (a -> Substitutions s)
        -> [a]
        -> Fill s
mapSubs f xs = Fill $ \_attrs (pth, tpl) lib ->
    T.concat <$>  mapM (\n -> runTemplate tpl pth (f n) lib) xs

-- | Create substitutions for each element in a list and use them in the
-- fill. using State and IO if you like.
mapSubs' :: (a -> StateT s IO (Substitutions s)) -> [a] -> Fill s
mapSubs' f xs = Fill $
  \_m (pth, tpl) lib ->
    T.concat <$>  mapM (\x -> do
                           s' <- f x
                           runTemplate tpl pth s' lib) xs

-- | Fill in the child nodes of the blank with substitutions already
-- available.
fillChildren :: Fill s
fillChildren = fillChildrenWith mempty

-- | Fill in the child nodes of the blank with new substitutions.
fillChildrenWith :: Substitutions s -> Fill s
fillChildrenWith m = maybeFillChildrenWith (Just m)

-- | Use substitutions with State and IO.
fillChildrenWith' :: StateT s IO (Substitutions s) -> Fill s
fillChildrenWith' m = maybeFillChildrenWith' (Just <$> m)

-- | Maybe helper
maybeFillChildrenWith :: Maybe (Substitutions s) -> Fill s
maybeFillChildrenWith Nothing = textFill ""
maybeFillChildrenWith (Just s) = Fill $ \_s (pth, Template tpl) l ->
  tpl pth s l

-- | Maybe with State and IO helper
maybeFillChildrenWith' :: StateT s IO (Maybe (Substitutions s)) -> Fill s
maybeFillChildrenWith' sMSubs = Fill $ \_s (pth, Template tpl) l -> do
  mSubs <- sMSubs
  case mSubs of
    Nothing -> return ""
    Just s  -> tpl pth s l

-- | Use attributes from the the blank as arguments to the fill.
useAttrs :: (AttrArgs -> k -> Fill s)
         ->  k
         ->  Fill s
useAttrs k fill= Fill $ \atrs (pth, tpl) lib ->
     unFill (k atrs fill) atrs (pth, tpl) lib

data AttrError = AttrMissing
               | AttrUnparsable Text
               | AttrOtherError Text deriving (Eq, Show)

class FromAttribute a where
  fromAttribute :: Maybe Text -> Either AttrError a

instance FromAttribute Text where
  fromAttribute = maybe (Left AttrMissing) Right
instance FromAttribute Int where
  fromAttribute (Just attr) = maybe (Left $ AttrUnparsable "Int") Right $ readMaybe $ T.unpack attr
  fromAttribute Nothing = Left AttrMissing
instance FromAttribute a => FromAttribute (Maybe a) where
  fromAttribute = traverse $ fromAttribute . Just

-- | Prepend to the name of an attribute, e.g. `a"name"`.
a :: FromAttribute a => Text -> AttrArgs -> (a -> b) -> b
a attrName attrs k =
  let mAttr = M.lookup attrName attrs in
  k (either (error . T.unpack . attrError) id (fromAttribute mAttr))
  where attrError AttrMissing        = "Attribute error: Unable to find attribute \"" <>
                                       attrName <> "\"."
        attrError (AttrUnparsable t) = "Attribute error: Unable to parse attribute \""
                                       <> attrName <> "\" as type " <> t <> "."
        attrError (AttrOtherError t) = "Attribute error: " <> t

-- | Combine attributes to use in the fill.
(%) :: (AttrArgs -> a -> b)
    -> (AttrArgs -> b -> c)
    ->  AttrArgs -> a -> c
(%) f1 f2 attrs k = f2 attrs (f1 attrs k)

-- | Turn lazy text into templates.
parse :: LT.Text -> Template s
parse = parseWithOverrides defaultOverrides

parseWithOverrides :: Overrides -> LT.Text -> Template s
parseWithOverrides o t =
  let (X.Document _ (X.Element _ _ nodes) _) = D.parseLT ("<div>" <> t <> "</div>")
  in mk o nodes

-- | Turn HTML nodes into templates.
mk :: Overrides -> [X.Node] -> Template s
mk o nodes =
  let unbound = findUnbound o nodes in
  Template $ \pth m l ->
    need pth m unbound <$>
    (T.concat <$> process (ProcessContext pth m l o unbound nodes))

fillIn :: Text -> Substitutions s -> Fill s
fillIn tn m =
  fromMaybe
     (error $ "Missing fill for blank: \"" <> T.unpack tn <> "\"")
     (M.lookup (Blank tn) m)

data ProcessContext s = ProcessContext { _pcPath      :: Path
                                       , _pcSubs      :: Substitutions s
                                       , _pcLib       :: Library s
                                       , _pcOverrides :: Overrides
                                       , _pcUnbound   :: [Blank]
                                       , _pcNodes     :: [X.Node] }

need :: Path -> Map Blank (Fill s) -> [Blank] -> Text -> Text
need pth m keys rest =
  let d = S.difference (S.fromList keys) (M.keysSet m)
  in if S.null d
     then rest
     else error $ "Template " <> show pth
          <> " is missing substitutions for blanks: " <> show d

-- | Add more substitutions to a template.
add :: Substitutions s -> Template s -> Template s
add mouter tpl =
  Template (\pth minner l -> runTemplate tpl pth (minner `M.union` mouter) l)

process :: ProcessContext s ->
           StateT s IO [Text]
process (ProcessContext _ _ _ _ _ []) = return []
process pc@(ProcessContext _ _ _ _ _ (X.NodeElement (X.Element "bind" atr kids):ns)) =
  processBind (pc { _pcNodes = ns }) atr kids
process pc = do
  let (currentNode: nextNodes) = _pcNodes pc
      nextPc = pc { _pcNodes = nextNodes}
  processedNode <-
    case currentNode of
      X.NodeElement (X.Element "apply" atr kids) -> processApply nextPc atr kids
      X.NodeElement (X.Element tn atr kids) | isPlain tn (_pcOverrides pc)
                                                 -> processPlain nextPc tn atr kids
      X.NodeElement (X.Element tn atr kids)      -> processFancy nextPc tn atr kids
      X.NodeContent t                            -> return [t]
      X.NodeComment c                            -> return ["<!--" <> c <> "-->"]
      X.NodeInstruction _                        -> return []
  restOfNodes <- process nextPc
  return $ processedNode ++ restOfNodes

isPlain :: X.Name -> Overrides -> Bool
isPlain tn os =
  let allPlainNodes = (customPlainNodes os `HS.union` html5Nodes)
                      `HS.difference` overridePlainNodes os in
  HS.member (X.nameLocalName tn) allPlainNodes

-- Add the open tag and attributes, process the children, then close
-- the tag.
processPlain :: ProcessContext s ->
                X.Name ->
                Map X.Name Text ->
                [X.Node] ->
                StateT s IO [Text]
processPlain pc@(ProcessContext _ m l o _ _) tn atr kids = do
  atrs <- attrsToText atr
  processed <- process (pc { _pcNodes = kids })
  let tagName = X.nameLocalName tn
  return $ ["<" <> tagName <> atrs <> ">"]
           ++ processed
           ++ ["</" <> tagName <> ">"]
  where attrsToText attrs = T.concat <$> mapM attrToText (M.toList attrs)
        attrToText (k,v) = do
          let name = X.nameLocalName k
              unbound =  eUnboundAttr v
          tuple <- sequence (name, T.concat <$> mapM fillAttr unbound)
          return $ toText tuple
        fillAttr eBlankText =
          case eBlankText of
            Right (Blank hole) -> unFill (fillIn hole m) mempty ([], mk o []) l
            Left text  -> return text

        toText (k, v) = " " <> k <> "=\"" <> v <> "\""

-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processFancy :: ProcessContext s ->
                X.Name ->
                Map X.Name Text ->
                [X.Node] ->
                StateT s IO [Text]
processFancy (ProcessContext pth m l o _ _) tn atr kids =
  let tagName = X.nameLocalName tn in do
  filled <- filledAttrs
  sequence [ unFill (fillIn tagName m)
                    (M.mapKeys X.nameLocalName filled)
                    (pth, add m (mk o kids)) l]
  where filledAttrs = M.fromList <$> mapM fillAttr (M.toList atr)
        fillAttr (k,v) = do
          let unbound =  eUnboundAttr v
          sequence (k, T.concat <$> mapM fillIt unbound)
        fillIt eBlankText =
          case eBlankText of
            Right (Blank hole) -> unFill (fillIn hole m) mempty ([], mk o []) l
            Left text  -> return text

processBind :: ProcessContext s ->
               Map X.Name Text ->
               [X.Node] ->
               StateT s IO [Text]
processBind (ProcessContext pth m l o unbound nodes) atr kids =
  let tagName = atr M.! "tag"
      newSubs = subs [(tagName, Fill $ \_a _t _l ->
                                       runTemplate (mk o kids) pth m l)] in
  process (ProcessContext pth (newSubs `M.union` m) l o unbound nodes)

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library. Phew.
processApply :: ProcessContext s ->
                Map X.Name Text ->
                [X.Node] ->
                StateT s IO [Text]
processApply (ProcessContext pth m l o _ _) atr kids = do
  let (absolutePath, tplToApply) = findTemplateFromAttrs pth l atr
  contentTpl <- runTemplate (mk o kids) pth m l
  let contentSub = subs [("apply-content",
                         textFill contentTpl)]
  sequence [ runTemplate tplToApply absolutePath (contentSub `M.union` m) l ]

findTemplateFromAttrs :: Path ->
                         Library s ->
                         Map X.Name Text ->
                         (Path, Template s)
findTemplateFromAttrs pth l atr =
  let tplPath = T.splitOn "/" $ fromMaybe (error "No template name given.")
                                          (M.lookup "template" atr) in
  case findTemplate l (init pth) tplPath of
    (_, Nothing) -> error $ "apply: Couldn't find " <> show tplPath <> " relative to " <> show pth <> "."
    (targetPath, Just tpl) -> (targetPath, tpl)

findTemplate :: Library s -> Path -> Path -> (Path, Maybe (Template s))
findTemplate lib [] targetPath = (targetPath, M.lookup targetPath lib)
findTemplate lib pth' targetPath =
  case M.lookup (pth' ++ targetPath) lib of
    Just tpl -> (pth' ++ targetPath, Just tpl)
    Nothing -> findTemplate lib (init pth') targetPath

findUnbound :: Overrides -> [X.Node] -> [Blank]
findUnbound _ [] = []
findUnbound o (X.NodeElement (X.Element name atr kids):ns) =
     let tn = X.nameLocalName name in
     if tn == "apply" || tn == "bind" || isPlain name o
     then findUnboundAttrs atr ++ findUnbound o kids
     else Blank tn : findUnboundAttrs atr ++ findUnbound o ns
findUnbound o (_:ns) = findUnbound o ns

findUnboundAttrs :: Map X.Name Text -> [Blank]
findUnboundAttrs atrs = rights $ concatMap (eUnboundAttr . snd) (M.toList atrs)

eUnboundAttr :: Text -> [Either Text Blank]
eUnboundAttr value = do
  let possibleWords = T.splitOn "${" value
  let mWord w =
        case T.splitOn "}" w of
          [_] -> [Left w]
          ["",_] -> [Left ("${" <> w)]
          (word: rest) -> Right (Blank word) : map Left rest
          _ -> [Left w]
  concatMap mWord possibleWords


{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use first" :: String) #-}
