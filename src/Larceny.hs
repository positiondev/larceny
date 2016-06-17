{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Larceny where

import           Control.Monad       (filterM)
import           Control.Monad.State (StateT, evalStateT)
import           Data.Hashable       (Hashable)
import qualified Data.HashSet        as HS
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Monoid         ((<>))
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import qualified Data.Text.Lazy.IO   as LT
import           Data.Traversable    (for)
import           Html                (plainNodes)
import           System.Directory    (doesDirectoryExist, listDirectory)
import           System.FilePath     (dropExtension, takeExtension)
import qualified Text.HTML.DOM       as D
import           Text.Read           (readMaybe)
import qualified Text.XML            as X

newtype Blank = Blank Text deriving (Eq, Show, Ord, Hashable)
type AttrArgs = Map Text Text
type Fill s = AttrArgs -> (Path, Template s) -> RenderContext s -> StateT s IO Text
type Substitutions s = Map Blank (Fill s)
type Path = [Text]
newtype Template s = Template { runTemplate :: Path
                                            -> Substitutions s
                                            -> RenderContext s
                                            -> StateT s IO Text }
type Library s = Map Path (Template s)

data Overrides = Overrides { customPlainNodes :: HS.HashSet Text
                           , overridePlainNodes :: HS.HashSet Text}

defaultOverrides :: Overrides
defaultOverrides = Overrides mempty mempty

data RenderContext s = RenderContext { library :: Library s
                                     , overrides :: Overrides }

defaultRenderContext :: Library s -> RenderContext s
defaultRenderContext l = RenderContext l defaultOverrides


render :: Library s -> s -> Path -> IO (Maybe Text)
render l = renderWith l mempty

renderWith :: Library s -> Substitutions s -> s -> Path -> IO (Maybe Text)
renderWith l sub s p = M.lookup p l `for` \(Template run) -> evalStateT (run p sub rc) s
  where rc = defaultRenderContext l

loadTemplates :: FilePath -> Overrides -> IO (Library s)
loadTemplates path overrides =
  do tpls <- getAllTemplates path
     M.fromList <$> mapM (\file -> do content <- LT.readFile (path <> "/" <> file)
                                      return (mkPath file, parseWithOverrides overrides content))
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

need :: Path -> Substitutions s -> [Blank] -> Text -> Text
need pth m keys rest =
  let d = S.difference (S.fromList keys) (M.keysSet m)
  in if S.null d
     then rest
     else error $ "Template " <> show pth
          <> " is missing substitutions for blanks: " <> show d

add :: Substitutions s -> Template s -> Template s
add mouter tpl =
  Template (\pth minner l -> runTemplate tpl pth (minner `M.union` mouter) l)

text :: Text -> Fill s
text t = \_m _t _l -> return t

useAttrs :: (AttrArgs -> Template s -> Fill s) -> Fill s
useAttrs f = \atrs (pth, tpl) lib ->
     f atrs tpl atrs (pth, tpl) lib

data AttrError = AttrMissing
               | AttrUnparsable Text
               | AttrOtherError Text deriving (Eq, Show)

class FromAttr a where
  readAttr :: Maybe Text -> Either AttrError a

instance FromAttr Text where
  readAttr = maybe (Left AttrMissing) Right
instance FromAttr Int where
  readAttr (Just attr) = maybe (Left $ AttrUnparsable "Int") Right $ readMaybe $ T.unpack attr
  readAttr Nothing = Left AttrMissing
instance FromAttr a => FromAttr (Maybe a) where
  readAttr = traverse $ readAttr . Just

a :: FromAttr a => Text -> (a -> b) -> AttrArgs -> b
a attrName k attrs =
  let mAttr = M.lookup attrName attrs in
  k (either (error . T.unpack . attrError) id (readAttr mAttr))
  where attrError AttrMissing        = "Attribute error: Unable to find attribute \"" <>
                                       attrName <> "\"."
        attrError (AttrUnparsable t) = "Attribute error: Unable to parse attribute \""
                                       <> attrName <> "\" as type " <> t <> "."
        attrError (AttrOtherError t) = "Attribute error: " <> t

(%) :: (a -> AttrArgs -> b) -> (b -> AttrArgs -> c) ->  a -> AttrArgs -> c
(%) f1 f2 fun attrs = f2 (f1 fun attrs) attrs

mapFills :: (a -> Substitutions s) -> [a] -> Fill s
mapFills f xs = \_m (pth, tpl) lib ->
    T.concat <$>  mapM (\n -> runTemplate tpl pth (f n) lib) xs

fills :: [(Text, Fill s)] -> Substitutions s
fills = M.fromList . map (\(x,y) -> (Blank x, y))

fill :: Substitutions s -> Fill s
fill m = \_m (pth, Template tpl) l -> tpl pth m l

parseWithOverrides :: Overrides -> LT.Text -> Template s
parseWithOverrides o t =
  let (X.Document _ (X.Element _ _ nodes) _) = D.parseLT ("<div>" <> t <> "</div>")
  in mk o nodes

parse :: LT.Text -> Template s
parse = parseWithOverrides defaultOverrides

mk :: Overrides -> [X.Node] -> Template s
mk o nodes = let unbound = findUnbound o nodes
           in Template $ \pth m rc ->
                need pth m (map Blank unbound) <$>
                (T.concat <$> process pth m rc unbound nodes)

fillIn :: Text -> Substitutions s -> Fill s
fillIn tn m =
  fromMaybe
     (error $ "Missing fill for blank: \"" <> T.unpack tn <> "\"")
     (M.lookup (Blank tn) m)

process :: Path -> Substitutions s -> RenderContext s -> [Text] -> [X.Node] -> StateT s IO [Text]
process _ _ _ _ [] = return []
process pth m rc unbound (X.NodeElement (X.Element "bind" atr kids):ns) =
  let tagName = atr M.! "tag"
      newFills = fills [(tagName, \_a _t _l -> runTemplate (mk (overrides rc) kids) pth m rc)] in
  process pth (newFills `M.union` m) rc unbound ns
process pth m rc unbound (n:ns) = do
  let o = overrides rc
      os = overridePlainNodes o
      l = library rc
  processedNode <-
    case n of
      X.NodeElement (X.Element "apply" atr kids) -> processApply pth m rc atr kids
      X.NodeElement (X.Element tn atr kids) | HS.member (X.nameLocalName tn) os
                                                 -> processFancy pth m rc tn atr kids
      X.NodeElement (X.Element tn atr kids) | HS.member (X.nameLocalName tn) plainNodes
                                                 -> processPlain pth m rc unbound tn atr kids
      X.NodeElement (X.Element tn atr kids)      -> processFancy pth m rc tn atr kids
      X.NodeContent t                            -> return [t]
      X.NodeComment c                            -> return ["<!--" <> c <> "-->"]
      X.NodeInstruction _                        -> return []
  restOfNodes <- process pth m rc unbound ns
  return $ processedNode ++ restOfNodes

-- Add the open tag and attributes, process the children, then close
-- the tag.
processPlain :: Path -> Substitutions s -> RenderContext s -> [Text] ->
                 X.Name -> Map X.Name Text -> [X.Node] -> StateT s IO [Text]
processPlain pth m rc unbound tn atr kids = do
  atrs <- attrsToText atr
  processed <- process pth m rc unbound kids
  let tagName = X.nameLocalName tn
  return $ ["<" <> tagName <> atrs <> ">"]
           ++ processed
           ++ ["</" <> tagName <> ">"]
  where attrsToText attrs = T.concat <$> mapM attrToText (M.toList attrs)
        attrToText (k,v) =
          let name = X.nameLocalName k in
          case mUnboundAttr (k,v) of
            Just hole -> do filledIn <- fillIn hole m mempty ([], (mk (overrides rc) [])) rc
                            return $ " " <> name <> "=\"" <> filledIn  <> "\""
            Nothing   -> return $ " " <> name <> "=\"" <> v <> "\""

-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processFancy :: Path -> Substitutions s -> RenderContext s ->
                X.Name -> Map X.Name Text -> [X.Node] -> StateT s IO [Text]
processFancy pth m rc tn atr kids =
  let tagName = X.nameLocalName tn in
  sequence [ fillIn tagName m (M.mapKeys X.nameLocalName atr) (pth, add m (mk (overrides rc) kids)) rc]

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library. Phew.
processApply :: Path -> Substitutions s -> RenderContext s ->
                 Map X.Name Text -> [X.Node] -> StateT s IO [Text]
processApply pth m rc atr kids = do
  let tplPath = T.splitOn "/" $ fromMaybe (error "No template name given.")
                                          (M.lookup "template" atr)
  let (absolutePath, tplToApply) = case findTemplate (init pth) tplPath of
                                    (_, Nothing) -> error $ "Couldn't find " <> show tplPath <> " relative to " <> show pth <> "."
                                    (targetPath, Just tpl) -> (targetPath, tpl)
  contentTpl <- runTemplate (mk (overrides rc) kids) pth m rc
  let contentSub = fills [("apply-content",
                        text contentTpl)]
  sequence [ runTemplate tplToApply absolutePath (contentSub `M.union` m) rc ]
  where l = library rc
        findTemplate [] targetPath = (targetPath, M.lookup targetPath l)
        findTemplate pth' targetPath =
          case M.lookup (pth' ++ targetPath) l of
            Just tpl -> (pth' ++ targetPath, Just tpl)
            Nothing -> findTemplate (init pth') targetPath

findUnbound :: Overrides -> [X.Node] -> [Text]
findUnbound _ [] = []
findUnbound o (X.NodeElement (X.Element name atr kids):ns) =
     let os = overridePlainNodes o
         tn = X.nameLocalName name in
     if tn == "apply" || tn == "bind" || HS.member tn plainNodes
     then
       if HS.member tn os
       then tn : findUnboundAttrs atr ++ findUnbound o ns
       else findUnboundAttrs atr ++ findUnbound o kids
     else tn : findUnboundAttrs atr ++ findUnbound o ns
findUnbound o (_:ns) = findUnbound o ns

findUnboundAttrs :: Map X.Name Text -> [Text]
findUnboundAttrs atrs = mapMaybe mUnboundAttr (M.toList atrs)

mUnboundAttr :: (X.Name, Text) -> Maybe Text
mUnboundAttr (_, value) = do
  endVal <- T.stripPrefix "${" value
  T.stripSuffix "}" endVal


{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use first" :: String) #-}
