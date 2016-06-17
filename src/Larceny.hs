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
type Fill s = AttrArgs -> (Path, Template s) -> Library s -> StateT s IO Text
type Substitutions s = Map Blank (Fill s)
type Path = [Text]
newtype Template s = Template { runTemplate :: Path
                                            -> Substitutions s
                                            -> Library s
                                            -> StateT s IO Text }
type Library s = Map Path (Template s)

render :: Library s -> s -> Path -> IO (Maybe Text)
render l = renderWith l mempty

renderWith :: Library s -> Substitutions s -> s -> Path -> IO (Maybe Text)
renderWith l sub s p = M.lookup p l `for` \(Template run) -> evalStateT (run p sub l) s

loadTemplates :: FilePath -> IO (Library s)
loadTemplates path =
  do tpls <- getAllTemplates path
     M.fromList <$> mapM (\file -> do content <- LT.readFile (path <> "/" <> file)
                                      return (mkPath file, parse content))
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

need :: Path -> Map Blank (Fill s) -> [Blank] -> Text -> Text
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

parse :: LT.Text -> Template s
parse t =
  let (X.Document _ (X.Element _ _ nodes) _) = D.parseLT ("<div>" <> t <> "</div>")
  in mk nodes

mk :: [X.Node] -> Template s
mk nodes = let unbound = findUnbound nodes
           in Template $ \pth m l ->
                need pth m (map Blank unbound) <$>
                (T.concat <$> process pth m l unbound nodes)

fillIn :: Text -> Substitutions s -> Fill s
fillIn tn m =
  fromMaybe
     (error $ "Missing fill for blank: \"" <> T.unpack tn <> "\"")
     (M.lookup (Blank tn) m)

process :: Path -> Substitutions s -> Library s -> [Text] -> [X.Node] -> StateT s IO [Text]
process _ _ _ _ [] = return []
process pth m l unbound (X.NodeElement (X.Element "bind" atr kids):ns) =
  let tagName = atr M.! "tag"
      newFills = fills [(tagName, \_a _t _l -> runTemplate (mk kids) pth m l)] in
  process pth (newFills `M.union` m) l unbound ns
process pth m l unbound (n:ns) = do
  processedNode <-
    case n of
      X.NodeElement (X.Element "apply" atr kids) -> processApply pth m l atr kids
      X.NodeElement (X.Element tn atr kids) | HS.member (X.nameLocalName tn) plainNodes
                                 -> processPlain pth m l unbound tn atr kids
      X.NodeElement (X.Element tn atr kids)     -> processFancy pth m l tn atr kids
      X.NodeContent t               -> return [t]
      X.NodeComment c                -> return ["<!--" <> c <> "-->"]
      X.NodeInstruction _       -> return []
  restOfNodes <- process pth m l unbound ns
  return $ processedNode ++ restOfNodes

-- Add the open tag and attributes, process the children, then close
-- the tag.
processPlain :: Path -> Substitutions s -> Library s -> [Text] ->
                 X.Name -> Map X.Name Text -> [X.Node] -> StateT s IO [Text]
processPlain pth m l unbound tn atr kids = do
  atrs <- attrsToText atr
  processed <- process pth m l unbound kids
  let tagName = X.nameLocalName tn
  return $ ["<" <> tagName <> atrs <> ">"]
           ++ processed
           ++ ["</" <> tagName <> ">"]
  where attrsToText attrs = T.concat <$> mapM attrToText (M.toList attrs)
        attrToText (k,v) =
          let name = X.nameLocalName k in
          case mUnboundAttr (k,v) of
            Just hole -> do filledIn <- fillIn hole m mempty ([], mk []) l
                            return $ " " <> name <> "=\"" <> filledIn  <> "\""
            Nothing   -> return $ " " <> name <> "=\"" <> v <> "\""

-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processFancy :: Path -> Substitutions s -> Library s ->
                X.Name -> Map X.Name Text -> [X.Node] -> StateT s IO [Text]
processFancy pth m l tn atr kids =
  let tagName = X.nameLocalName tn in
  sequence [ fillIn tagName m (M.mapKeys X.nameLocalName atr) (pth, add m (mk kids)) l]

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library. Phew.
processApply :: Path -> Substitutions s -> Library s ->
                 Map X.Name Text -> [X.Node] -> StateT s IO [Text]
processApply pth m l atr kids = do
  let tplPath = T.splitOn "/" $ fromMaybe (error "No template name given.")
                                          (M.lookup "template" atr)
  let (absolutePath, tplToApply) = case findTemplate (init pth) tplPath of
                                    (_, Nothing) -> error $ "Couldn't find " <> show tplPath <> " relative to " <> show pth <> "."
                                    (targetPath, Just tpl) -> (targetPath, tpl)
  contentTpl <- runTemplate (mk kids) pth m l
  let contentSub = fills [("apply-content",
                        text contentTpl)]
  sequence [ runTemplate tplToApply absolutePath (contentSub `M.union` m) l ]
  where findTemplate [] targetPath = (targetPath, M.lookup targetPath l)
        findTemplate pth' targetPath =
          case M.lookup (pth' ++ targetPath) l of
            Just tpl -> (pth' ++ targetPath, Just tpl)
            Nothing -> findTemplate (init pth') targetPath

findUnbound :: [X.Node] -> [Text]
findUnbound [] = []
findUnbound (X.NodeElement (X.Element name atr kids):ns) =
     let tn = X.nameLocalName name in
     if tn == "apply" || tn == "bind" || HS.member tn plainNodes
     then findUnboundAttrs atr ++ findUnbound kids
     else tn : findUnboundAttrs atr
   ++ findUnbound ns
findUnbound (_:ns) = findUnbound ns

findUnboundAttrs :: Map X.Name Text -> [Text]
findUnboundAttrs atrs = mapMaybe mUnboundAttr (M.toList atrs)

mUnboundAttr :: (X.Name, Text) -> Maybe Text
mUnboundAttr (_, value) = do
  endVal <- T.stripPrefix "${" value
  T.stripSuffix "}" endVal


{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use first" :: String) #-}
