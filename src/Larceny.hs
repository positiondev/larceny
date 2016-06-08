{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Larceny where

import           Data.Hashable      (Hashable)
import qualified Data.HashSet       as HS
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Data.Monoid        ((<>))
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy     as LT
import qualified Text.HTML.DOM      as D
import qualified Text.XML           as X

newtype Blank = Blank Text deriving (Eq, Show, Ord, Hashable)
type AttrArgs = Map Text Text
type Fill = AttrArgs -> Template -> Library -> IO Text
type BlankFills = Map Blank Fill
newtype Template = Template { runTemplate :: BlankFills -> Library -> IO Text }
type Library = Map Text Template

need :: Map Blank Fill -> [Blank] -> Text -> Text
need m keys rest =
  let sk = S.fromList keys
      sm = M.keysSet m
      d = S.difference sk sm
  in if S.null d
        then rest
        else error $ "Missing keys: " <> show d

add :: BlankFills -> Template -> Template
add mouter tpl =
  Template (\minner l -> runTemplate tpl (minner `M.union` mouter) l)

text :: Text -> Fill
text t = \_m _t _l -> return t

useAttrs :: (AttrArgs -> Text -> IO Text) -> Fill
useAttrs f = \atrs tpl lib ->
  do childText <- runTemplate tpl mempty lib
     f atrs childText

class FromAttr a where
  readAttr :: Text -> AttrArgs -> a

instance FromAttr Text where
  readAttr attrName attrs = attrs M.! attrName
instance FromAttr Int where
  readAttr attrName attrs = read $ T.unpack (attrs M.! attrName)

a :: FromAttr a => Text -> (a -> b) -> AttrArgs -> b
a attrName k attrs = k (readAttr attrName attrs)

(%) :: (a -> AttrArgs -> b)
    -> (b -> AttrArgs -> c)
    ->  a -> AttrArgs -> c
(%) f1 f2 fun attrs = f2 (f1 fun attrs) attrs

mapFills :: (a -> BlankFills) -> [a] -> Fill
mapFills f xs = \_m tpl lib ->
    T.concat <$>  mapM (\n -> runTemplate tpl (f n) lib) xs

fills :: [(Text, Fill)] -> BlankFills
fills = M.fromList . map (\(x,y) -> (Blank x, y))

fill :: BlankFills -> Fill
fill m = \_m (Template tpl) l -> tpl m l

plainNodes :: HS.HashSet Text
plainNodes = HS.fromList ["html","body","base","head","link","meta","style","title","address","article","aside","footer","header","h1","h2","h3","h4","h5","h6","nav","dd","div","dl","dt","figcaption","figure","hr","li","main","ol","p","pre","ul","a","abbr","b","bdi","bdo","br","cite","code","data","dfn","em","i","kbd","mark","q","rp","rt","rtc","ruby","s","samp","small","span","strong","sub","sup","time","u","var","wbr","area","img", "audio","map","track","video","embed","object","param","source","canvas","noscript","script","del","ins","caption","col","colgroup","table","tbody","td","tfoot","th","thead","tr","button","datalist","fieldset","form","input","label","legend","meter","optgroup","option","output","progress","select","textarea","details","dialog","menu","menuitem","summary","element","shadow","template","command","keygen","nextid","noembed","xmp"]

parse t =
  let (X.Document _ (X.Element _ _ nodes) _) = D.parseLT (LT.fromStrict ("<div>" <> t <> "</div>"))
  in mk nodes

mk :: [X.Node] -> Template
mk nodes = let unbound = findUnbound nodes
           in Template $ \m l ->
                need m (map Blank unbound) <$>
                (T.concat <$> process m l unbound nodes)

fillIn :: Text -> BlankFills -> Fill
fillIn tn m = m M.! Blank tn

process :: BlankFills -> Library -> [Text] -> [X.Node] -> IO [Text]
process _ _ _ [] = return []
process m l unbound (n:ns) = do
  processedNode <-
    case n of
      X.NodeElement (X.Element "apply" atr kids) -> processApply m l atr kids
      X.NodeElement (X.Element tn atr kids) | HS.member (X.nameLocalName tn) plainNodes
                                 -> processPlain m l unbound tn atr kids
      X.NodeElement (X.Element tn atr kids)     -> processFancy m l tn atr kids
      X.NodeContent t               -> return [t]
      X.NodeComment c                -> return ["<!--" <> c <> "-->"]
  restOfNodes <- process m l unbound ns
  return $ processedNode ++ restOfNodes

-- Add the open tag and attributes, process the children, then close
-- the tag.
processPlain :: BlankFills -> Library -> [Text] ->
                 X.Name -> Map X.Name Text -> [X.Node] -> IO [Text]
processPlain m l unbound tn atr kids = do
  atrs <- attrsToText atr
  processed <- process m l unbound kids
  let tagName = X.nameLocalName tn
  return $ ["<" <> tagName <> atrs <> ">"]
           ++ processed
           ++ ["</" <> tagName <> ">"]
  where attrsToText attrs = T.concat <$> mapM (attrToText) (M.toList attrs)
        attrToText :: (X.Name, Text) -> IO Text
        attrToText (k,v) =
          let name = X.nameLocalName k in
          case mUnboundAttr (k,v) of
            Just hole -> do filledIn <- fillIn hole m mempty (mk []) l
                            return $ " " <> name <> "=\"" <> filledIn  <> "\""
            Nothing   -> return $ " " <> name <> "=\"" <> v <> "\""

-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processFancy :: BlankFills -> Library ->
                X.Name -> Map X.Name Text -> [X.Node] -> IO [Text]
processFancy m l tn atr kids =
  let tagName = X.nameLocalName tn in
  sequence [ fillIn tagName m (M.mapKeys X.nameLocalName atr) (add m (mk kids)) l]

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library. Phew.
processApply :: BlankFills -> Library ->
                 Map X.Name Text -> [X.Node] -> IO [Text]
processApply m l atr kids = do
  let tplName = fromMaybe
                (error "No template name given.")
                (M.lookup "template" atr)
  let tplToApply =
        case M.lookup tplName l of
          Just tpl -> tpl
          Nothing  -> error $ T.unpack
                      ("Template \"" <> tplName <> "\" not found")
  contentTpl <- runTemplate (mk kids) m l
  let contentSub = fills [("apply-content",
                        text contentTpl)]
  sequence [ runTemplate tplToApply (contentSub `M.union` m) l ]

findUnbound :: [X.Node] -> [Text]
findUnbound [] = []
findUnbound (X.NodeElement (X.Element tn atr kids):ns) =
     let tagName = X.nameLocalName tn in
     if tn == "apply" || HS.member tagName plainNodes
     then findUnboundAttrs atr ++ findUnbound kids
     else tagName : findUnboundAttrs atr
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
