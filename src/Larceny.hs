{-# LANGUAGE OverloadedStrings #-}

module Larceny where

import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (catMaybes, fromJust, isJust)
import           Data.Monoid        ((<>))
--import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Debug.Trace        (trace)
import qualified Text.XmlHtml       as X

newtype Hole = Hole Text deriving (Eq, Show, Ord)
type Fill = Template -> Library -> Text
type Substitution = Map Hole Fill
newtype Template = Template { runTemplate :: Substitution -> Library -> Text }
type Library = Map Text Template



-- what about overriding whitelisted tags? like <title/>

-- page' = parse "<body><head><title>Great site!</title></head>\
--               \ <site-title/>\
--               \ <people>\
--               \   <p><name/></p>\
--               \   <site-title/>\
--               \ </people>\
--               \</body>"

need :: Map Hole a -> [Hole] -> c -> c
need m keys rest =
  let sk = S.fromList keys
      sm = M.keysSet m
      d = S.difference sk sm
  in if S.null d
        then rest
        else error $ "Missing keys: " <> (show d)

add :: Substitution -> Template -> Template
add mouter tpl = Template (\minner l -> runTemplate tpl (minner `M.union` mouter) l)

-- works to create both Templates and Fills?
text :: Text -> a -> Library -> Text
text t = \_ _ -> t

mapSub :: (a -> Substitution) -> [a] -> Fill
mapSub f xs = \tpl lib ->
  T.concat $
  map (\n ->
        runTemplate tpl (f n) lib) xs

sub :: [(Text, Fill)] -> Substitution
sub = M.fromList . map (\(x,y) -> (Hole x, y))

fill :: Substitution -> Fill
fill s = \(Template tpl) -> tpl s

specialNodes :: [Text]
specialNodes = ["apply"]

plainNodes :: [Text]
plainNodes = ["body", "p", "h1", "img"]

parse :: Text -> Template
parse t = let Right (X.HtmlDocument _ _ nodes) = X.parseHTML "" (T.encodeUtf8 t)
          in mk nodes

mk :: [X.Node] -> Template
mk nodes = let unbound = findUnbound nodes
           in Template $ \m l -> need m (map Hole unbound) (T.concat $ process m l unbound nodes)

process :: Substitution -> Library -> [Text] -> [X.Node] -> [Text]
process _ _ _ [] = []
process m l unbound (n:ns) =
  case nodeType n of
   Plain -> processPlain m l unbound n
   Fancy -> trace "Fancy" $ processFancy m l unbound n
   Apply -> processApply m l unbound n
   TextNode -> (\(X.TextNode t) -> [(t)]) n
   Comment -> (\(X.Comment c) -> ["<!--" <> c <> "-->"]) n
   ++ process m l unbound ns

data NodeType = Apply | Plain | Fancy | TextNode | Comment

nodeType :: X.Node -> NodeType
nodeType (X.TextNode _) = TextNode
nodeType (X.Comment _) = Comment
nodeType (X.Element "apply" _ _) = Apply
nodeType (X.Element tn atr kids) =
  if tn `elem` plainNodes
  then Plain
  else Fancy

-- Add the open tag, process the children, then close the tag.
processPlain :: Substitution -> Library -> [Text] -> X.Node -> [Text]
processPlain m l unbound (X.Element tn atr kids) =
  ["<" <> tn <> attrsToText m l atr <> ">"]
  ++ process m l unbound kids
  ++ ["</" <> tn <> ">"]

-- Look up the Fill for the hole.  Apply the Fill to a Template made
-- from the child nodes, adding in the outer substituion, and the
-- library.
processFancy :: Substitution -> Library -> [Text] -> X.Node -> [Text]
processFancy m l unbound (X.Element tn atr kids) =
  -- add attrs as substitutions
  let attrSubs = sub $ map attrSub atr
      attrSub (key, val) = (key, text val)
      lookupFill =
        case M.lookup (Hole tn) m of
          Just x -> x
          Nothing -> text "whoops" in
  trace ("Tag: " ++ show tn) $ [(m M.! (Hole tn)) (add (attrSubs `M.union` m) (mk kids)) l]

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library. Phew.
processApply :: Substitution -> Library -> [Text] -> X.Node -> [Text]
processApply m l unbound (X.Element tn atr kids) =
  let tplName =
        case lookup "name" atr of
          Just n -> n
          Nothing -> error "Template to apply not found"
      tplToApply = l M.! tplName
      contentSub = sub [("content",
                         (\_ l' -> runTemplate (mk kids) m l'))] in
  [ runTemplate tplToApply (contentSub `M.union` m) l ]

attrsToText :: Substitution -> Library -> [(Text, Text)] -> Text
attrsToText m l attrs= T.concat $ map (attrToText m l) attrs
  where
    attrToText m l atr =
      case mUnboundAttr atr of
       Just hole -> " " <> fst atr <> "=\"" <>
                    ((m M.! (Hole hole)) (mk []) l)  <> "\""
       Nothing   -> " " <> fst atr <> "=\"" <> snd atr <> "\""

--findUnbound :: [X.Node] -> [Text]
findUnbound [] = []
findUnbound (n:ns) =
  case n of
   X.Element _ atr _ ->
     if X.elementTag n `elem` plainNodes || X.elementTag n `elem` specialNodes
     then findUnboundAttrs atr ++ findUnbound (X.elementChildren n)
     else X.elementTag n : findUnboundAttrs atr
   _ -> []
   ++ findUnbound ns
   where

--findUnboundAttrs :: [(Text, Text)] -> [Text]
findUnboundAttrs atrs = catMaybes $ map mUnboundAttr atrs

--mUnboundAttr :: (Text, Text) -> Maybe Text
mUnboundAttr (_, value) = do
  endVal <- T.stripPrefix "${" value
  T.stripSuffix "}" endVal
