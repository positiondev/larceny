{-# LANGUAGE OverloadedStrings #-}

module Larceny where

import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Data.Monoid        ((<>))
--import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
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
        else error $ "Missing keys: " <> show d

add :: Substitution -> Template -> Template
add mouter tpl = Template (\minner l -> runTemplate tpl (minner `M.union` mouter) l)

-- works to create both Templates and Fills?
text :: Text -> Fill
text t = \_ _ -> t

mapSub :: (a -> Substitution) -> [a] -> Fill
mapSub f xs = \tpl lib ->
  T.concat $
  map (\n ->
        runTemplate tpl (f n) lib) xs

funkyTpl :: Text -> (Text -> Text) -> Template
funkyTpl argName f = Template
  (\m l ->
    let argText = (m M.! Hole argName) (mk []) l in
    need m [Hole argName] $  f argText)

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
  case n of
    X.Element "apply" atr kids -> processApply m l atr kids
    X.Element tn atr kids       | tn `elem` plainNodes ->
                                  processPlain m l unbound tn atr kids
    X.Element tn atr kids      -> processFancy m l tn atr kids
    X.TextNode t -> [t]
    X.Comment c -> ["<!--" <> c <> "-->"]
  ++ process m l unbound ns

-- Add the open tag, process the children, then close the tag.
processPlain :: Substitution -> Library -> [Text] ->
                Text -> [(Text, Text)] -> [X.Node] -> [Text]
processPlain m l unbound tn atr kids =
  ["<" <> tn <> attrsToText m l atr <> ">"]
  ++ process m l unbound kids
  ++ ["</" <> tn <> ">"]

-- Look up the Fill for the hole.  Apply the Fill to a Template made
-- from the child nodes (adding in the outer substituion) and the
-- library.
processFancy :: Substitution -> Library ->
                Text -> [(Text, Text)] -> [X.Node] -> [Text]
processFancy m l tn atr kids =
  -- add attrs as substitutions
  let attrSubs = sub $ map (\(k,v) -> (k, text v)) atr in
  [ fillIn tn m (add (attrSubs `M.union` m) (mk kids)) l]

fillIn :: Text -> Substitution -> Fill
fillIn tn m = m M.! Hole tn

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library. Phew.
processApply :: Substitution -> Library ->
                [(Text, Text)] -> [X.Node] -> [Text]
processApply m l atr kids =
  let tplName = fromMaybe
                (error "Template to apply not found")
                (lookup "name" atr)
      tplToApply = l M.! tplName
      attrSubs = sub $ map (\(k,v) -> (k, text v)) atr
      contentSub = sub [("content",
                         \_ l' -> runTemplate (mk kids) m l')] in
  [ runTemplate tplToApply (contentSub `M.union` (attrSubs `M.union` m)) l ]

attrsToText :: Substitution -> Library -> [(Text, Text)] -> Text
attrsToText m l attrs= T.concat $ map attrToText attrs
  where
    attrToText atr =
      case mUnboundAttr atr of
       Just hole -> " " <> fst atr <> "=\"" <>
                    fillIn hole m (mk []) l  <> "\""
       Nothing   -> " " <> fst atr <> "=\"" <> snd atr <> "\""

findUnbound :: [X.Node] -> [Text]
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

findUnboundAttrs :: [(Text, Text)] -> [Text]
findUnboundAttrs = mapMaybe mUnboundAttr

mUnboundAttr :: (Text, Text) -> Maybe Text
mUnboundAttr (_, value) = do
  endVal <- T.stripPrefix "${" value
  T.stripSuffix "}" endVal
