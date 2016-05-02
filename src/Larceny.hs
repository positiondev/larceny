{-# LANGUAGE OverloadedStrings #-}

module Larceny where

import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Data.Monoid        ((<>))
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Debug.Trace        (trace)
import qualified Text.XmlHtml       as X

newtype Hole = Hole Text deriving (Eq, Show, Ord)
type Fill = Template -> Library -> Text
newtype Substitution = Substitution { findFill :: Map Hole (Substitution -> Fill) }
newtype Template = Template { runTemplate :: Substitution -> Library -> Text }
type Library = Map Text Template

need :: Map Hole a -> [Hole] -> c -> c
need m keys rest =
  let sk = S.fromList keys
      sm = M.keysSet m
      d = S.difference sk sm
  in if S.null d
        then rest
        else error $ "Missing keys: " <> show d

union :: Substitution -> Substitution -> Substitution
union (Substitution s1) (Substitution s2) = Substitution (s1 `M.union` s2)

sempty :: Substitution
sempty = Substitution mempty

add :: Substitution -> Template -> Template
add mouter tpl = Template (\minner l -> runTemplate tpl (minner `union` mouter) l)

text :: Text -> Substitution -> Fill
text t = \_ _ _ -> t

mapSub :: (a -> Substitution) -> [a] -> (Substitution -> Fill)
mapSub f xs = \_ tpl lib ->
  T.concat $
  map (\n ->
        runTemplate tpl (f n) lib) xs

funkyFill :: Text -> (Text -> Text) -> (Substitution -> Fill)
funkyFill argName f = trace (show $ argName) $
  (\m _ l ->
    let argText = fillIn argName m m (mk []) l in
    f argText)

funkyTpl :: Text -> (Text -> Text) -> Template
funkyTpl argName f = Template
  (\m@(Substitution s) l ->
    let argText = fillIn argName m m (mk []) l in
    need s [Hole argName] $  f argText)

sub :: [(Text, Substitution -> Fill)] -> Substitution
sub xs = Substitution $ M.fromList (map (\(x,y) -> (Hole x, y)) xs)

fill :: Substitution -> (Substitution -> Fill)
fill s = \s' (Template tpl) -> tpl (s `union` s')

specialNodes :: [Text]
specialNodes = ["apply"]

plainNodes :: [Text]
plainNodes = ["body", "p", "h1", "img"]

parse :: Text -> Template
parse t = let Right (X.HtmlDocument _ _ nodes) = X.parseHTML "" (T.encodeUtf8 t)
          in mk nodes

mk :: [X.Node] -> Template
mk nodes = let unbound = findUnbound nodes
           in Template $ \m@(Substitution s) l ->
                          need s (map Hole unbound)
                                 (T.concat $ process m l unbound nodes)

process :: Substitution -> Library -> [Text] -> [X.Node] -> [Text]
process _ _ _ [] = []
process m l unbound (n:ns) =
  case n of
    X.Element "apply" atr kids -> processApply m l atr kids
    X.Element tn atr kids | tn `elem` plainNodes ->
                                  processPlain m l unbound tn atr kids
    X.Element tn atr kids      -> processFancy m l tn atr kids
    X.TextNode t               -> [t]
    X.Comment c                -> ["<!--" <> c <> "-->"]
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
  [ fillIn tn (attrSubs `union` m) (attrSubs `union` m) (add (attrSubs `union` m) (mk kids)) l]

fillIn :: Text -> Substitution -> Substitution -> Fill
fillIn tn (Substitution m) = m M.! Hole tn

-- Look up the template that's supposed to be applied in the library,
-- add all the attributes as text substitutions, create a substitution
-- for the content hole using the child elements of the apply tag,
-- then run the template with that substitution combined with outer
-- substitution and the library. Phew.
processApply :: Substitution -> Library ->
                [(Text, Text)] -> [X.Node] -> [Text]
processApply m l atr kids =
  let tplName = fromMaybe
                (error "Template to apply not found")
                (lookup "name" atr)
      tplToApply = l M.! tplName
      attrSubs = sub $ map (\(k,v) -> (k, text v)) atr
      contentSub = sub [("content",
                         \_ _ l' -> runTemplate (mk kids) m l')] in
  [ runTemplate tplToApply (contentSub `union` (attrSubs `union` m)) l ]

attrsToText :: Substitution -> Library -> [(Text, Text)] -> Text
attrsToText m l attrs= T.concat $ map attrToText attrs
  where
    attrToText atr =
      case mUnboundAttr atr of
       Just hole -> " " <> fst atr <> "=\"" <>
                    fillIn hole m m (mk []) l  <> "\""
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
