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

page :: Template
page = Template $ \m l -> need m [Hole "site-title", Hole "people"] $
                          T.concat ["<body>"
                                 , (m M.! (Hole "site-title")) (Template $ text "") l
                                 , (m M.! (Hole "people")) (add m peopleBody) l
                                 , "</body>"
                                 ]
  where peopleBody :: Template
        peopleBody = Template $ \m l -> need m [Hole "name", Hole "site-title"] $
                                      T.concat ["<p>"
                                               , (m M.! (Hole "name")) (Template $ text "") l
                                               , "</p>"
                                               , (m M.! (Hole "site-title")) (Template $ text "") l
                                               ]



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
   X.Element tn atr kids ->
     if tn `elem` plainNodes
     then ["<" <> tn <> attrsToText atr <> ">"] ++ process m l unbound kids ++ ["</" <> tn <> ">"]
     else
       if tn == "apply" && isJust (lookup "name" atr)
       then
         let tplToApply = l M.! (fromJust (lookup "name" atr))
             contentSub = sub [("content", (\_ l' -> runTemplate (mk kids) m l'))] in
         [ runTemplate tplToApply (contentSub `M.union` m) l ]
       else [(m M.! (Hole tn)) (add m (mk kids)) l]
   X.TextNode t ->  [t]
   X.Comment c -> ["<!--" <> c <> "-->"]
   ++ process m l unbound ns
  where attrsToText attrs = T.concat $ map attrToText attrs
        attrToText :: (Text, Text) -> Text
        attrToText atr =
          case mUnboundAttr atr of
            Just hole -> " " <> fst atr <> "=\"" <> ((m M.! (Hole hole)) (mk []) l)  <> "\""
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
findUnboundAttrs atrs = catMaybes $ map mUnboundAttr atrs

mUnboundAttr :: (Text, Text) -> Maybe Text
mUnboundAttr (_, value) = do
  endVal <- T.stripPrefix "${" value
  T.stripSuffix "}" endVal
