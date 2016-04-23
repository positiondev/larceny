{-# LANGUAGE OverloadedStrings #-}
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Monoid        ((<>))
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Text.XmlHtml       as X

newtype Hole = Hole Text deriving (Eq, Show, Ord)
newtype Template = Template { runTemplate :: Map Hole Substitution -> Text }
type Substitution = Template -> Text

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

add :: Map Hole Substitution -> Template -> Template
add mouter tpl = Template (\minner -> runTemplate tpl (minner `M.union` mouter))

page :: Template
page = Template $ \m -> need m [Hole "site-title", Hole "people"] $
                        T.concat ["<body>"
                                 , m M.! (Hole "site-title") $ Template (const "")
                                 , m M.! (Hole "people") $ (add m peopleBody)
                                 , "</body>"
                                 ]
  where peopleBody :: Template
        peopleBody = Template $ \m -> need m [Hole "name", Hole "site-title"] $
                                      T.concat ["<p>"
                                               , m M.! (Hole "name") $ Template (const "")
                                               , "</p>"
                                               , m M.! (Hole "site-title") $ Template (const "")
                                               ]

-- to use
subst :: Map Hole Substitution
subst = (M.fromList [ (Hole "site-title", const "My site")
                    , (Hole "people",
                       \tpl -> T.concat $ map (\n -> runTemplate tpl (M.fromList [(Hole "name", const n)])) ["Daniel", "Matt", "Cassie", "Libby"])])


render = runTemplate page subst


plainNodes = ["body", "p", "h1"]

parse :: Text -> Template
parse t = let Right (X.HtmlDocument _ _ nodes) = X.parseHTML "" (T.encodeUtf8 t)
          in mk nodes
  where mk nodes = let unbound = findUnbound nodes
          in Template $ \m -> need m (map Hole unbound) (T.concat $ process m unbound nodes)
        process m unbound [] = []
        process m unbound (n:ns) =
          case n of
            X.Element tn _ kids ->
              if tn `elem` plainNodes
              then ["<" <> tn <> ">"] ++ process m unbound kids ++ ["</" <> tn <> ">"]
              else [m M.! (Hole tn) $ add m (mk kids)]
            X.TextNode t ->  [t]
            X.Comment c -> ["<!--" <> c <> "-->"]
          ++ process m unbound ns
        findUnbound [] = []
        findUnbound (n:ns) =
          case n of
            X.Element tn _ kids ->
              if X.elementTag n `elem` plainNodes
                then findUnbound (X.elementChildren n)
                else [X.elementTag n]
            _ -> []
          ++ findUnbound ns

page' = parse "<body>\
              \ <site-title/>\
              \ <people>\
              \   <p><name/></p>\
              \   <site-title/>\
              \ </people>\
              \</body>"

render' = runTemplate page' subst


-- apply?
