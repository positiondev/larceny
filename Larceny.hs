{-# LANGUAGE OverloadedStrings #-}
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Monoid        ((<>))
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Test.Hspec
import qualified Text.XmlHtml       as X

type Library = Map Text Template

newtype Hole = Hole Text deriving (Eq, Show, Ord)
newtype Template = Template { runTemplate :: Substitution -> Library -> Text }
type Fill = Template -> Library -> Text
type Substitution = Map Hole Fill



-- apply?

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

text s = \ x y -> s

page :: Template
page = Template $ \m l -> need m [Hole "site-title", Hole "people"] $
                          T.concat ["<body>"
                                 , (m M.! (Hole "site-title")) (Template (text "")) l
                                 , (m M.! (Hole "people")) (add m peopleBody) l
                                 , "</body>"
                                 ]
  where peopleBody :: Template
        peopleBody = Template $ \m l -> need m [Hole "name", Hole "site-title"] $
                                      T.concat ["<p>"
                                               , (m M.! (Hole "name")) (Template (text "")) l
                                               , "</p>"
                                               , (m M.! (Hole "site-title")) (Template (text "")) l
                                               ]

-- to use
subst :: Substitution
subst = (M.fromList [ (Hole "site-title", text "My site")
                    , (Hole "people",
                       \tpl l -> T.concat $ map (\n -> runTemplate tpl (M.fromList [(Hole "name", text n)]) l) ["Daniel", "Matt", "Cassie", "Libby"])])

subst' = sub [ ("name", text "My site")
             , ("person", fill $ sub [("name", text "Daniel")])]

sub :: [(Text, Fill)] -> Substitution
sub = M.fromList . map (\(x,y) -> (Hole x, y))

fill :: Substitution -> Fill
fill s = \(Template tpl) -> tpl s

render = runTemplate page subst



plainNodes = ["body", "p", "h1"]

parse :: Text -> Template
parse t = let Right (X.HtmlDocument _ _ nodes) = X.parseHTML "" (T.encodeUtf8 t)
          in mk nodes
  where mk nodes = let unbound = findUnbound nodes
          in Template $ \m l -> need m (map Hole unbound) (T.concat $ process m l unbound nodes)
        process m l unbound [] = []
        process m l unbound (n:ns) =
          case n of
            X.Element tn _ kids ->
              if tn `elem` plainNodes
              then ["<" <> tn <> ">"] ++ process m l unbound kids ++ ["</" <> tn <> ">"]
              else [(m M.! (Hole tn)) (add m (mk kids)) l]
            X.TextNode t ->  [t]
            X.Comment c -> ["<!--" <> c <> "-->"]
          ++ process m l unbound ns
        findUnbound [] = []
        findUnbound (n:ns) =
          case n of
            X.Element tn _ kids ->
              if X.elementTag n `elem` plainNodes
                then findUnbound (X.elementChildren n)
                else [X.elementTag n]
            _ -> []
          ++ findUnbound ns

page' = "<body>\
         \ <site-title/>\
              \ <people>\
              \   <p><name/></p>\
              \   <site-title/>\
              \ </people>\
              \</body>"

page'' = "<body>\
              \ My site\
              \   <p>Daniel</p>\
              \   My site\
              \   <p>Matt</p>\
              \   My site\
              \   <p>Cassie</p>\
              \   My site\
              \   <p>Libby</p>\
              \   My site\
              \</body>"

render' = runTemplate (parse page') subst



shouldRender :: (Text, Substitution, Library) -> Text -> Expectation
shouldRender (template, subst, lib) output =
  T.replace " " "" (runTemplate (parse template) subst lib) `shouldBe`
  T.replace " " "" output

spec = hspec $ do
  describe "parse" $ do
    it "should parse HTML into a Template" $ do
      (page', subst, mempty) `shouldRender` page''
  describe "add" $ do
    it "should allow overriden tags" $ do
      ("<name /><person><name /></person>", subst', mempty) `shouldRender` "My siteDaniel"
  describe "apply" $ do
    it "should allow templates to be included in other templates" $ do
      ("<apply name=\"hello\" />", mempty, M.fromList [("hello", parse "hello")]) `shouldRender` "hello"
    it "should allow templates with unfilled holes to be included in other templates" $ do
      ("<apply name=\"person\" />", sub [("name", text "Daniel")], M.fromList [("person", parse "<name />")]) `shouldRender` "Daniel"
    it "should allow templates to be included in other templates" $ do
      ("<apply name=\"person\">Libby</apply>", mempty, M.fromList [("person", parse "<content />")]) `shouldRender` "Libby"
