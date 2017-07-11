{-# LANGUAGE OverloadedStrings          #-}

module Web.Larceny.Internal ( findTemplate
                            , parse
                            , parseWithOverrides) where

import           Control.Exception
import           Lens.Micro
import           Control.Monad       (void)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.State (MonadState, StateT, evalStateT, runStateT, get, modify)
import           Data.Either
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import qualified Text.HTML.DOM       as D
import qualified Text.XML            as X
------------
import           Web.Larceny.Types
import           Web.Larceny.Fills
import           Web.Larceny.Html    (html5Nodes, html5SelfClosingNodes)

-- | Turn lazy text into templates.
parse :: LT.Text -> Template s
parse = parseWithOverrides defaultOverrides

-- | Use overrides when parsing a template.
parseWithOverrides :: Overrides -> LT.Text -> Template s
parseWithOverrides o t =
  let (X.Document _ (X.Element _ _ nodes) _) = D.parseLT ("<div>" <> t <> "</div>")
  in mk o nodes

-- | Turn HTML nodes and overrides into templates.
mk :: Overrides -> [X.Node] -> Template s
mk o = f
  where allPlainNodes = (HS.fromList (customPlainNodes o) `HS.union` html5Nodes)
                        `HS.difference` HS.fromList (overrideNodes o)
        f nodes = let unbound = findUnbound allPlainNodes nodes in
          Template $ \pth m l ->
                      let pc = ProcessContext pth m l o unbound allPlainNodes f nodes in
                      do s <- get
                         need pth m unbound <$>
                           (T.concat <$> liftS (pc s) (process nodes))

liftP :: StateT s IO a -> StateT (ProcessContext s) IO a
liftP f =
  do pc <- get
     (blah, s') <- liftIO $ runStateT f (_pcState pc)
     pcState .= s'
     return blah

liftS :: ProcessContext s -> StateT (ProcessContext s) IO a -> StateT s IO a
liftS pc f =
  do s <- get
     liftIO $ evalStateT f (pc { _pcState = s })

fillIn :: Text -> Substitutions s -> Fill s
fillIn tn m = fromMaybe (textFill "") (M.lookup (Blank tn) m)

data ProcessContext s = ProcessContext { _pcPath          :: Path -- ???
                                       , _pcSubs          :: Substitutions s -- changes
                                       , _pcLib           :: Library s -- stays the same
                                       , _pcOverrides     :: Overrides -- doesn't matter
                                       , _pcUnbound       :: [Blank] -- changes
                                       , _pcAllPlainNodes :: HashSet Text -- doesn't matter
                                       , _pcMk            :: [X.Node] -> Template s -- ???
                                       , _pcNodes         :: [X.Node]
                                       , _pcState         :: s } -- changes }

infix  4 .=
(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

{-
use :: MonadState s m => Getting a s a -> m a
use l = gets ((^.) l)
{-# INLINE use #-}
-}

-- pcPath :: Lens' (ProcessContext s) Path
-- pcPath = lens _pcPath (\pc p -> pc { _pcPath = p })

pcSubs :: Lens' (ProcessContext s) (Substitutions s)
pcSubs = lens _pcSubs (\pc s -> pc { _pcSubs = s })

-- pcLib :: Lens' (ProcessContext s) (Library s)
-- pcLib = lens _pcLib (\pc l -> pc { _pcLib = l })

-- pcOverrides :: Lens' (ProcessContext s) Overrides
-- pcOverrides = lens _pcOverrides (\pc o -> pc { _pcOverrides = o })

-- pcUnbound :: Lens' (ProcessContext s) [Blank]
-- pcUnbound = lens _pcUnbound (\pc u -> pc { _pcUnbound = u })

-- pcAllPlainNodes :: Lens' (ProcessContext s) (HashSet Text)
-- pcAllPlainNodes = lens _pcAllPlainNodes (\pc p -> pc { _pcAllPlainNodes = p })

-- pcMk :: Lens' (ProcessContext s) ([X.Node] -> Template s)
-- pcMk = lens _pcMk (\pc f -> pc { _pcMk = f })

pcNodes :: Lens' (ProcessContext s) [X.Node]
pcNodes  = lens _pcNodes (\pc n -> pc { _pcNodes = n })

pcState :: Lens' (ProcessContext s) s
pcState = lens _pcState (\pc s -> pc { _pcState = s })

type ProcessT s = StateT (ProcessContext s) IO [Text]

need :: Path -> Map Blank (Fill s) -> [Blank] -> Text -> Text
need pth m keys rest =
  let d = S.difference (S.fromList keys) (M.keysSet m)
  in if S.null d
     then rest
     else throw $ MissingBlanks (S.toList d) pth

add :: Substitutions s -> Template s -> Template s
add mouter tpl =
  Template (\pth minner l -> runTemplate tpl pth (minner `M.union` mouter) l)

process :: [X.Node] -> ProcessT s
process [] = return []
process (currentNode:nextNodes) = do
  pc <- get
  pcNodes .= nextNodes
  processedNode <-
    case currentNode of
      X.NodeElement (X.Element "bind" atr kids)  -> do void $ processBind atr kids
                                                       return []
      X.NodeElement (X.Element "apply" atr kids) -> processApply atr kids
      X.NodeElement (X.Element tn atr kids) | HS.member (X.nameLocalName tn) (_pcAllPlainNodes pc)
                                                 -> processPlain tn atr kids
      X.NodeElement (X.Element tn atr kids)      -> processFancy tn atr kids
      X.NodeContent t                            -> return [t]
      X.NodeComment c                            -> return ["<!--" <> c <> "-->"]
      X.NodeInstruction _                        -> return []
  restOfNodes <- process nextNodes
  return $ processedNode ++ restOfNodes

-- Add the open tag and attributes, process the children, then close
-- the tag.
processPlain :: X.Name ->
                Map X.Name Text ->
                [X.Node] ->
                ProcessT s
processPlain tn atr kids = do
  pc <- get
  atrs <- attrsToText atr
  processed <- process kids
  let tagName = X.nameLocalName tn
  return $ tagToText (_pcOverrides pc) tagName atrs processed

selfClosing :: Overrides -> HS.HashSet Text
selfClosing (Overrides _ _ sc) =
  HS.fromList sc <> html5SelfClosingNodes

tagToText :: Overrides
          -> Text
          -> Text
          -> [Text]
          -> [Text]
tagToText overrides tagName atrs processed =
  if tagName `HS.member` selfClosing overrides
  then ["<" <> tagName <> atrs <> "/>"]
  else ["<" <> tagName <> atrs <> ">"]
           ++ processed
           ++ ["</" <> tagName <> ">"]

attrsToText :: Map X.Name Text -> StateT (ProcessContext s) IO Text
attrsToText attrs =
  T.concat <$> mapM attrToText (M.toList attrs)
  where attrToText (k,v) = do
          let (unboundK, unboundV) =  eUnboundAttrs (k,v)
          keys <- T.concat <$> mapM fillAttr unboundK
          vals <- T.concat <$> mapM fillAttr unboundV
          return $ toText (keys, vals)
        toText (k, "") = " " <> k
        toText (k, v) = " " <> k <> "=\"" <> T.strip v <> "\""

fillAttrs :: Map X.Name Text -> StateT (ProcessContext s) IO (Map X.Name Text)
fillAttrs attrs =  M.fromList <$> mapM fill (M.toList attrs)
  where fill p = do
          let (unboundKeys, unboundValues) = eUnboundAttrs p
          keys <- T.concat <$> mapM fillAttr unboundKeys
          vals <- T.concat <$> mapM fillAttr unboundValues
          return (X.Name keys Nothing Nothing, vals)

fillAttr :: Either Text Blank -> StateT (ProcessContext s) IO Text
fillAttr eBlankText =
  do (ProcessContext _ m l _ _ _ mko _ _) <- get
     liftP $
       case eBlankText of
         Right (Blank hole) -> unFill (fillIn hole m) mempty ([], mko []) l
         Left text -> return text


-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processFancy :: X.Name ->
                Map X.Name Text ->
                [X.Node] ->
                ProcessT s
processFancy tn atr kids = do
  (ProcessContext pth m l _ _ _ mko _ _) <- get
  let tagName = X.nameLocalName tn
  filled <- fillAttrs atr
  sequence [ liftP $ unFill (fillIn tagName m)
                    (M.mapKeys X.nameLocalName filled)
                    (pth, add m (mko kids)) l]

processBind :: Map X.Name Text ->
               [X.Node] ->
               ProcessT s
processBind atr kids = do
  (ProcessContext pth m l _ _ _ mko nodes _) <- get
  let tagName = atr M.! "tag" -- Use the AttrMissing exception
      newSubs = subs [(tagName, Fill $ \_a _t _l ->
                                       runTemplate (mko kids) pth m l)]
  pcSubs .= newSubs `M.union` m
  process nodes

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library.
processApply :: Map X.Name Text ->
                [X.Node] ->
                ProcessT s
processApply atr kids = do
  (ProcessContext pth m l _ _ _ mko _ _) <- get
  filledAttrs <- fillAttrs atr
  let (absolutePath, tplToApply) = findTemplateFromAttrs pth l filledAttrs
  contentTpl <- liftP $ runTemplate (mko kids) pth m l
  let contentSub = subs [("apply-content",
                         rawTextFill contentTpl)]
  sequence [ liftP $ runTemplate tplToApply absolutePath (contentSub `M.union` m) l ]

findTemplateFromAttrs :: Path ->
                         Library s ->
                         Map X.Name Text ->
                         (Path, Template s)
findTemplateFromAttrs pth l atr =
  let tplPath = T.splitOn "/" $ fromMaybe (throw $ AttrMissing "template")
                                          (M.lookup "template" atr) in
  case findTemplate l (init pth) tplPath of
    (_, Nothing) -> throw $ ApplyError tplPath pth
    (targetPath, Just tpl) -> (targetPath, tpl)

findTemplate :: Library s -> Path -> Path -> (Path, Maybe (Template s))
findTemplate lib [] targetPath = (targetPath, M.lookup targetPath lib)
findTemplate lib pth' targetPath =
  case M.lookup (pth' ++ targetPath) lib of
    Just tpl -> (pth' ++ targetPath, Just tpl)
    Nothing  -> findTemplate lib (init pth') targetPath

findUnbound :: HashSet Text -> [X.Node] -> [Blank]
findUnbound _ [] = []
findUnbound plainNodes (X.NodeElement (X.Element name atr kids):ns) =
     let tn = X.nameLocalName name in
     if tn == "apply" || tn == "bind" || HS.member tn plainNodes
     then findUnboundAttrs atr ++ findUnbound plainNodes kids
     else Blank tn : findUnboundAttrs atr ++ findUnbound plainNodes ns
findUnbound plainNodes (_:ns) = findUnbound plainNodes ns

findUnboundAttrs :: Map X.Name Text -> [Blank]
findUnboundAttrs atrs =
  rights $ concatMap (uncurry (<>) . eUnboundAttrs) (M.toList atrs)

eUnboundAttrs :: (X.Name, Text) -> ([Either Text Blank], [Either Text Blank])
eUnboundAttrs (X.Name n _ _, value) = do
  let possibleWords = T.splitOn "${"
  let mWord w =
        case T.splitOn "}" w of
          [_] -> [Left w]
          ["",_] -> [Left ("${" <> w)]
          (word: rest) -> Right (Blank word) : map Left rest
          _ -> [Left w]
  ( concatMap mWord (possibleWords n)
    , concatMap mWord (possibleWords value))


{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use first" :: String) #-}