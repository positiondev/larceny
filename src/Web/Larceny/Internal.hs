{-# LANGUAGE OverloadedStrings          #-}

module Web.Larceny.Internal ( findTemplate
                            , parse
                            , parseWithOverrides) where

import           Control.Exception
import           Lens.Micro
import           Control.Monad.Trans (liftIO)
import           Control.Monad.State (MonadState, StateT, evalStateT, runStateT, get, modify)
import qualified Data.HashSet        as HS
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
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
  in mk o $! map (toLarcenyNode o) nodes

data Node = NodeElement Element
          | NodeContent Text
          | NodeComment Text

data Element = PlainElement Text Attributes [Node]
             | ApplyElement Attributes [Node]
             | BindElement Attributes [Node]
             | BlankElement Name Attributes [Node]

toLarcenyName :: X.Name -> Name
toLarcenyName (X.Name tn _ _) =
  case T.splitOn ":" tn of
    (ns:name:_) -> Name (Just ns) name
    (name: _ )  -> Name Nothing name
    []          -> Name Nothing ""

toLarcenyNode :: Overrides -> X.Node -> Node
toLarcenyNode o (X.NodeElement (X.Element tn atr nodes)) =
  let larcenyNodes = map (toLarcenyNode o) nodes
      attrs = M.mapKeys toLarcenyName atr
      allPlainNodes = (HS.fromList (customPlainNodes o) `HS.union` html5Nodes)
                             `HS.difference` HS.fromList (overrideNodes o)in
  case toLarcenyName tn of

    -- these are our special larceny elements
    Name Nothing "bind" ->
      NodeElement (BindElement attrs larcenyNodes)
    Name Nothing "apply" ->
      NodeElement (ApplyElement attrs larcenyNodes)
    Name Nothing "apply-content" ->
      NodeElement (BlankElement (Name Nothing "apply-content") attrs larcenyNodes)

    -- these are the blank and plain elements
    -- if there's a namespace, it's definitely a Blank
    -- if there's not a namespace, and the tag is a member of the set of plain nodes, it's not a Blank
    -- otherwise, it's a blank
    Name (Just ns) name ->
      NodeElement (BlankElement (Name (Just ns) name) attrs larcenyNodes)
    Name Nothing name | HS.member name allPlainNodes ->
      NodeElement (PlainElement name attrs larcenyNodes)
    Name Nothing name ->
      NodeElement (BlankElement (Name Nothing name) attrs larcenyNodes)
toLarcenyNode _ (X.NodeContent c)  = NodeContent c
toLarcenyNode _ (X.NodeComment c) = NodeComment c
toLarcenyNode _ (X.NodeInstruction _) = NodeContent ""

-- | Turn HTML nodes and overrides into templates.
mk :: Overrides -> [Node] -> Template s
mk o = f
  where f nodes =
          Template $ \pth m l ->
                      let pc = ProcessContext pth m l o f nodes in
                      do s <- get
                         T.concat <$> toUserState (pc s) (process nodes)

toProcessState :: StateT s IO a -> StateT (ProcessContext s) IO a
toProcessState f =
  do pc <- get
     (result, s') <- liftIO $ runStateT f (_pcState pc)
     pcState .= s'
     return result

toUserState :: ProcessContext s -> StateT (ProcessContext s) IO a -> StateT s IO a
toUserState pc f =
  do s <- get
     liftIO $ evalStateT f (pc { _pcState = s })

fillIn :: Blank -> Substitutions s -> Fill s
fillIn tn m = fromMaybe (fallbackFill tn m) (M.lookup tn m)

fallbackFill :: Blank -> Substitutions s -> Fill s
fallbackFill FallbackBlank m =  fromMaybe (textFill "") (M.lookup FallbackBlank m)
fallbackFill (Blank tn) m =
  let fallback = fromMaybe (textFill "") (M.lookup FallbackBlank m) in
  Fill $ \attr (pth, tpl) lib ->
    do liftIO $ putStrLn ("Larceny: Missing fill for blank " <> show tn <> " in template " <> show pth)
       unFill fallback attr (pth, tpl) lib

data ProcessContext s = ProcessContext { _pcPath          :: Path
                                       , _pcSubs          :: Substitutions s
                                       , _pcLib           :: Library s
                                       , _pcOverrides     :: Overrides
                                       , _pcMk            :: [Node] -> Template s
                                       , _pcNodes         :: [Node]
                                       , _pcState         :: s }

infix  4 .=
(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

pcSubs :: Lens' (ProcessContext s) (Substitutions s)
pcSubs = lens _pcSubs (\pc s -> pc { _pcSubs = s })

pcNodes :: Lens' (ProcessContext s) [Node]
pcNodes  = lens _pcNodes (\pc n -> pc { _pcNodes = n })

pcState :: Lens' (ProcessContext s) s
pcState = lens _pcState (\pc s -> pc { _pcState = s })

type ProcessT s = StateT (ProcessContext s) IO [Text]

add :: Substitutions s -> Template s -> Template s
add mouter tpl =
  Template (\pth minner l -> runTemplate tpl pth (minner `M.union` mouter) l)

process :: [Node] -> ProcessT s
process [] = return []
process (NodeElement (BindElement atr kids):nextNodes) = do
  pcNodes .= nextNodes
  processBind atr kids
process (currentNode:nextNodes) = do
  pcNodes .= nextNodes
  processedNode <-
    case currentNode of
      NodeElement (ApplyElement atr kids) ->
          processApply atr kids
      NodeElement (PlainElement tn atr kids) ->
          processPlain tn atr kids
      NodeElement (BlankElement (Name Nothing name) atr kids) ->
          processBlank name atr kids
      NodeElement (BlankElement (Name (Just _) name) atr kids) ->
          processBlank name atr kids
      NodeContent t ->
          return [t]
      NodeComment c ->
          return ["<!--" <> c <> "-->"]
      _ ->
          return []
  restOfNodes <- process nextNodes
  return $ processedNode ++ restOfNodes

-- Add the open tag and attributes, process the children, then close
-- the tag.
processPlain :: Text ->
                Map Name Text ->
                [Node] ->
                ProcessT s
processPlain tagName atr kids = do
  pc <- get
  atrs <- attrsToText atr
  processed <- process kids
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

attrsToText :: Map Name Text -> StateT (ProcessContext s) IO Text
attrsToText attrs =
  T.concat <$> mapM attrToText (M.toList attrs)
  where attrToText (k,v) = do
          let (unboundK, unboundV) =  eUnboundAttrs (k,v)
          keys <- T.concat <$> mapM fillAttr unboundK
          vals <- T.concat <$> mapM fillAttr unboundV
          return $ toText (keys, vals)
        toText (k, "") = " " <> k
        toText (k, v) = " " <> k <> "=\"" <> T.strip v <> "\""

fillAttrs :: Map Name Text -> StateT (ProcessContext s) IO (Map Name Text)
fillAttrs attrs =  M.fromList <$> mapM fill (M.toList attrs)
  where fill p = do
          let (unboundKeys, unboundValues) = eUnboundAttrs p
          keys <- T.concat <$> mapM fillAttr unboundKeys
          vals <- T.concat <$> mapM fillAttr unboundValues
          return (Name Nothing keys, vals)

fillAttr :: Either Text Blank -> StateT (ProcessContext s) IO Text
fillAttr eBlankText =
  do (ProcessContext pth m l _ mko _ _) <- get
     toProcessState $
       case eBlankText of
         Right hole -> unFill (fillIn hole m) mempty (pth, mko []) l
         Left text -> return text

-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processBlank :: Text ->
                Map Name Text ->
                [Node] ->
                ProcessT s
processBlank tagName atr kids = do
  (ProcessContext pth m l _ mko _ _) <- get
  filled <- fillAttrs atr
  sequence [ toProcessState $ unFill (fillIn (Blank tagName) m)
                    filled
                    (pth, add m (mko kids)) l]

processBind :: Map Name Text ->
               [Node] ->
               ProcessT s
processBind atr kids = do
  (ProcessContext pth m l _ mko nodes _) <- get
  let tagName = atr M.! (Name Nothing "tag")
      newSubs = subs [(tagName, Fill $ \_a _t _l ->
                                       runTemplate (mko kids) pth m l)]
  pcSubs .= newSubs `M.union` m
  process nodes

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library.
processApply :: Map Name Text ->
                [Node] ->
                ProcessT s
processApply atr kids = do
  (ProcessContext pth m l _ mko _ _) <- get
  filledAttrs <- fillAttrs atr
  let (absolutePath, tplToApply) = findTemplateFromAttrs pth l filledAttrs
  contentTpl <- toProcessState $ runTemplate (mko kids) pth m l
  let contentSub = subs [("apply-content",
                         rawTextFill contentTpl)]
  sequence [ toProcessState $ runTemplate tplToApply absolutePath (contentSub `M.union` m) l ]

findTemplateFromAttrs :: Path ->
                         Library s ->
                         Map Name Text ->
                         (Path, Template s)
findTemplateFromAttrs pth l atr =
  let tplPath = T.splitOn "/" $ fromMaybe (throw $ AttrMissing "template")
                                          (M.lookup (Name Nothing "template") atr) in
  case findTemplate l (init pth) tplPath of
    (_, Nothing) -> throw $ ApplyError tplPath pth
    (targetPath, Just tpl) -> (targetPath, tpl)

findTemplate :: Library s -> Path -> Path -> (Path, Maybe (Template s))
findTemplate lib [] targetPath = (targetPath, M.lookup targetPath lib)
findTemplate lib pth' targetPath =
  case M.lookup (pth' ++ targetPath) lib of
    Just tpl -> (pth' ++ targetPath, Just tpl)
    Nothing  -> findTemplate lib (init pth') targetPath

eUnboundAttrs :: (Name, Text) -> ([Either Text Blank], [Either Text Blank])
eUnboundAttrs (Name _ n, value) = do
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
