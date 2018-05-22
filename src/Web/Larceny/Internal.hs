{-# LANGUAGE OverloadedStrings #-}

module Web.Larceny.Internal ( findTemplate
                            , parse
                            , parseWithOverrides) where

import           Control.Exception
import           Control.Monad.State (MonadState, StateT, evalStateT, get,
                                      modify, put, runStateT)
import           Control.Monad.Trans (liftIO)
import qualified Data.HashSet        as HS
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as LT
import           Lens.Micro
import qualified Text.HTML.DOM       as D
import qualified Text.XML            as X
------------
import           Web.Larceny.Fills
import           Web.Larceny.Html    (html5Nodes, html5SelfClosingNodes)
import           Web.Larceny.Svg     (svgNodes)
import           Web.Larceny.Types

-- | Phases of the template parsing/rendering process: 1. Parse the document
--     into HTML (or really, XML) nodes 2. Turn those nodes into Larceny nodes,
--     which encodes more information about the elements, including prefix and
--     whether the node is a regular HTML node, a special Larceny element, or a
--     Larceny blank. 3. Render each node into Text according to its node type.

data Node = NodeElement Element
          | NodeContent Text
          | NodeComment Text

data Element = PlainElement Name Attributes [Node]
             | ApplyElement Attributes [Node]
             | BindElement Attributes [Node]
             | BlankElement Name Attributes [Node]
             | DoctypeElement

-- | Turn lazy text into templates.
parse :: LT.Text -> Template s
parse = parseWithOverrides defaultOverrides

-- | Use overrides when parsing a template.
parseWithOverrides :: Overrides -> LT.Text -> Template s
parseWithOverrides o t =
  let textWithoutDoctype = LT.replace "<!DOCTYPE html>" "<doctype />" t
      (X.Document _ (X.Element _ _ nodes) _) = D.parseLT ("<div>" <> textWithoutDoctype <> "</div>")
  in mk $! map (toLarcenyNode o) nodes


toLarcenyName :: X.Name -> Name
toLarcenyName (X.Name tn _ _) =
  case T.stripPrefix "l:" tn of
    Just larcenyTagName -> Name (Just "l") larcenyTagName
    Nothing -> case T.stripPrefix "svg:" tn of
                 Just svgTagName -> Name (Just "svg") svgTagName
                 Nothing -> Name Nothing tn

toLarcenyNode :: Overrides -> X.Node -> Node
toLarcenyNode o (X.NodeElement (X.Element tn atr nodes)) =
  let larcenyNodes = map (toLarcenyNode o) nodes
      attrs = M.mapKeys X.nameLocalName atr
      allPlainNodes = (HS.fromList (customPlainNodes o) `HS.union` html5Nodes `HS.union` svgNodes)
                             `HS.difference` HS.fromList (overrideNodes o)in
  case toLarcenyName tn of

    -- these are our special larceny elements
    Name Nothing "bind" ->
      NodeElement (BindElement attrs larcenyNodes)
    Name Nothing "apply" ->
      NodeElement (ApplyElement attrs larcenyNodes)
    Name Nothing "apply-content" ->
      NodeElement (BlankElement (Name Nothing "apply-content") attrs larcenyNodes)
    Name Nothing "doctype" ->
      NodeElement DoctypeElement

    -- these are the blank and plain elements
    -- if it's in the "svg" prefix, it's definitely a plain node
    -- if there's an "l" prefix, it's definitely a Blank
    -- if there's not a prefix, and the tag is a member of the set of plain nodes, it's plain
    -- otherwise, it's a Blank
    Name (Just "svg") name ->
      NodeElement (PlainElement (Name (Just "svg") name) attrs larcenyNodes)
    Name (Just "l") name ->
      NodeElement (BlankElement (Name (Just "l") name) attrs larcenyNodes)
    Name pf name | HS.member name allPlainNodes ->
      NodeElement (PlainElement (Name pf name) attrs larcenyNodes)
    Name _ name ->
      NodeElement (BlankElement (Name Nothing name) attrs larcenyNodes)
toLarcenyNode _ (X.NodeContent c)  = NodeContent c
toLarcenyNode _ (X.NodeComment c) = NodeComment c
toLarcenyNode _ (X.NodeInstruction _) = NodeContent ""

-- | Turn HTML nodes and overrides into templates.
mk :: [Node] -> Template s
mk = f
  where f nodes =
          Template $ \pth m _l ->
                      do lSubs .= m
                         lPath .= pth
                         T.concat <$> process nodes

fillIn :: Blank -> Substitutions s -> Fill s
fillIn tn m = fromMaybe (fallbackFill tn m) (M.lookup tn m)

fallbackFill :: Blank -> Substitutions s -> Fill s
fallbackFill FallbackBlank m =  fromMaybe (textFill "") (M.lookup FallbackBlank m)
fallbackFill (Blank tn) m =
  let fallback = fromMaybe (textFill "") (M.lookup FallbackBlank m) in
  Fill $ \attr (pth, tpl) lib ->
    do liftIO $ putStrLn ("Larceny: Missing fill for blank " <> show tn <> " in template " <> show pth)
       unFill fallback attr (pth, tpl) lib

add :: Substitutions s -> Template s -> Template s
add mouter tpl =
  Template (\pth minner l -> runTemplate tpl pth (minner `M.union` mouter) l)

process :: [Node] -> LarcenyM s [Text]
process [] = return []
process (NodeElement (BindElement atr kids):nextNodes) = do
  processBind atr kids nextNodes
process (currentNode:nextNodes) = do
  processedNode <-
    case currentNode of
      NodeElement DoctypeElement  -> return ["<!DOCTYPE html>"]
      NodeElement (ApplyElement atr kids) ->
          processApply atr kids
      NodeElement (PlainElement tn atr kids) ->
          processPlain tn atr kids
      NodeElement (BlankElement (Name _ name) atr kids) ->
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
processPlain :: Name ->
                Attributes ->
                [Node] ->
                LarcenyM s [Text]
processPlain tagName atr kids = do
  pc <- get
  atrs <- attrsToText atr
  processed <- process kids
  return $ tagToText (_lOverrides pc) tagName atrs processed

selfClosing :: Overrides -> HS.HashSet Text
selfClosing (Overrides _ _ sc) =
  HS.fromList sc <> html5SelfClosingNodes

tagToText :: Overrides
          -> Name
          -> Text
          -> [Text]
          -> [Text]
tagToText overrides (Name mPf name) atrs processed =
  let prefix = fromMaybe "" ((\pf -> pf <> ":") <$> mPf) in
  if name `HS.member` selfClosing overrides
  then ["<" <> prefix <> name <> atrs <> "/>"]
  else ["<" <> prefix <> name <> atrs <> ">"]
           ++ processed
           ++ ["</" <> prefix <> name <> ">"]

attrsToText :: Attributes -> LarcenyM s Text
attrsToText attrs =
  T.concat <$> mapM attrToText (M.toList attrs)
  where attrToText (k,v) = do
          let (unboundK, unboundV) =  eUnboundAttrs (k,v)
          keys <- T.concat <$> mapM fillAttr unboundK
          vals <- T.concat <$> mapM fillAttr unboundV
          return $ toText (keys, vals)
        toText (k, "") = " " <> k
        toText (k, v) = " " <> k <> "=\"" <> T.strip v <> "\""

fillAttrs :: Attributes -> LarcenyM s Attributes
fillAttrs attrs =  M.fromList <$> mapM fill (M.toList attrs)
  where fill p = do
          let (unboundKeys, unboundValues) = eUnboundAttrs p
          keys <- T.concat <$> mapM fillAttr unboundKeys
          vals <- T.concat <$> mapM fillAttr unboundValues
          return (keys, vals)

fillAttr :: Either Text Blank -> LarcenyM s Text
fillAttr eBlankText =
  do (LarcenyState pth m l o _ _) <- get
     case eBlankText of
         Right hole -> unFill (fillIn hole m) mempty (pth, mk []) l
         Left text -> return text

-- Look up the Fill for the hole.  Apply the Fill to a map of
-- attributes, a Template made from the child nodes (adding in the
-- outer substitution) and the library.
processBlank :: Text ->
                Attributes ->
                [Node] ->
                LarcenyM s [Text]
processBlank tagName atr kids = do
  (LarcenyState pth m l o _ _) <- get
  filled <- fillAttrs atr
  sequence [ unFill (fillIn (Blank tagName) m)
                    filled
                    (pth, add m (mk kids)) l]

processBind :: Attributes ->
               [Node] ->
               [Node] ->
               LarcenyM s [Text]
processBind atr kids nextNodes = do
  (LarcenyState pth m l o _ _) <- get
  let tagName = atr M.! "tag"
      newSubs = subs [(tagName, Fill $ \_a _t _l ->
                                       runTemplate (mk kids) pth m l)]
  lSubs .= newSubs `M.union` m
  process nextNodes

-- Look up the template that's supposed to be applied in the library,
-- create a substitution for the content hole using the child elements
-- of the apply tag, then run the template with that substitution
-- combined with outer substitution and the library.
processApply :: Attributes ->
                [Node] ->
                LarcenyM s [Text]
processApply atr kids = do
  (LarcenyState pth m l o _ _) <- get
  filledAttrs <- fillAttrs atr
  let (absolutePath, tplToApply) = findTemplateFromAttrs pth l filledAttrs
  contentTpl <- runTemplate (mk kids) pth m l
  let contentSub = subs [("apply-content",
                         rawTextFill contentTpl)]
  sequence [ runTemplate tplToApply absolutePath (contentSub `M.union` m) l ]

findTemplateFromAttrs :: Path ->
                         Library s ->
                         Attributes ->
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

eUnboundAttrs :: (Text, Text) -> ([Either Text Blank], [Either Text Blank])
eUnboundAttrs (name, value) = do
  let possibleWords = T.splitOn "${"
  let mWord w =
        case T.splitOn "}" w of
          [_] -> [Left w]
          ["",_] -> [Left ("${" <> w)]
          (word: rest) -> Right (Blank word) : map Left rest
          _ -> [Left w]
  ( concatMap mWord (possibleWords name)
    , concatMap mWord (possibleWords value))

{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use first" :: String) #-}
