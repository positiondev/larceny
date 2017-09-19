#!/usr/bin/env stack
-- stack --resolver lts-5.14 --install-ghc runghc --package larceny
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.HashSet         as HS
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as ST
import qualified Data.Text.Lazy       as LT
import           Lens.Micro
import           System.Directory     (doesDirectoryExist, listDirectory)
import           System.FilePath      (dropExtension, takeExtension)
import qualified Text.HTML.DOM        as D
import qualified Text.XML             as X
------------
import           Web.Larceny
import           Web.Larceny.Fills
import           Web.Larceny.Html     (html5Nodes, html5SelfClosingNodes)
import           Web.Larceny.Internal
import           Web.Larceny.Svg      (svgNodes)
import           Web.Larceny.Types

main = do putStrLn "Parsing templates"
          let o = defaultOverrides
          tpls <- loadTemplates' "INSERT_DIRECTORY_HERE" o
          mapM (\(fp, tpl) -> ST.writeFile fp tpl) tpls
          putStrLn "Finished parsing"

loadTemplates' :: FilePath -> Overrides -> IO [(FilePath,(Text))]
loadTemplates' path overrides =
  do tpls <- getAllTemplates path
     mapM (\file -> do content <- ST.readFile (path <> "/" <> file)
                       return (path <> "/" <>file,
                                 parseWithOverrides' overrides (LT.fromStrict content)))
                         tpls
  where mkPath p = T.splitOn "/" $ T.pack $ dropExtension p

toLarcenyNode' :: Overrides -> X.Node -> Node
toLarcenyNode' o (X.NodeElement (X.Element tn atr nodes)) =
  let larcenyNodes = map (toLarcenyNode' o) nodes
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

    -- these are the blank and plain elements
    -- if it's in the "svg" namespace, it's a plain node
    -- otherwise, if there's a namespace, it's definitely a Blank
    -- if there's not a namespace, and the tag is a member of the set of plain nodes, it's not a Blank
    -- otherwise, it's a blan
    Name (Just "svg") name ->
      NodeElement (PlainElement (Name (Just "svg") name) attrs larcenyNodes)
    Name (Just ns) name ->
      NodeElement (BlankElement (Name (Just ns) name) attrs larcenyNodes)
    Name ns name | HS.member name (svgNodes `HS.difference` html5Nodes) ->
      NodeElement (PlainElement (Name (Just "l") name) attrs larcenyNodes)
    Name ns name | HS.member name allPlainNodes ->
      NodeElement (PlainElement (Name ns name) attrs larcenyNodes)
    Name Nothing name ->
      NodeElement (BlankElement (Name Nothing name) attrs larcenyNodes)
toLarcenyNode' _ (X.NodeContent c)  = NodeContent c
toLarcenyNode' _ (X.NodeComment c) = NodeComment c
toLarcenyNode' _ (X.NodeInstruction _) = NodeContent ""

-- | Use overrides when parsing a template.
parseWithOverrides' :: Overrides -> LT.Text -> Text
parseWithOverrides' o t =
  let (X.Document _ (X.Element _ _ nodes) _) = D.parseLT ("<div>" <> t <> "</div>")
  in T.concat $ process' o $  map (toLarcenyNode' o) nodes

process' :: Overrides -> [Node] -> [Text]
process' _ [] = []
process' o (currentNode:nextNodes) =
  let processedNode =
        case currentNode of
          NodeElement (BindElement atr kids) ->
            tagToText' o (Name Nothing "bind") (attrsToText' atr) (process' o kids)
          NodeElement (ApplyElement atr kids) ->
            tagToText' o (Name Nothing "apply") (attrsToText' atr) (process' o kids)
          NodeElement (PlainElement tn atr kids) ->
            processPlain' o tn atr kids
          NodeElement (BlankElement (Name Nothing name) atr kids) ->
            processBlank' o name atr kids
          NodeElement (BlankElement (Name (Just _) name) atr kids) ->
            processBlank' o name atr kids
          NodeContent t ->
            [t]
          NodeComment c ->
            ["<!--" <> c <> "-->"]
          _ ->
            []
      restOfNodes = process' o nextNodes in
  processedNode ++ restOfNodes


processPlain' :: Overrides ->
                 Name ->
                 Attributes ->
                 [Node] ->
                 [Text]
processPlain' o tagName atr kids = do
  let processed = process' o kids
      textAtrs = attrsToText' atr
  tagToText' o tagName textAtrs processed

tagToText' :: Overrides
          -> Name
          -> Text
          -> [Text]
          -> [Text]
tagToText' overrides (Name mNs name) atrs processed =
  let prefix = fromMaybe "" ((\ns -> ns <> ":") <$> mNs) in
  if name `HS.member` selfClosing overrides
  then ["<" <> prefix <> name <> atrs <> "/>"]
  else ["<" <> prefix <> name <> atrs <> ">"]
           ++ processed
           ++ ["</" <> prefix <> name <> ">"]

attrsToText' :: Attributes -> Text
attrsToText' attrs =
  T.concat $ map toText (M.toList attrs)
  where toText (k, "") = " " <> k
        toText (k, v) = " " <> k <> "=\"" <> T.strip v <> "\""

processBlank' :: Overrides ->
                 Text ->
                 Attributes ->
                 [Node] ->
                 [Text]
processBlank' o tagName atr kids = do
  let processed = process' o kids
      textAtrs = attrsToText' atr
  tagToText' o (Name Nothing tagName) textAtrs processed
