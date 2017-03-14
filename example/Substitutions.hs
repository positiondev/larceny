{-# LANGUAGE OverloadedStrings #-}

module Substitutions where

import           Data.Monoid      ((<>))
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as LT
import           Data.Time.Clock
import qualified Text.Blaze.Html5 as H
import           Text.HTML.DOM
import           Text.XML
import           Web.Larceny

import           Data             (Team (..), teamDatabase)

teamSubs :: Team -> Substitutions ()
teamSubs (Team i n y d) =
  subs [ ("id", textFill $ T.pack $ show i)
       , ("name", textFill n)
       , ("foundedIn", textFill y)
       , ("longDesc", textFill d)]

defaultSubs :: Substitutions ()
defaultSubs =
  subs [ ("page-title", textFill "The Best Roller Derby Teams")
       , ("teams", mapSubs
                     teamSubs
                     teamDatabase )
       , ("shorten", useAttrs (a"length" %
                               a"text")
                                shortenFill)

       , ("buggyReverse", buggyReverseFill)
       , ("reverse", reverseFill)]
  where shortenFill maybeNumber fullText = textFill $
          case maybeNumber of
            Just numChars -> T.take numChars fullText <> "..."
            Nothing -> fullText

buggyReverseFill :: Fill s
buggyReverseFill = Fill reverseFill'
  where reverseFill' _attrs (pth,tpl)lib = do
          children <- runTemplate tpl pth mempty lib
          return (T.reverse children)

reverseFill :: Fill s
reverseFill = Fill reverseFill'
  where reverseFill' _attrs (pth,tpl)lib = do
          children <- runTemplate tpl pth mempty lib
          -- html-conduit assumes a single root element
          -- it just abandons orphan text, so wrap in a parent
          let wrappedChildren = "<div>" <> children <> "</div>"
          let doc = parseLT (LT.fromStrict wrappedChildren)
          return (LT.toStrict $ renderText def (reverseDoc doc))

reverseElement :: Element -> Element
reverseElement e =
  e { elementNodes = map reverseNode (reverse $ elementNodes e) }

reverseDoc :: Document -> Document
reverseDoc doc =
  let e = documentRoot doc in
  doc { documentRoot = reverseElement e }

reverseNode :: Node -> Node
reverseNode (NodeElement e) = NodeElement (reverseElement e)
reverseNode (NodeContent c) = NodeContent (T.reverse c)
reverseNode anything = anything
