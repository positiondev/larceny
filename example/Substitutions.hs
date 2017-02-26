{-# LANGUAGE OverloadedStrings #-}

module Substitutions where

import           Data.Monoid ((<>))
import qualified Data.Text   as T
import           Web.Larceny

import           Data        (Team (..), teamDatabase)

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
                                shortenFill) ]
  where shortenFill maybeNumber fullText = textFill $
          case maybeNumber of
            Just numChars -> T.take numChars fullText <> "..."
            Nothing -> fullText
