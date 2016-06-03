{-# LANGUAGE OverloadedStrings #-}

module Examples where

import qualified Data.Map    as M
import           Data.Monoid ((<>))
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Larceny

tpl1 :: Text
tpl1 = "<h1>Hello</h1>"

tpl2 :: Text
tpl2 = "<h1><name/></h1>"

tpl3 :: Text
tpl3 = "<apply name=\"skater\" />"

tpl4 :: Text
tpl4 = "<body>                     \
\          <h1>                    \
\            <name/>               \
\          </h1>                   \
\          <ul>                    \
\            <skaters>             \
\              <li>                \
\                <h2><name/></h2>  \
\                <p><position/></p>\
\              </li>               \
\            </skaters>            \
\          </ul>                   \
\        </body>"

tpl5 :: Text
tpl5 = "<desc length=\"10\" text=\"A really long description\" />"

subst :: BlankFills
subst = fills [ ("site-title", text "Gotham Girls roster")
            , ("name", text "Gotham Girls roster")
            , ("skater", fill $ fills [("name", text "Amy Roundhouse")])
            , ("skaters", mapFills
                          (\(n, p) -> fills [("name", text n)
                                            ,("position", text p)])
                          [ ("Bonnie Thunders", "jammer")
                          , ("Donna Matrix", "blocker")
                          , ("V-Diva", "jammer") ] )
            , ("desc", useAttrs ((a"length" %
                                 a"text")
                                (\n d -> T.take n d <> "...")))]

tplLib :: Library
tplLib = M.fromList [("skater", (parse "Beyonslay") )]
