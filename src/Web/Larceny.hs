{-# LANGUAGE OverloadedStrings #-}

{-|

A long description with example use with Scotty.

It'll be something like:

Write some templates.

Here's _post_base.tpl:
@
<html>
  <head><title>Post: <pageTitle /></title></head>
  <body>
    <header>
      <h1><pageTitle /></h1>
    </header>

    <apply-content />

  </body>
</html>
@

And post.tpl:
@
<apply template="_base">
  <bind tag="pageTitle"><someTitle /></bind>
  <h
</apply>
@

Add a `Library` to your Scotty state with `loadTemplates`.

Write some `Substitutions`.

Write some Larceny/Scotty glue.

Use `render` and `renderWith` in your Scotty handlers.

Here's a `larcenyServe`.

Admire your lovely app!!

-}

module Web.Larceny ( Blank(..)
                   , Fill(..)
                   , Attributes
                   , Name(..)
                   , Substitutions
                   , Template(..)
                   , Path
                   , Library
                   , Overrides(..)
                   , defaultOverrides
                   , render
                   , renderWith
                   , renderRelative
                   , loadTemplates
                   , getAllTemplates
                   , subs
                   , fallbackSub
                   , textFill
                   , textFill'
                   , rawTextFill
                   , rawTextFill'
                   , mapSubs
                   , mapSubs'
                   , fillChildren
                   , fillChildrenWith
                   , fillChildrenWith'
                   , maybeFillChildrenWith
                   , maybeFillChildrenWith'
                   , useAttrs
                   , FromAttribute(..)
                   , AttrError(..)
                   , ApplyError(..)
                   , a
                   , (%)
                   , parse
                   , parseWithOverrides) where

import           Control.Monad        (filterM)
import           Control.Monad.State  (evalStateT)
import qualified Data.Map             as M
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as ST
import qualified Data.Text.Lazy       as LT
import           System.Directory     (doesDirectoryExist, listDirectory)
import           System.FilePath      (dropExtension, takeExtension)
------------
import           Web.Larceny.Fills
import           Web.Larceny.Internal
import           Web.Larceny.Types

-- | Render a template from the library by path.
--
-- @
-- render appTemplates appState ["path", "to", "template"]
-- @
render :: Library s -> s -> Path -> IO (Maybe Text)
render l = renderWith l mempty

-- | Render a template from the library by path, with some additional
-- substitutions.
--
-- @
-- renderWith appTemplates extraSubs appState ["path", "to", "template"]
-- @
renderWith :: Library s -> Substitutions s -> s -> Path -> IO (Maybe Text)
renderWith l sub s = renderRelative l sub s []

-- | Render a template found relative to current template's path.
--
-- This will attempt to find the target template starting at the same
-- level as the given path, then will traverse up the directory tree
-- until it finds a template with the target path.
--
-- For example: Given these templates: ["current"], ["current",
-- "dashboard"], ["current", "private", "dashboard"], ["private",
-- "dashboard"], `renderRelative` called with a given path of
-- ["current"] and target path of ["private", "dashboard"] will find
-- ["current", "private", "dashboard"]. If there /wasn't/ a ["current",
-- "private", "dashboard"], it would render ["private", "dashboard"].
renderRelative :: Library s -> Substitutions s -> s -> Path -> Path -> IO (Maybe Text)
renderRelative l sub s givenPath targetPath =
  case findTemplate l givenPath targetPath of
    (pth, Just (Template run)) -> Just <$> evalStateT (run pth sub l) s
    (_, Nothing) -> return Nothing

-- | Load all the templates in some directory into a Library.
loadTemplates :: FilePath -> Overrides -> IO (Library s)
loadTemplates path overrides =
  do tpls <- getAllTemplates path
     M.fromList <$>
       mapM (\file -> do content <- ST.readFile (path <> "/" <> file)
                         return (mkPath file,
                                 parseWithOverrides overrides (LT.fromStrict content)))
                         tpls
  where mkPath p = T.splitOn "/" $ T.pack $ dropExtension p

getAllTemplates :: FilePath -> IO [FilePath]
getAllTemplates path =
  do cExist <- doesDirectoryExist path
     cs <- if cExist then listDirectory path else return []
     let tpls = filter ((== ".tpl") . takeExtension) cs
     dirs <- filterM (doesDirectoryExist . (\d -> path <> "/" <> d)) cs
     rs <- mapM (\dir -> do r <- getAllTemplates (path <> "/" <> dir)
                            return $ map (\p -> dir <> "/" <> p) r) dirs
     return $ tpls ++ concat rs

{-# ANN module ("HLint: ignore Redundant lambda" :: String) #-}
{-# ANN module ("HLint: ignore Use first" :: String) #-}
