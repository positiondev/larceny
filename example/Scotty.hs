module Main where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Monoid                   ((<>))
import qualified Data.Text.Lazy                as LT
import           Network.HTTP.Types.Status     (status404)
import           Network.Wai
import           Network.Wai.Middleware.Static
import           Web.Larceny
import           Web.Scotty

import           Data                          (findTeam)
import           Substitutions                 (defaultSubs, teamSubs)

main :: IO ()
main = do
  templates <- loadTemplates "templates" defaultOverrides

  scotty 8000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/team/:team_id" $ do
      teamId <- param "team_id"
      let mTeam = findTeam teamId
      case mTeam of
        Just team -> larcenyRenderWith templates (teamSubs team) ["team"]
        Nothing -> notFound404

    get "/" $
      larcenyRenderWith templates mempty ["index"]

    get "/:anything" $ do
      path <- pathInfo <$> request
      larcenyRenderWith templates mempty path

larcenyRenderWith :: Library () -> Substitutions () -> Path -> ActionM ()
larcenyRenderWith tpls subs' pth = do
  mPage <- liftIO $ renderWith tpls (defaultSubs <> subs') () pth
  case mPage of
    Just page -> html $ LT.fromStrict page
    Nothing -> notFound404

notFound404 :: ActionM ()
notFound404 = do status status404
                 html "404: Not Found"
