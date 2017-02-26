module Main where

import           Data.Monoid              ((<>))
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Web.Fn
import           Web.Larceny

import           Data
import           Substitutions

data Ctxt = Ctxt { request       :: FnRequest
                 , templates     :: Library ()
                 , substitutions :: Substitutions ()}

instance RequestContext Ctxt where
  getRequest = request
  setRequest ctxt newRequest = ctxt { request = newRequest }

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler ctxt = larcenyRenderWith ctxt mempty ["index"]

teamHandler :: Ctxt -> Int -> IO (Maybe Response)
teamHandler ctxt teamId =
  let mTeam = findTeam teamId
      renderTeam team = larcenyRenderWith ctxt (teamSubs team) ["team"] in
  maybe (return Nothing) renderTeam mTeam

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> indexHandler
             , path "team" // segment ==> teamHandler
             , anything ==> larcenyServe
             , anything ==> staticServe "static"]
   `fallthrough` notFoundText "Page not found."

main :: IO ()
main = do
  ctxt <- initializer
  run 8000 (toWAI ctxt site)

initializer :: IO Ctxt
initializer = do
  templates' <- loadTemplates "templates" defaultOverrides
  return $ Ctxt defaultFnRequest templates' defaultSubs

larcenyRenderWith :: Ctxt -> Substitutions () -> Path -> IO (Maybe Response)
larcenyRenderWith ctxt subs' pth = do
  mPage <- renderWith (templates ctxt)
                      (substitutions ctxt <> subs')
                      ()
                      pth
  case mPage of
    Just page -> okHtml page
    Nothing -> return Nothing

larcenyServe :: Ctxt -> IO (Maybe Response)
larcenyServe ctxt = do
  mPage <- larcenyRenderWith ctxt mempty (pathInfo . fst . getRequest $ ctxt)
  case mPage of
    Nothing -> larcenyRenderWith ctxt mempty ((pathInfo . fst . getRequest $ ctxt) <> ["index"])
    Just _ -> return mPage
