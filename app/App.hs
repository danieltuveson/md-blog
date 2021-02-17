{-# LANGUAGE OverloadedStrings #-}

module App where

import Prelude hiding(writeFile)

import Control.Concurrent.STM
import Control.Concurrent

import Data.Maybe(fromMaybe)
import qualified Data.Map.Strict as Map

import Text.Blaze.Html.Renderer.Text(renderHtml)

import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp (runEnv)
import Web.Scotty

import System.Environment(lookupEnv)

import Templates
import Blog 


data Environment
  = Production
  | Stage
  deriving(Show, Eq)


app :: TVar BlogPosts -> IO Application
app tPosts = do 
  posts <- readTVarIO tPosts
  let getBlog = renderHtml . blog posts
  
  scottyApp $ do 
    middleware $ staticPolicy (noDots >-> addBase "static")
    
    get "/" $ do 
      html $ getBlog (fst $ head $ Map.toDescList posts)

    get "/posts" $ do 
      html $ renderHtml $ blogs posts

    get "/:post" $ do 
      post <- param "post"
      if Map.member (BlogKey post) posts then 
        html $ getBlog $ BlogKey $ post
      else 
        next 
        
    notFound $ do 
      html $ renderHtml notFoundPage


m :: IO ()
m = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe "8080" maybePort
  let env = if port == "80" then Production else Stage 
  putStrLn $ "http://localhost:" ++ port
  posts  <- getPosts
  tPosts <- newTVarIO posts
  forkIO $ refreshPosts env tPosts
  
  myapp <- app tPosts
  runEnv (read port :: Int) myapp

-- Reloads all md files in the "posts" folder every hour when live, every or second on stage
refreshPosts :: Environment -> TVar BlogPosts -> IO () 
refreshPosts env tPosts = do 
  threadDelay $ if env == Production then 36 * 10^8 else 10^6
  posts <- getPosts
  atomically $ writeTVar tPosts posts 
  refreshPosts env tPosts