{-# LANGUAGE OverloadedStrings #-}

module Routing where

import Control.Concurrent.STM
import Control.Monad.IO.Class

import qualified Data.Map.Strict as Map

import Text.Blaze.Html.Renderer.Text(renderHtml)

import Network.Wai.Middleware.Static
import Web.Scotty

import Templates
import Blog 


routing :: TVar BlogPosts -> ScottyM ()
routing tPosts = do 
  -- Include css, js, etc
  middleware $ staticPolicy (noDots >-> addBase "static")
  
  -- Root loads most recent post
  get "/" $ do 
    posts <- getPosts
    let postKey = fst $ head $ Map.toDescList posts
    blogResponse postKey posts

  -- List of blog posts 
  get "/posts" $ getPosts >>= htmlResponse . blogs

  -- Load a specific blog post 
  get "/:post" $ do 
    posts <- getPosts
    post  <- BlogKey <$> param "post"
    if Map.member post posts then 
      blogResponse post posts
    else 
      next 
  
  -- 404
  notFound $ htmlResponse notFoundPage

  where getPosts = liftIO (readTVarIO tPosts)
        htmlResponse = html . renderHtml
        blogResponse postKey = htmlResponse . blog postKey
