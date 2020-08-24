{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding(writeFile)

import Markdown(parseMarkdown)
import HTML(markdownToHtml)

import Text.Parsec.String(parseFromFile)
import Text.Blaze.Html hiding (map)
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import Control.Concurrent(threadDelay)

import Data.ByteString.Lazy(ByteString, writeFile)
import Data.String

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

type BlogPost = ByteString

-- generatePosts :: IO () 
-- generatePosts = do
--   eitherMD <- parseFromFile parseMarkdown "./posts/hello-world.md"
--   case eitherMD of 
--     Left err -> print err 
--     Right md -> writeFile "hello-world.html" $ renderMarkup $ markdownToHtml md 

generatePost :: IO BlogPost
generatePost = do
  eitherMD <- parseFromFile parseMarkdown "./posts/hello-world.md"
  return $ 
    case eitherMD of 
      Left err -> fromString $ show err 
      Right md -> renderMarkup $ markdownToHtml md 

blogPost :: BlogPost -> Response 
blogPost post = responseLBS
  status200
  [("Content-Type", "text/html")]
  post

app :: Application
app request respond = do 
  p <- generatePost
  respond $ case rawPathInfo request of
    -- "/"     -> index
    "/posts/hello-world" -> blogPost p
    -- _       -> notFound

main :: IO ()
main = do
  -- generatePosts
  putStrLn $ "http://localhost:8080/"
  run 8080 app

-- main = print "hi"