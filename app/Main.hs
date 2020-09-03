{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding(writeFile)

import Markdown(parseMarkdown)
import HTML(markdownToHtml)
import Templates(index)

import Text.Parsec.String(parseFromFile)
import Text.Blaze.Html hiding (map)
import Text.Blaze.Html5(h1)
import Text.Blaze.Renderer.Utf8(renderMarkup)

import Control.Concurrent(threadDelay)

import Data.ByteString.Lazy(ByteString, writeFile)
import Data.String

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import System.Directory(getCurrentDirectory)

type BlogPost = Html

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
      Right md -> markdownToHtml md 

page :: Html -> Response 
page post = responseLBS
  status200
  [("Content-Type", "text/html")]
  (renderMarkup $ index post)

css :: Response
css = responseFile
    status200
    [("Content-Type", "text/css")]
    "./css/style.css"
    Nothing

app :: Application
app request respond = do 
  p <- generatePost
  respond $ case rawPathInfo request of
    "/"     -> page $ h1 "Welcome"
    "/posts/hello-world" -> page p
    "/css/style.css" -> css
    -- _       -> "FUCK"
    

main :: IO ()
main = do
  -- generatePosts
  wd <- getCurrentDirectory
  print wd
  putStrLn $ "http://localhost:80/"
  run 8080 app