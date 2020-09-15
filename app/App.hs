module App where

import Prelude hiding(writeFile)

import Control.Concurrent.STM
import Control.Concurrent

import Data.String.ToString
import Data.Maybe(fromMaybe)
import qualified Data.Map.Strict as Map

import Network.Wai
import Network.Wai.Handler.Warp (runEnv)

import System.Environment(lookupEnv)

import Templates
import Routing
import Blog 


app :: TVar BlogPosts -> Application
app tPosts request respond = do 
  posts <- readTVarIO tPosts
  let getBlog = htmlRoute . blog posts
  respond $ 
    case toString $ rawPathInfo request of
      str
        | str == "/" -> getBlog (fst $ head $ Map.toDescList posts)
        | str == "/posts" -> htmlRoute $ blogs posts
        | take 7 str == "/posts/" && Map.member (BlogKey $ drop 7 $ str) posts -> getBlog $ BlogKey $ drop 7 str
        | str == "/css/style.css" -> css
        | otherwise -> notFound

m :: IO ()
m = do
  maybePort <- lookupEnv "PORT"
  let port = fromMaybe "8080" maybePort
  putStrLn $ "http://localhost:" ++ port
  posts  <- getPosts
  tPosts <- newTVarIO posts
  forkIO $ refreshPosts tPosts
  runEnv (read port :: Int) (app tPosts)

-- reloads all md files in the "posts" folder every hour
refreshPosts :: TVar BlogPosts -> IO () 
refreshPosts tPosts = do 
  threadDelay $ 36 * 10^8
  posts <- getPosts
  atomically $ writeTVar tPosts posts 
  refreshPosts tPosts