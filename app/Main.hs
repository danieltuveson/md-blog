module Main where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad(forever)

import Data.Maybe(fromMaybe)

import Web.Scotty

import System.Environment(lookupEnv)

import Blog 
import Routing


data Environment
  = Production
  | Stage
  deriving(Show, Eq)


main :: IO ()
main = do
  -- Read port from environment
  maybePort <- lookupEnv "PORT"

  -- If locally debugging, PORT won't be defined, so set to 8080
  let port = fromMaybe "8080" maybePort
  let env = if port == "80" then Production else Stage 

  putStrLn $ "Environment: " ++ show env
  putStrLn $ "http://localhost:" ++ port
  
  -- Initialize posts, put it in our TVar
  posts  <- getPosts
  tPosts <- newTVarIO posts
  
  -- Refresh posts every once in a while in case I added a new one
  forkIO $ refreshPosts env tPosts

  -- Run the app!
  scotty (read port :: Int) $ routing tPosts


-- Reloads all md files in the "posts" folder every 10 minutes when live, every or second on stage
refreshPosts :: Environment -> TVar BlogPosts -> IO () 
refreshPosts env tPosts = forever $ do 
  threadDelay $ if env == Production then 6 * 10^8 else 10^6
  posts <- getPosts
  atomically $ writeTVar tPosts posts 