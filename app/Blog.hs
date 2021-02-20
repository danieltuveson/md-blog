module Blog where 

import Markdown(Markdown, parseMarkdown)
import HTML(markdownToHtml)

import Text.Parsec.String(parseFromFile)
import Text.Blaze.Html 

import Safe(atMay)
import Data.Either
import Data.Sort
import qualified Data.Map.Strict as Map

import System.Directory(listDirectory)
import System.FilePath.Posix


-- Keys are the full name of the file (so {date}-{title}.md)
newtype BlogKey = BlogKey String 
  deriving(Show, Eq, Ord)

type BlogPosts = Map.Map BlogKey Blog

data Blog = Blog
  { date :: String 
  , title :: String 
  , blogContents :: Html
  , prevPost :: Maybe BlogKey
  , nextPost :: Maybe BlogKey
  } 

-- Grabs all posts in the "posts" directory, and returns a map of 
getPosts :: IO BlogPosts
getPosts = do 
  files <- listDirectory folder 
  eitherMarkdown <- mapM parseFile files
  if (length $ lefts eitherMarkdown) > 0 then 
    -- If it can't parse a blog, don't crash the site, but let me know I messed up
    putStrLn $ "Error parsing new blog post: " ++ (show $ lefts eitherMarkdown)
  else return ()
  -- Return whatever parses successfully 
  return $ getPostsFromFiles $ zip files $ rights eitherMarkdown
  where 
    folder = "./posts"
    parseFile = parseFromFile parseMarkdown . (folder </>)

-- Helper function for getPosts
getPostsFromFiles :: [(FilePath, [Markdown])] -> BlogPosts 
getPostsFromFiles files = Map.fromList posts
  where 
    htmls = map (\(f, m) -> (f, markdownToHtml m)) $ sortBy (\x y -> compare (fst x) (fst y)) files
    posts = map makePost $ zip [0..] htmls
    makePost (i, (str, file)) = 
      let date  = take 10 str
          title = take (length str - 14) $ drop 11 str 
          prevPost = BlogKey . fst <$> htmls `atMay` (i - 1)
          nextPost = BlogKey . fst <$> htmls `atMay` (i + 1)
      in (BlogKey str, Blog date title file prevPost nextPost)

