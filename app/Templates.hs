module Templates where 

import Prelude hiding (head, id, div, span)

import qualified Data.Map.Strict as Map

import Text.Blaze.Html
import Text.Blaze.Html5 as Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title, span)

import Blog 


index :: Html -> Html 
index h = docTypeHtml $ do
    head $ do
      textComment "Global site tag (gtag.js) - Google Analytics" 
      Html5.title "Daniel Tuveson"
      link ! rel "stylesheet" ! type_ "text/css" ! href "../css/style.css" 
    body $ do 
      header $ do 
        span $ do
          a ! href "/" $ "Home"
          span " / "
          a ! href "/posts" $ "Posts"
      main $ div h 
      footer $ do 
        br
        br 
        br 
        a ! href "https://www.linkedin.com/in/daniel-tuveson" $ "LinkedIn"
        span " - "
        a ! href "https://github.com/danieltuveson" $ "Github"
      script ! async "async" ! src "https://www.googletagmanager.com/gtag/js?id=UA-177935445-1" $ ""
      script "window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag('js', new Date()); gtag('config', 'UA-177935445-1');"

blog :: BlogPosts -> BlogKey -> Html
blog blogPosts key = index $ blogContents (blogPosts Map.! key)
  
blogs :: BlogPosts -> Html 
blogs blogPosts = index $ do
  h1 $ "Posts"
  ul $ mapM_ li htmlList
  where 
    blogList = Map.toDescList blogPosts 
    htmlList = map (uncurry toLink) blogList 
    toLink (BlogKey key) (Blog d t _) =
      a ! href (stringValue $ "posts/" ++ key) $ 
        toHtml $ d ++ " - " ++ t

notFoundPage :: Html 
notFoundPage = index $ h1 "404: Not Found"