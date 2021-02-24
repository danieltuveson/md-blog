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
    link ! rel "stylesheet" ! type_ "text/css" ! href "/css/style.css" 
    link ! rel "stylesheet" ! href "//cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.2.0/build/styles/default.min.css"
    link ! rel "stylesheet" ! href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css" ! dataAttribute "integrity" "sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z" ! dataAttribute "crossorigin" "anonymous"
  body $ do 
    header $ do 
      span $ do
        -- a ! href "/" $ "Home"
        -- span " / "
        -- a ! href "/posts" $ "Posts"
        -- span " / "
        -- a ! href "https://www.linkedin.com/in/daniel-tuveson" $ "LinkedIn"
        -- span " / "
        -- a ! href "https://github.com/danieltuveson" $ "Github"
        nav $ do 
          ol ! class_ "breadcrumb" $ do
            li ! class_ "breadcrumb-item" $ 
              a ! href "/" $ "Home"
            li ! class_ "breadcrumb-item" $ 
              a ! href "/posts" $ "Posts"
            li ! class_ "breadcrumb-item" $ 
              a ! href "/contact" $ "Contact"
            
    br
    br 
    br 
    main $ div h 
    footer $ do 
      br
      br 
      br 
    
    script ! src "//cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.2.0/build/highlight.min.js" $ ""
    script ! src "https://code.jquery.com/jquery-3.5.1.slim.min.js" ! dataAttribute "integrity" "sha384-DfXdz2htPH0lsSSs5nCTpuj/zy4C+OGpamoFVy38MVBnE+IbbVYUew+OrCXaRkfj" ! dataAttribute "crossorigin" "anonymous" $ ""
    script ! src "https://cdn.jsdelivr.net/npm/popper.js@1.16.1/dist/umd/popper.min.js" ! dataAttribute "integrity" "sha384-9/reFTGAW83EW2RDu2S0VKaIzap3H66lZH81PoYlFhbGU+6BZp6G7niu735Sk7lN" ! dataAttribute "crossorigin" "anonymous" $ ""
    script ! src "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js" ! dataAttribute "integrity" "sha384-B4gt1jrGC7Jh4AgTPSdUtOBvfO8shuf57BaghqFfPlYxofvL8/KUEfYiJOMMV+rV" ! dataAttribute "crossorigin" "anonymous" $ ""
    script "hljs.initHighlightingOnLoad();"
    script ! async "async" ! src "https://www.googletagmanager.com/gtag/js?id=UA-177935445-1" $ ""
    script ! charset "UTF-8" ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.2/languages/haskell.min.js" $ ""
    script ! charset "UTF-8" ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.2/languages/java.min.js" $ ""
    script ! charset "UTF-8" ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.1.2/languages/python.min.js" $ ""
    script "window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag('js', new Date()); gtag('config', 'UA-177935445-1');"

blog :: BlogKey -> BlogPosts -> Html
blog key blogPosts = index $ do 
  blogContents b
  br 
  ul ! class_ "nav justify-content-center" $ do 
    li ! class_ "nav-item" $ possiblePrevPost
    li ! class_ "nav-item" $ possibleNextPost
  where b = (blogPosts Map.! key) 
        possiblePrevPost = 
          case prevPost b of 
            Nothing -> ""
            Just (BlogKey p)  -> a ! class_ "nav-link" ! href (stringValue p) $ "<< Previous Post"
        possibleNextPost = 
          case nextPost b of 
            Nothing -> ""
            Just (BlogKey p)  -> a ! class_ "nav-link" ! href (stringValue p) $ "Next Post >>"

blogs :: BlogPosts -> Html 
blogs blogPosts = index $ do
  h1 $ "Posts"
  ul $ mapM_ li htmlList
  where 
    blogList = Map.toDescList blogPosts 
    htmlList = map (uncurry toLink) blogList 
    toLink (BlogKey key) b =
      a ! href (stringValue key) $ 
        toHtml $ (date b) ++ " - " ++ (Blog.title b)

contact :: Html 
contact = index $ do 
  h1 "Contact" 
  ul $ do 
    li $ a ! href "https://www.linkedin.com/in/daniel-tuveson" $ "LinkedIn"
    li $ a ! href "https://github.com/danieltuveson" $ "Github"
    li $ a ! href "mailto: danieltuveson@gmail.com" $ "Email"

notFoundPage :: Html 
notFoundPage = index $ h1 "404: Not Found"