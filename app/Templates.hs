{-# LANGUAGE OverloadedStrings #-}

module Templates where 

import Prelude hiding (head, id, div, span)

import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title, span)

index :: Html -> Html 
index h = docTypeHtml $ do
    head $ do
      title "Dan's Blog"
      link ! rel "stylesheet" ! type_ "text/css" ! href "../css/style.css" 
    body $ do 
      header $ do 
        span $ do
          a ! href "#" $ "Home"
          span " / "
          a ! href "../css/style.css" $ "Posts"
      main $ div ! class_ "thing" $ h 
      footer $ do 
        span "LinkedIn"
        span " - "
        span "Github"
