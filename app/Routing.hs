module Routing where 

import Text.Blaze.Html 
import Text.Blaze.Renderer.Utf8(renderMarkup)

import Network.Wai
import Network.HTTP.Types

import Templates


htmlRoute :: Html -> Response 
htmlRoute h = responseLBS
  status200
  [("Content-Type", "text/html")]
  (renderMarkup h)

css :: Response
css = responseFile
  status200
  [("Content-Type", "text/css")]
  "./css/style.css"
  Nothing

notFound :: Response 
notFound = responseLBS
  status404
  [("Content-Type", "text/html")]
  (renderMarkup notFoundPage)