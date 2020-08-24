{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-} 

module HTML where 

import Markdown

import Prelude hiding (head, id, div)

import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title)

class MD a where 
  evalMarkdown :: a -> Html

instance MD Markdown where 
  evalMarkdown m = 
    case m of 
      MDText mdts        -> evalMDTextsNewline mdts
      Header h mdts      -> evalMDHeader h mdts
      CodeBlock lang str -> evalCodeBlock lang str
      List l             -> evalMarkdown l

instance MD MDText where 
  evalMarkdown mdt = 
    case mdt of 
      Plain str          -> string str
      Bold mdt'          -> strong $ evalMarkdown mdt'
      Italic mdt'        -> em $ evalMarkdown mdt'
      Link (text, link)  -> a ! (href $ stringValue link) $ string text
      Strikethrough mdt' -> del $ evalMarkdown mdt'
      CodeLine str       -> code $ string str

instance MD MDList where 
  evalMarkdown mdl = 
    case mdl of 
      OL l -> ol $ toList olElt l
      UL l -> ul $ toList (li . evalMDTexts) l
    where 
      olElt (i, elt) = li ! (value $ stringValue $ show i) $ evalMDTexts elt
      toList fun = toHtml . map fun

markdownToHtml :: [Markdown] -> Html 
markdownToHtml = toHtml . map evalMarkdown

evalMDHeader :: MDHeader -> [MDText] -> Html
evalMDHeader h = toHeader h . evalMDTexts
  where 
    toHeader h = 
      case h of 
        H1 -> h1
        H2 -> h2
        H3 -> h3
        H4 -> h4
        H5 -> h5
        H6 -> h6

evalCodeBlock :: Language -> String -> Html 
evalCodeBlock lang str = pre ! languageClass $ string str
  where languageClass = class_ $ stringValue $ show lang

evalMDTexts :: [MDText] -> Html 
evalMDTexts = evalMDTextsWithEnd evalMarkdown

evalMDTextsNewline :: [MDText] -> Html 
evalMDTextsNewline = evalMDTextsWithEnd evalToken
  where 
    evalToken t
      | t == Plain "\n" = br 
      | otherwise = evalMarkdown t

evalMDTextsWithEnd :: (MDText -> Html) -> [MDText] -> Html 
evalMDTextsWithEnd evalToken = toHtml . map evalToken
