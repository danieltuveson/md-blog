
module Markdown where 

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String(Parser, parseFromFile)

import Control.Monad
import Data.Maybe


{--
The functions in this file are responsible for reading markdown and building an 
abstract syntax tree, that can be then be translated into HTML
--}


data Markdown
  = MDText [MDText]
  | Header MDHeader [MDText]
  | CodeBlock Language String 
  | List MDList 
  deriving(Show, Eq)


data MDText
  = Plain String 
  | Bold MDText 
  | Italic MDText 
  | Link (String, String)
  | Strikethrough MDText 
  | CodeLine String 
  deriving(Show, Eq)

data MDHeader 
  = H1
  | H2
  | H3
  | H4
  | H5
  | H6
  deriving(Show, Eq, Enum)

data MDList 
  = OL [(Int, [MDText])]
  | UL [[MDText]]
  deriving(Show, Eq)

data Language
  = Haskell 
  | NoLanguage
  deriving(Show, Eq)


nonNewlineSpace :: Parser Char 
nonNewlineSpace = 
  do  nl <- space <?> "non-newline space"
      if nl == '\n' then 
        unexpected "non-newline space" 
      else return nl

parseEOF = eof >> return ""

markdown :: Parser [Markdown]
markdown = 
  do  m <- manyTill parseMD parseEOF
      return m
  where parseMD = (try parseHeader) <|> (try parseList) <|> (try codeBlock) <|> (try parseMDLine >>= return . MDText)

parseMDLine :: Parser [MDText]
parseMDLine = 
  do  m <- manyTill parseMDTextChunk ((try $ string "\n") <|> parseEOF)
      return $ concat m

-- -- Used for debugging parseMDTextChunk
-- parseMDTextChunkNTimes :: Int -> Parser [[MDText]]
-- parseMDTextChunkNTimes n = mapM (const parseMDTextChunk) [1..n]

parseMDTextChunk :: Parser [MDText]
parseMDTextChunk = 
  do  text    <- manyTill anyChar $ (lookAhead $ try inlineIdentifier) <|> (lookAhead $ try $ string "\n") <|> parseEOF -- doesn't matter what eof returns
      inlineStyle <- optionMaybe $ (try bold) <|> (try italic) <|> (try inLineCode) <|> (try parseLink) <|> (try strikethrough)
      case inlineStyle of 
        Nothing -> 
          -- if failed to parse as inline style, parse a *, _, or `, etc as plain text, that way we don't try to parse them again
          do  rem <- optionMaybe $ lookAhead $ try $ inlineIdentifier
              case rem of 
                Nothing -> return [Plain text]
                Just r  -> 
                  do  okactuallytho <- inlineIdentifier 
                      return [Plain $ text ++ r]
        Just s  -> return $ [Plain text] ++ s
  where inlineIdentifier = 
          do  c <- string "*" <|> string "_" <|> string "`" <|> string "h" <|> string "[" <|> string "~"
              return c


inlineStyle :: String -> (String -> MDText) -> Parser [MDText]
inlineStyle str constructor = try $ 
  do  string str
      text  <- manyTill (noneOf "\n") (try (string str))
      lookAhead space
      return $ [constructor text]

bold :: Parser [MDText] 
bold = inlineStyle "**" (Bold . Plain) <|> inlineStyle "__" (Bold . Plain)

italic :: Parser [MDText] 
italic = inlineStyle "*" (Italic . Plain) <|> inlineStyle "_" (Italic . Plain)

inLineCode :: Parser [MDText] 
inLineCode = inlineStyle "`" CodeLine

strikethrough :: Parser [MDText]
strikethrough = inlineStyle "~~" (Strikethrough . Plain)

parseLink :: Parser [MDText]
parseLink = (try autoLink) <|> (try link)
  where link = 
          do  char '['
              text <- manyTill (noneOf "\n") (string "](")
              link <- manyTill nonNewlineSpace (char ')')
              return [Link (text, link)]
        autoLink =  
          do  http <- (try $ string "http://") <|> (try $ string "https://")
              rest <- manyTill anyChar (lookAhead $ try $ space)
              let link = http ++ rest
              return [Link (link, link)]

parseHeader :: Parser Markdown
parseHeader = 
  do  h <- char '#'
      maybeHashes <- count 5 (optionMaybe $ char '#')
      let hashes = h : (map fromJust $ takeWhile isJust maybeHashes)
      let headerType = toEnum $ (length hashes - 1)
      nonNewlineSpace
      l <- parseMDLine
      return $ Header headerType l

parseList :: Parser Markdown
parseList = ((try parseUL) <|> (try parseOL)) >>= return . List

parseUL :: Parser MDList
parseUL = many1 parseULItem >>= return . UL
  where parseULItem = 
          do  oneOf "-*"
              nonNewlineSpace
              l <- parseMDLine
              return l

parseOL :: Parser MDList
parseOL = many1 parseOLItem >>= return . OL
  where parseOLItem = 
          do  num <- many1 digit
              char '.'
              nonNewlineSpace
              l <- parseMDLine
              return (read num :: Int, l)

codeBlock :: Parser Markdown
codeBlock = 
  do  string "```"
      lang <- parseLanguage
      newline 
      code <- manyTill anyChar $ try $ (newline >> string "```")
      return $ CodeBlock lang code 

parseLanguage :: Parser Language
parseLanguage = 
  do  optional nonNewlineSpace
      str <- many alphaNum
      return $ toLanguage str
  where toLanguage str
          | str == "haskell" = Haskell
          | otherwise = NoLanguage 
