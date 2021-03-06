module Markdown where 

import Text.Parsec
import Text.Parsec.String(Parser)

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
  | Link String String
  | Image String String
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
  | Java
  | Python
  | NoLanguage
  deriving(Eq)

instance Show Language where 
  show = \case
    Haskell    -> "haskell"
    Java       -> "java"
    Python     -> "python"
    NoLanguage -> "lang-none"


nonNewlineSpace :: Parser Char 
nonNewlineSpace = 
  do  nl <- space <?> "non-newline space"
      if nl == '\n' then 
        unexpected "non-newline space" 
      else return nl

checkEOF :: Parser String 
checkEOF = eof >> return ""

checkEOL :: Parser String 
checkEOL = lookAhead $ try $ string "\n"

parseMarkdown :: Parser [Markdown]
parseMarkdown = manyTill parseMD checkEOF
  where parseMD = parseHeader 
              <|> parseList 
              <|> codeBlock 
              <|> parseMDLineWithNewline

parseMDLine :: Parser [MDText]
parseMDLine = do
  m <- manyTill parseMDTextChunk ((try $ string "\n") <|> checkEOF)
  return $ concat m

parseMDLineWithNewline :: Parser Markdown
parseMDLineWithNewline = try $ do
  m <- manyTill parseMDTextChunk (checkEOL <|> checkEOF)
  n <- string "\n" <|> checkEOF
  return $ MDText (concat m ++ [Plain n])

parseMDTextChunk :: Parser [MDText]
parseMDTextChunk = do 
  text        <- manyTill anyChar $ lookAhead inlineIdentifier <|> checkEOL <|> checkEOF
  maybeStyled <- optionMaybe $ bold <|> italic <|> inLineCode <|> parseLink <|> parseImage <|> strikethrough
  case maybeStyled of 
    Nothing -> do 
      remainder   <- optionMaybe $ inlineIdentifier
      let identifierString = fromMaybe "" remainder 
      return $ [Plain $ text ++ identifierString]
    Just s  -> return $ [Plain text, s]
  where 
    inlineIdentifier = try $ do 
      c <- oneOf "*_`h[~!" 
      return [c]

inlineStyle :: String -> (String -> MDText) -> Parser MDText
inlineStyle str constructor = try $ do
  string str
  text  <- manyTill (noneOf "\n") (try (string str))
  return $ constructor text

bold :: Parser MDText 
bold = inlineStyle "**" (Bold . Plain) <|> inlineStyle "__" (Bold . Plain)

italic :: Parser MDText
italic = inlineStyle "*" (Italic . Plain) <|> inlineStyle "_" (Italic . Plain)

inLineCode :: Parser MDText
inLineCode = inlineStyle "`" CodeLine

strikethrough :: Parser MDText
strikethrough = inlineStyle "~~" (Strikethrough . Plain)

parseLink :: Parser MDText
parseLink = (labeledLink Link) <|> autoLink

-- Parser for labeled hyperlink *or* for an image
labeledLink :: (String -> String -> MDText) -> Parser MDText
labeledLink constuctor = try $ do 
  char '['
  text <- manyTill (noneOf "\n") (string "](")
  l    <- manyTill anyChar (string ")")
  return $ constuctor text l

autoLink :: Parser MDText
autoLink = try $ do 
  l <- rawLink space
  return $ Link l l

rawLink :: Parser Char -> Parser String
rawLink end =  do
  http <- (try $ string "http://") <|> (try $ string "https://")
  rest <- manyTill anyChar (lookAhead $ try $ end)
  end
  return $ http ++ rest

parseImage :: Parser MDText 
parseImage = try (char '!' >> labeledLink Image)

parseHeader :: Parser Markdown
parseHeader = try $ do 
  h <- char '#'
  maybeHashes <- count 5 (optionMaybe $ char '#')
  let hashes = h : (map fromJust $ takeWhile isJust maybeHashes)
  let headerType = toEnum $ (length hashes - 1)
  nonNewlineSpace
  l <- parseMDLine
  return $ Header headerType l

parseList :: Parser Markdown
parseList = (parseUL <|> parseOL) >>= return . List

parseUL :: Parser MDList
parseUL = try $ many1 parseULItem >>= return . UL
  where 
    parseULItem = do 
      oneOf "-*"
      nonNewlineSpace
      l <- parseMDLine
      return l

parseOL :: Parser MDList
parseOL = try $ many1 parseOLItem >>= return . OL
  where 
    parseOLItem = do
      num <- many1 digit
      char '.'
      nonNewlineSpace
      l <- parseMDLine
      return (read num :: Int, l)

codeBlock :: Parser Markdown
codeBlock = try $ do
  string "```"
  lang <- parseLanguage
  newline 
  code <- manyTill anyChar $ try $ (newline >> string "```")
  string "\n" <|> checkEOF
  return $ CodeBlock lang code 

parseLanguage :: Parser Language
parseLanguage = try $ do 
  optional nonNewlineSpace
  str <- many alphaNum
  return $ toLanguage str
  where 
    toLanguage str
      | str == "haskell" = Haskell
      | str == "java"    = Java
      | str == "python"  = Python 
      | otherwise = NoLanguage 