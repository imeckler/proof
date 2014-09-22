{-# LANGUAGE LambdaCase #-}
module Parse.Latex where

import Parse.Common
import qualified Text.Parsec as P
import Data.Char
import Latex

-- TODO: Keep track of SourcePos in the Latex parse tree

block :: Parser Block
block = Block <$> many chunk

chunk :: Parser Chunk
chunk = try env <|> command <|> text

peekChar :: Parser (Maybe Char)
peekChar = Just <$> (try $ lookAhead anyChar) <|> pure Nothing

command :: Parser Chunk
command =
  char '\\' >>
  peekChar >>= \case
    Nothing -> parserFail "File ends in \\"
    Just c  ->
      if isSpecial c
      then (\c' -> Command [c'] Nothing) <$> (anyChar <* whiteSpace)
      else Command <$> many (P.satisfy commandChar) <*> cmdArgs
  where
  isSpecial     = (`elem` specials)
  specials      = "'(),.-\"!^$&#{}%~|/:;=[]\\` "
  commandChar c = isAsciiLower c || isAsciiUpper c

cmdArgs :: Parser (Maybe [Arg])
cmdArgs =  try (const (Just []) <$> (symbol "{" >> symbol "}"))
      <|> (Just <$> many1 cmdArg)
      <|> return Nothing
  where
  -- TODO: Make sure we don't have the same [ problem as HaTeX
  cmdArg =  between (char '{') (char '}') (FixArg <$> block)
        <|> between (char '[') (char ']') (FixArg <$> block)

-- TODO: Environment with args
env :: Parser Chunk
env = do
  symbol "\\begin"
  name  <- char '{' *> many (P.satisfy (/= '}')) <* char '}'
  args  <- cmdArgs
  inner <- block
  symbol "\\end" *> symbol "{" *> symbol name *> symbol "}"
  return (Env name args inner)

braced :: Parser Chunk
braced = Braced <$> between (symbol "{") (symbol "}") block

text :: Parser Chunk
text = Raw <$> many (noneOf "\\{}")

{-
escapedControlChar = try $ do
  c <- anyChar
  case c of
    '{'  -> 
    '\\' -> 
-}
