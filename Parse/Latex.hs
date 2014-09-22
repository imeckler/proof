{-# LANGUAGE LambdaCase #-}
module Parse.Latex where

import Parse.Common
import Data.Char
import Latex

block :: Parser Block
block = Block <$> many chunk

chunk :: Parser Chunk
chunk = try env <|> command <|> raw

peekChar :: Parser (Maybe Char)
peekChar = Just <$> (try $ lookAhead anyChar) <|> pure Nothing

command = do
  char '\\'
  peekChar >>= \case
    Nothing -> parserFail "File ends in \\"
    Just c  ->
      if isSpecial c
      then return (Command [c] Nothing)
      else Command <$> many (satisfy commandChar) <*> cmdArgs
  where
  isSpecial     = (`elem` specials)
  specials      = "'(),.-\"!^$&#{}%~|/:;=[]\\` "
  commandChar c = isAsciiLower c || isAsciiUpper c

cmdArgs :: Parser (Maybe [Arg])
cmdArgs =  try (const (Just []) <$> string "{}")
      <|> (Just <$> many1 cmdArg)
      <|> return Nothing
  where
  -- TODO: Suppose [ ] args
  cmdArg =  between (char '{') (char '}') (FixArg <$> block)
        <|> between (char '[') (char ']') (FixArg <$> block)

-- TODO: Environment with args
env :: Parser Chunk
env = do
  begin <- symbol "\\begin"
  name  <- char '{' *> many (satisfy (/= '}')) <* char '{'
  args  <- cmdArgs
  inner <- block
  symbol "\\end" *> symbol "{" *> symbol name *> symbol "}"
  return (Env name args inner)

braced = Braced <$> between (symbol "{") (symbol "}") block

text1 = many (noneOf "\\{}")

{-
escapedControlChar = try $ do
  c <- anyChar
  case c of
    '{'  -> 
    '\\' -> 
-}
