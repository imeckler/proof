{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Parse.Common (
  module Text.Parsec,
  module Control.Applicative,
  Parser,
  lineComment,
  simpleSpace,
  whiteSpace,
  symbol
) where

import Text.Parsec hiding (satisfy, label)
import qualified Text.Parsec as P
import Data.Char (isSpace)
import Control.Applicative hiding (many, (<|>), optional)

type Parser = Parsec String ()

lineComment :: Parser ()
lineComment = (<?> "line comment") $ do
  try (char '%') -- TODO: Why is this a try?
  skipMany (P.satisfy (/= '\n'))

simpleSpace :: Parser ()
simpleSpace = skipMany1 (P.satisfy isSpace) 

whiteSpace :: Parser ()
whiteSpace = optional (skipMany (lineComment <|> simpleSpace) <?> "whitespace") where

symbol :: String -> Parser String
symbol s = string s <* whiteSpace

{-
envBlock :: Parser Chunk
envBlock = do
  begin <- symbol "\\begin"
  name <- char '{' *> manyTill (satisfy (/= '}')) (char '}')
  inner <- texBlock
  end <- concat <$> sequence [symbol "\\end", symbol "{", symbol name, symbol "}"]
  return (Env begin end inner)

sequenceChunks :: [Tex.Chunk] -> Tex.Block Tex.Ref
sequenceChunks = Tex.Block . concatMap go where
  go (Tex.Raw s)             = [Left s]
  go (Tex.Reference r)       = [Right r]
  go (Tex.Env beg end inner) = (Left beg : Tex.unBlock inner ++ [Left end])

texBlock :: Parser (Tex.Block Tex.Ref)
texBlock = sequenceChunks <$> many chunk
  where
  verbEnvs = ["verbatim", "Verbatim", "lstlisting", "minted", "alltt"]

  verbatimBlock = fmap Raw $ do
    begin <- symbol "\\begin"
    name <- char '{' *> manyTill (satisfy (/= '}')) (char '}')
    guard $ name `elem` verbEnvs
    text <- manyTill anyChar (try . string $ "\\end{" ++ name ++ "}")
    return (concat [begin, "{", name, "}", text, "\\end{", name, "}"])

  reference = fmap Reference . (<?> "Reference") $ do
    try $ symbol "\\gref"
    symbol "{"
    ref <- manyTill (noneOf "%~#\\{") (char '}')
    return (Tex.Ref ref)

  chunk =  try verbatimBlock
       <|> reference
       <|> envBlock
       <|> Raw <$> manyTill anyChar (lookAhead endOfChunk)
            
    where
      envStart = symbol "\\begin"

      endOfChunk =  try (void (symbol "\\gref" *> char '{'))
                <|> try (void envStart)
                <|> eof

-}
