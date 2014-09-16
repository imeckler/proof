{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
module Parse where

import Types
import Text.Parsec
import Control.Monad
import Control.Applicative hiding (many, (<|>))

type Parser a = forall s m u. (Stream s m Char) => ParsecT s u m a

symbol :: String -> Parser String
symbol s = string s <* spaces

literalText :: Parser String
literalText = do
  string "[|"
  manyTill anyChar (try (string "|]"))

comment :: Parser Comment
comment = do
  symbol "comment"
  text1 <- literalText
  text2 <- optionMaybe literalText 
  let (name, comm) = maybe (Nothing, text1) (\t2 -> (Just text1, t2)) text2 
  return $ Comment name comm

listOf p = between (symbol "[") (symbol "]") ((p <* spaces) `sepBy` symbol ",")

definition :: Parser Declaration
definition = do
  symbol "definition"
  Definition <$> (literalText <* spaces)
             <*> listOf (fmap Left literalText <|> fmap Right comment)

assumeProve :: Parser TheoremStatement
assumeProve = do
  symbol "assume"
  assumptions <- listOf literalText
  spaces
  symbol "prove"
  results <- listOf literalText
  return (AssumeProve assumptions results)

theoremStatement :: Parser TheoremStatement
theoremStatement = assumeProve

-- Add such that option for let
let_ :: Parser Step
let_ = do
  symbol "let"
  Let <$> listOf literalText

cases :: Parser Step
cases = do
  symbol "cases"
  Cases <$> listOf oneCase
  where
  oneCase = do
    symbol "case"
    (,) <$> (literalText <* spaces) <*> proof

claim :: Parser Step
claim = do
  symbol "claim"
  Claim <$> (literalText <* spaces) <*> proof

step :: Parser Step
step = let_ <|> try cases <|> claim

proof :: Parser Proof
proof = try (fmap Simple literalText) <|> fmap Steps (listOf step)

theorem :: Parser Declaration
theorem = do
  symbol "theorem"
  Theorem <$> (literalText <* spaces) <*> (theoremStatement <* spaces) <*> proof

document :: Parser [Declaration]
document = (theorem <|> definition) `sepBy` spaces

