{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
module Parse where

import Types
import Text.Parsec
import Control.Monad
import Prelude hiding (take)
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

maybeJustified :: Parser (MaybeJustified String)
maybeJustified =
  (,) <$> (literalText <* spaces)
      <*> optionMaybe (symbol "because" *> proof)

suchThat :: Parser SuchThat
suchThat = do
  symbol "such" >> symbol "that"
  SuchThat <$> listOf maybeJustified
           <*> optionMaybe (symbol "because" *> proof)

take :: Parser Step
take = do
  symbol "take"
  Take <$> listOf literalText <*> optionMaybe suchThat

-- Add such that option for let
let_ :: Parser Step
let_ = do
  symbol "let"
  Let <$> listOf maybeJustified <*> optionMaybe suchThat

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
  Claim <$> (literalText <* spaces) <*> (optionMaybe proof)

suppose :: Parser Step
suppose = do
  symbol "suppose"
  assumps <- listOf literalText
  symbol "then"
  results <- listOf literalText
  spaces
  Suppose assumps results <$> optionMaybe (symbol "because" *> proof)

step :: Parser Step
step = let_ <|> suppose <|> take <|> try cases <|> claim

proof :: Parser Proof
proof = try (fmap Simple literalText) <|> fmap Steps (listOf step)

theorem :: Parser Declaration
theorem = do
  symbol "theorem"
  Theorem <$> (literalText <* spaces) <*> (theoremStatement <* spaces) <*> proof

macros = do
  symbol "macros"
  Macros <$> literalText

document :: Parser [Declaration]
document = (macros <|> theorem <|> definition) `sepBy` spaces

