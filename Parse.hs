{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
module Parse where

import Types
import Utils
import Text.Parsec hiding (label)
import Control.Monad
import Prelude hiding (take)
import Control.Applicative hiding (many, (<|>), optional)
import Data.Functor.Coproduct

import Data.Char (isSpace)

-- TODO: Make macros
-- TODO: Implement scope checking for labels

comment' :: Parser a -> Parser (a, Comment Ref)
comment' p = do
  symbol "comment"
  x <- p
  text1 <- texBlock
  whiteSpace
  text2 <- optionMaybe texBlock
  let (name, comm) = maybe (Nothing, text1) (\t2 -> (Just text1, t2)) text2 
  return (x, Comment name comm)

comment = snd <$> comment' (pure ())

listOf p = between (symbol "[" *> whiteSpace) (symbol "]") ((p <* whiteSpace) `sepBy` symbol ",")

nodeData :: Parser NodeData
nodeData = optionMaybe (between (symbol "(") (symbol ")") label <* whiteSpace)

keywordWithData :: String -> Parser NodeData
keywordWithData kw = symbol kw *> nodeData

definition :: Parser (Raw DeclarationF)
definition =
  Definition () <$> keywordWithData "definition"
                <*> texBlock <* whiteSpace
                <*> listOf (fmap left texBlock <|> fmap right comment)

assumeProve :: Parser (TheoremStatement Ref)
assumeProve = do
  symbol "assume"
  assumptions <- listOf texBlock
  whiteSpace
  symbol "prove"
  results <- listOf texBlock
  return (AssumeProve assumptions results)

theoremStatement :: Parser (TheoremStatement Ref)
theoremStatement = assumeProve

maybeJustified :: Parser (MaybeJustifiedF () Ref)
maybeJustified =
  (,) <$> (texBlock <* whiteSpace)
      <*> optionMaybe (symbol "because" *> proof)

suchThat :: Parser (Raw SuchThatF)
suchThat = do
  symbol "such" >> symbol "that"
  SuchThat <$> listOf maybeJustified
           <*> optionMaybe (symbol "because" *> proof)

take :: Parser (Raw StepF)
take =
  Take () <$> keywordWithData "take"
          <*> listOf texBlock
          <*> optionMaybe suchThat

-- Add such that option for let
let_ :: Parser (Raw StepF)
let_ =
  Let () <$> keywordWithData "let"
         <*> listOf maybeJustified
         <*> optionMaybe suchThat

cases :: Parser (Raw StepF)
cases =
  Cases () <$> keywordWithData "cases" <*> listOf oneCase
  where
  oneCase = do
    symbol "case"
    (,) <$> (texBlock <* whiteSpace) <*> proof

claim :: Parser (Raw StepF)
claim =
  Claim () <$> keywordWithData "claim"
           <*> texBlock <* whiteSpace
           <*> optionMaybe proof

suppose :: Parser (Raw StepF)
suppose =
  Suppose () <$> (keywordWithData "suppose")
             <*> listOf texBlock <* symbol "then" -- TODO: Need whitespace here before symbol?
             <*> listOf texBlock <* whiteSpace
             <*> optionMaybe (symbol "because" *> proof)

-- TODO: Restructure parser for clearer parse-error messages. You have to
-- commit to a particular type. E.g., if something deep in a `claim` fails,
-- you'll get an error saying: unexpected "l", expecting "ases"

step :: Parser (Raw StepF)
step =  let_
    <|> suppose
    <|> take
    <|> try (uncurry (CommentStep ()) <$> comment' nodeData)
    <|> try claim
    <|> cases

proof :: Parser (Raw ProofF)
proof = try (fmap Simple texBlock) <|> fmap Steps (listOf step)

label :: Parser Label
label = Label <$> many (noneOf "%~#\\()[]{}")

-- TODO: Add support for in-file definition of theorem-kinds
theorem :: Parser (Raw DeclarationF)
theorem = do
  kind      <- symbol "theorem" <|> symbol "lemma"
  mayLabel  <- optionMaybe (between (symbol "(") (symbol ")") label) <* whiteSpace
  name      <- texBlock <* whiteSpace
  statement <- theoremStatement <* whiteSpace
  prf       <- proof
  pure (Theorem () mayLabel kind name statement prf)

macros :: Parser (Raw DeclarationF)
macros = do
  symbol "macros"
  Macros <$> literalText
  where literalText = symbol "[|" *> manyTill anyChar (try (symbol "|]"))

document :: Parser RawDocument
document = do
  many ((theorem <|> macros <|> definition) <* whiteSpace) <* eof

