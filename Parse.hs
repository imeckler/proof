{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, RankNTypes #-}
module Parse where

import Types
import Utils
import Text.Parsec hiding (label)
import Control.Monad
import Prelude hiding (take)
import Control.Applicative hiding (many, (<|>))

import Data.Char (isSpace)

type Parser a = forall s m u. (Stream s m Char) => ParsecT s u m a

-- TODO: Make macros

symbol :: String -> Parser String
symbol s = string s <* spaces

lineComment = do
  try (char '%')
  skipMany (satisfy (/= '\n'))

whiteSpace = skipMany (simpleSpace <|> lineComment) where
  simpleSpace = skipMany1 (satisfy isSpace) 

texBlock :: Parser TexBlock
texBlock = do
  string "[|"
  manyTill chunk (try (string "|]"))
  where
  verbEnvs = ["verbatim", "Verbatim", "lstlisting", "minted", "alltt"]
  verbatimBlock = fmap Left $ do
    begin <- symbol "\\begin"
    name <- char '{' *> manyTill (satisfy (/= '}')) (char '}')
    guard $ name `elem` verbEnvs
    text <- manyTill anyChar (try . string $ "\\end{" ++ name ++ "}")
    return (concat [begin, "{", name, "}", text, "\\end{", name, "}"])

  reference = fmap Right $ do
    try $ symbol "\\gref"
    Ref <$> (symbol "{" *> manyTill (noneOf "%~#\\{") (satisfy (/= '}'))) <?> "Reference"

  chunk =  try verbatimBlock
       <|> reference
       <|> Left <$> manyTill anyChar
          -- fix if too inefficient.
            (try (void (symbol "\\gref" *> char '{')) <|> try (void verbatimStart))
    where
      verbatimStart = symbol "\\begin" *> char '{' *> manyTill (satisfy (/= '}')) (char '}')

comment' :: Parser a -> Parser (a, Comment)
comment' p = do
  symbol "comment"
  x <- p
  text1 <- texBlock
  whiteSpace
  text2 <- optionMaybe texBlock
  let (name, comm) = maybe (Nothing, text1) (\t2 -> (Just text1, t2)) text2 
  return $ (x, Comment name comm)

comment = snd <$> comment' (pure ())

listOf p = between (symbol "[" *> whiteSpace) (symbol "]") ((p <* whiteSpace) `sepBy` symbol ",")

stepData = optionMaybe (between (symbol "(") (symbol ")") label <* whiteSpace)
declData = stepData

stepKeywordWithData :: String -> Parser StepData
stepKeywordWithData kw = symbol kw *> stepData

declKeywordWithData :: String -> Parser StepData
declKeywordWithData = stepKeywordWithData

definition :: Parser Declaration
definition =
  Definition <$> declKeywordWithData "definition"
             <*> texBlock <* whiteSpace
             <*> listOf (fmap Left texBlock <|> fmap Right comment)

assumeProve :: Parser TheoremStatement
assumeProve = do
  symbol "assume"
  assumptions <- listOf texBlock
  whiteSpace
  symbol "prove"
  results <- listOf texBlock
  return (AssumeProve assumptions results)

theoremStatement :: Parser TheoremStatement
theoremStatement = assumeProve

maybeJustified :: Parser (MaybeJustified TexBlock)
maybeJustified =
  (,) <$> (texBlock <* whiteSpace)
      <*> optionMaybe (symbol "because" *> proof)

suchThat :: Parser SuchThat
suchThat = do
  symbol "such" >> symbol "that"
  SuchThat <$> listOf maybeJustified
           <*> optionMaybe (symbol "because" *> proof)

take :: Parser Step
take =
  Take <$> stepKeywordWithData "take"
       <*> listOf texBlock
       <*> optionMaybe suchThat

-- Add such that option for let
let_ :: Parser Step
let_ =
  Let <$> stepKeywordWithData "let"
      <*> listOf maybeJustified
      <*> optionMaybe suchThat

cases :: Parser Step
cases =
  Cases <$> stepKeywordWithData "cases" <*> listOf oneCase
  where
  oneCase = do
    symbol "case"
    (,) <$> (texBlock <* whiteSpace) <*> proof

claim :: Parser Step
claim =
  Claim <$> stepKeywordWithData "claim"
        <*> texBlock <* whiteSpace
        <*> optionMaybe proof

suppose :: Parser Step
suppose =
  Suppose <$> (stepKeywordWithData "suppose")
          <*> listOf texBlock <* symbol "then" -- TODO: Need whitespace here before symbol?
          <*> listOf texBlock <* whiteSpace
          <*> optionMaybe (symbol "because" *> proof)

step :: Parser Step
step =  let_
    <|> suppose
    <|> take
    <|> try (uncurry CommentStep <$> comment' stepData)
    <|> try claim
    <|> cases

proof :: Parser Proof
proof = try (fmap Simple texBlock) <|> fmap Steps (listOf step)

label :: Parser Label
label = Label <$> many (noneOf "%~#\\()[]{}")

-- TODO: Add support for in-file definition of theorem-kinds
theorem :: Parser Declaration
theorem = do
  kind      <- symbol "theorem" <|> symbol "lemma"
  name      <- texBlock <* whiteSpace
  mayLabel  <- optionMaybe (between (symbol "(") (symbol ")") label) <* whiteSpace
  statement <- theoremStatement <* whiteSpace
  prf       <- proof
  pure (Theorem mayLabel kind name statement prf)

macros = do
  symbol "macros"
  Macros <$> literalText
  where literalText = symbol "[|" *> manyTill anyChar (try (symbol "|]"))

document :: Parser [Declaration]
document = (macros <|> theorem <|> definition) `sepBy` whiteSpace

