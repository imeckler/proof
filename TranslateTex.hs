{-# LANGUAGE LambdaCase,
             PatternSynonyms,
             TupleSections,
             ViewPatterns,
             NoMonomorphismRestriction #-}
module TranslateTex where

import DecoratedTex
import Types
import Control.Monad.Except
import Data.Functor.Coproduct
-- import Control.Applicative
import Data.Char
import Prelude hiding (take)
import qualified Data.Text as T
import Control.Applicative hiding (many, (<|>))
import Text.Parsec hiding (label, satisfy)
import qualified Control.Arrow as A

type TexParser loc = Parsec [Chunk loc] ()

tok = tokenPrim show (\p _ _ -> p)

withInput s p = do { s0 <- getInput; setInput s; x <- p; setInput s0; return x }

-- Primitive Chunk parsers
env          :: Show loc => TexParser loc (String, [Arg loc], Block loc)
namedEnv     :: Show loc => String -> TexParser loc ([Arg loc], Block loc)
command      :: Show loc => TexParser loc (String, Maybe [Arg loc])
namedCommand :: Show loc => String -> TexParser loc (Maybe [Arg loc])
raw          :: Show loc => TexParser loc T.Text
reference    :: Show loc => TexParser loc loc
braced       :: Show loc => TexParser loc (Block loc)
anyChunk     :: Show loc => TexParser loc (Chunk loc)
satisfy      :: Show loc => (Chunk loc -> Bool) -> TexParser loc (Chunk loc)

env        = tok $ \case { Env e args b -> Just (e, args, b); _ -> Nothing }
namedEnv s = tok $ \case
  Env e args b -> if e == s then Just (args, b) else Nothing
  _            -> Nothing
command        = tok $ \case { Command c args -> Just (c, args); _ -> Nothing }
namedCommand s = tok $ \case { Command c args -> if c == s then Just args else Nothing; _ -> Nothing }
raw            = tok $ \case { Raw s -> Just s; _ -> Nothing }
reference      = tok $ \case { Reference r -> Just r; _ -> Nothing }
braced         = tok $ \case { Braced b -> Just b; _ -> Nothing }
anyChunk       = tok $ Just
satisfy p      = tok $ \c -> if p c then Just c else Nothing

-- Latex structure parsers
label = tok $ \case
  Command "label" (Just [FixArg (Block [Raw l])]) -> Just (Label (T.unpack l))
  _                                               -> Nothing

optLabel = optionMaybe label

descSection keyword p = do
  (desc, inner) <- tok $ \case
    Command c (Just [FixArg desc, FixArg (Block inner)]) ->
      if c == keyword then Just (desc, inner) else Nothing
    _                                                    -> Nothing
  x <- withInput inner p
  return (desc, x)

simpleSection keyword p = do
  inner <- tok $ \case
    Command c (Just [FixArg (Block b)]) -> if c == keyword then Just b else Nothing
    _                                   -> Nothing
  withInput inner p

{-
  envStep s p = do
    (Nothing, Block b) <- namedEnv s
    withInput b p
-}

-- TODO: Get rid of pattern matches.
labelAndStatement :: TexParser Ref (Maybe Label, TheoremStatement Ref)
labelAndStatement = (,) <$> optLabel <*> statement
  where
  statement =
    AssumeProve <$> simpleSection "suppose" (many item)
                <*> simpleSection "then" (many item)

item = satisfy (== NoArgCmd "item") >> Block <$> many (satisfy (/= NoArgCmd "item"))

proof :: TexParser Ref (Raw ProofF)
proof =  simpleSection "proof" proofInner
     <|> simpleSection "simple" (Simple . Block <$> many anyChunk)
  where
  proofInner = Steps <$> many step

  cases = simpleSection "cases" (Cases () <$> optLabel <*> many oneCase)
    where oneCase = descSection "case" proofInner

  -- TODO: Make claim proof optional
  claim = do
    (desc, (lab, prf)) <- descSection "claim" ((,) <$> optLabel <*> proofInner)
    return (Claim () lab desc (Just prf))

  -- TODO: Decide what goes in/outside of section macros
  let_ = do
    (lab, bindings) <- simpleSection "let" ((,) <$> optLabel <*> many maybeJustified)
    Let () lab bindings <$> optionMaybe suchThat

  take = do
    (lab, bindings) <- simpleSection "let" ((,) <$> optLabel <*> many item)
    Take () lab bindings <$> optionMaybe suchThat

  -- TODO: Make comment desc optional
  comment = do
    (desc, (lab, comm)) <- descSection "comment" ((,) <$> optLabel <*> many anyChunk)
    return (CommentStep () lab (Comment (Just desc) (Block comm)))

  -- TODO: Decide on syntax for suppose
  -- suppose = undefined

  suchThat =
    simpleSection "suchthat" $
      SuchThat <$> many maybeJustified <*> optBecause

  maybeJustified = do
    satisfy (== NoArgCmd "item")
    (,) <$> (Block <$> manyTill anyChunk 
                          (try (void (satisfy (== NoArgCmd "item"))
                            <|> void (namedCommand "because"))))
        <*> optBecause

  optBecause = optionMaybe (simpleSection "because" proofInner)

  step = cases <|> claim <|> let_ <|> take <|> comment

theorem :: TexParser Ref (Raw DeclarationF)
theorem = do
  (kind, name, b)    <- thmSection
  ((lab, stmt), prf) <- withInput b $ ((,) <$> labelAndStatement <*> proof)
  texSpace
  return (Theorem () lab kind name stmt prf)
  where
  thmSection = tok $ \case
    Command e (Just [FixArg name, FixArg (Block b)]) ->
      if e `elem` thmKinds then Just (e, name, b) else Nothing
    _                                                -> Nothing

  thmKinds = ["theorem", "lemma"]

definition :: TexParser Ref (Raw DeclarationF)
definition = do
  (desc, (lab, clauses)) <- descSection "definition" ((,) <$> optLabel <*> many clause)
  texSpace
  return (Definition () lab desc clauses)
  where
  notComment = \case { Command "comment" _ -> False; _ -> True }

  comment = try $ do
    args <- namedCommand "comment"
    (name, comm) <- case args of
      Just [OptArg lab, FixArg comm] -> return (Just lab, comm)
      Just [FixArg comm]             -> return (Nothing, comm)
      _                              -> fail "comment: Wrong number of arguments"
    return (Comment name comm)

  clause = (left . Block) <$> many1 (satisfy notComment)
        <|> right <$> comment

texSpace :: TexParser Ref ()
texSpace = void (satisfy spaceChunk)

document :: TexParser Ref (RawDocument)
document = do
  preamble <- many (satisfy (\case { Env "document" _ _ -> False; _ -> True }))
  (_, Block body) <- namedEnv "document"
  decls <- withInput body $ do
    texSpace
    many (definition <|> theorem)
  texSpace
  eof
  return (Macros (Block preamble) : decls)

translate :: Monad m => Block Ref -> Err m RawDocument
translate = ExceptT . return . A.left show . parse document "" . unBlock -- TODO: Source name

pattern NoArgCmd name      = Command name Nothing
pattern OneArgCmd name arg = Command name (Just [FixArg arg])
pattern LabelCmd arg       = OneArgCmd "label" (Block [Raw arg])
-- TODO: Error out when there's two labels in the block
-- TODO: Check that raw whitespace as the first chunk doesn't screw it up

