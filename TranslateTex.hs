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
import Utils
import Prelude hiding (take)
import qualified Data.Text as T
import Control.Applicative hiding (many, satisfy, (<|>))
import Text.Parsec hiding (label, satisfy)
import qualified Control.Arrow as A
import Control.Monad.Identity

type TexParser loc = Parsec [Chunk loc] ()

tok = tokenPrim show (\p _ _ -> p)

withInput s p = do { s0 <- getInput; setInput s; x <- p; setInput s0; return x }

-- Primitive Chunk parsers
env       :: Show loc => TexParser loc (String, [Arg loc], Block loc)
namedEnv  :: Show loc => String -> TexParser loc ([Arg loc], Block loc)
command   :: Show loc => TexParser loc (String, Maybe [Arg loc])
raw       :: Show loc => TexParser loc T.Text
reference :: Show loc => TexParser loc loc
braced    :: Show loc => TexParser loc (Block loc)
anyChunk  :: Show loc => TexParser loc (Chunk loc)
satisfy   :: Show loc => (Chunk loc -> Bool) -> TexParser loc (Chunk loc)

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
  return (Definition () lab desc clauses)
  where
  notComment = \case { Env "comment" _ _ -> False; _ -> True }
  clause = (left . Block) <$> many (satisfy notComment)

document :: TexParser Ref (RawDocument)
document = do
  preamble <- many (satisfy (\case { Env "document" _ _ -> False; _ -> True }))
  decls    <- many (definition <|> theorem)
  return (Macros (Block preamble) : decls)

translate :: Block Ref -> Err RawDocument
translate = ExceptT . Identity . A.left show . parse document "" . unBlock -- TODO: Source name

type Err = Except String

pattern NoArgCmd name      = Command name Nothing
pattern OneArgCmd name arg = Command name (Just [FixArg arg])
pattern LabelCmd arg       = OneArgCmd "label" (Block [Raw arg])

{-
theoremName :: Maybe [Arg Ref] -> Err (Block Ref)
theoremName =
  maybe (throwError "Expected theorem name")
        (\case { [FixArg b] -> return b;
                 _          ->
                throwError "Wrong number of arguments for theorem environment"})

(<:>) x y = fmap (x:) y

block :: Block Ref -> Err RawDocument
block = block' . unBlock

-- TODO: Error out when there's two labels in the block
-- TODO: Check that raw whitespace as the first chunk doesn't screw it up
statementAndLabel :: Block Ref -> Err (TheoremStatement Ref, NodeData)
statementAndLabel (Block (LabelCmd lab : cs)) =
  (, Just (Label $ strip lab)) <$> statement cs
statementAndLabel (Block cs) = (, Nothing) <$> statement cs

-- TODO: Make more robust, support other kinds of statements
statement :: [Chunk Ref] -> Err (TheoremStatement Ref)
statement (Env "suppose" Nothing (Block as) : Env "then" Nothing (Block rs) : []) = 
  AssumeProve <$> assumps as <*> results rs
  where
  itemList _ [] = pure []
  itemList s (NoArgCmd "item" : (break (== NoArgCmd "item") -> (a, rest))) =
    Block a <:> itemList s rest
  itemList s _ = throwError ("Malformed list of " ++ s)

  assumps = itemList "assumptions"
  results = itemList "results"

statement _ = throwError "Could not parse theorem statement"

proof = undefined

block' :: [Chunk Ref] -> Err RawDocument
block' (Env e args b : Env "proof" Nothing prf : rest)
  | e `elem` thmKinds = do
      name        <- theoremName args
      (stmt, lab) <- statementAndLabel b
      p           <- proof prf
      Theorem () lab e name stmt p <:> block' rest
    -- TODO: Labels
  where
  thmKinds = ["theorem", "lemma"]
-}
