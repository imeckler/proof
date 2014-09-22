{-# LANGUAGE LambdaCase,
             PatternSynonyms,
             TupleSections,
             ViewPatterns,
             NoMonomorphismRestriction #-}
module TranslateTex where

import DecoratedTex
import Types
import Control.Monad.Except
-- import Control.Applicative
import Utils
import Parse.Common

type TexParser loc = Parsec [Chunk loc] ()

tok = tokenPrim show (\p _ _ -> p)

withInput s p = do { s0 <- getInput; setInput s; x <- p; setInput s0; return x }

-- Primitive Chunk parsers
env :: Show loc => TexParser loc (String, Maybe [Arg loc], Block loc)
env = tok $ \case { Env e args b -> Just (e, args, b); _ -> Nothing }

namedEnv :: Show loc => String -> TexParser loc (Maybe [Arg loc], Block loc)
namedEnv s = do
  (e, args, b) <- env
  if e == s then return (args, b) else fail ("Expected env named " ++ s)

command :: Show loc => TexParser loc (String, Maybe [Arg loc])
command = tok $ \case { Command c args -> Just (c, args); _ -> Nothing }

raw :: Show loc => TexParser loc String
raw = tok $ \case { Raw s -> Just s; _ -> Nothing }

reference :: Show loc => TexParser loc loc
reference = tok $ \case { Reference r -> Just r; _ -> Nothing }

braced :: Show loc => TexParser loc (Block loc)
braced = tok $ \case { Braced b -> Just b; _ -> Nothing }

anyChunk :: Show loc => TexParser loc (Chunk loc)
anyChunk = tok $ Just

satisfy :: Show loc => (Chunk loc -> Bool) -> TexParser loc (Chunk loc)
satisfy p = tok $ \c -> if p c then Just c else Nothing

-- Latex structure parsers
label = do
  ("label", Just [FixArg (Block [Raw lab])]) <- command
  return (Label lab)

labelAndStatement :: TexParser Ref (Maybe Label, TheoremStatement Ref)
labelAndStatement = 
  (,) <$> (optionMaybe label)
      <*> statement
  where

  statement = do
    ("suppose", Nothing, Block as) <- env
    assumps                        <- withInput as $ many item
    ("then", Nothing, Block rs)    <- env
    results                        <- withInput rs $ many item
    return (AssumeProve assumps results)

  item = satisfy (== NoArgCmd "item") >>
         Block <$> many (satisfy (/= NoArgCmd "item"))

proof = do
  ("proof", Nothing, Block b) <- env
  Steps <$> withInput b (many step)
  where step = undefined

theorem = do
  (kind, Just [FixArg nameBlock], Block b) <- thmEnv
  (lab, stmt) <- withInput b $ labelAndStatement
  prf         <- proof
  return (Theorem () lab kind nameBlock stmt prf)
  where
  thmEnv = do
    t@(e, _, _) <- env
    if e `elem` thmKinds
       then return t
       else fail ("Expected one of: " ++ show thmKinds)
  thmKinds = ["theorem", "lemma"]

definition = do
  ("definition", Just [FixArg nameBlock], Block b) <- env
  (lab, clauses) <- withInput b ((,) <$> optionMaybe label <*> many clause)
  return (Definition () lab nameBlock clauses)
  where
  notComment = \case { Env "comment" _ _ -> False; _ -> True }
  clause = (left . Block) <$> many (satisfy notComment)

  labelAndContent (LabelCmd lab : b) = (Just (Label lab), Block b)
  labelAndContent b                  = (Nothing, Block b)

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
