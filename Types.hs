{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Types where

import Data.Foldable
import Data.Traversable

newtype Ref   = Ref String deriving Show
newtype Label = Label String deriving (Eq, Ord, Show)

type TexBlock = [Either String Ref]

data SuchThatF a = SuchThat [MaybeJustifiedF a TexBlock] (Maybe (ProofF a))
  deriving Show

data Comment = Comment (Maybe TexBlock) TexBlock
  deriving (Show)

data TheoremStatement = AssumeProve [TexBlock] [TexBlock]
  deriving Show

type MaybeJustifiedF a p = (p, Maybe (ProofF a))

data ProofF a
  = Steps [StepF a]
  | Simple TexBlock
  deriving Show

type SuchThat = SuchThatF StepData
type Proof = ProofF StepData
type Step = StepF StepData
type Declaration = DeclarationF DeclarationData

type StepData = Maybe Label
type DeclarationData = Maybe Label

data StepF a
  = Cases a [(TexBlock, ProofF a)]
  | Let a [MaybeJustifiedF a TexBlock] (Maybe (SuchThatF a))
  | Suppose a [TexBlock] [TexBlock] (Maybe (ProofF a))
  | Take a [TexBlock] (Maybe (SuchThatF a))
  | Claim a TexBlock (Maybe (ProofF a))
  | CommentStep a Comment
  deriving (Show, Functor, Foldable, Traversable) -- GHC bug. Loading this file causes (!!) index too large error

data DeclarationF a
  = Theorem a String TexBlock TheoremStatement (ProofF a)
  | Definition a TexBlock [Either TexBlock Comment]
  | CommentDecl a Comment
  | Macros String
  deriving (Show, Functor, Foldable, Traversable)

type Location = (Int, Int)

type Document = [Declaration]

