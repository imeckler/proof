{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Types where

import Data.Foldable
import Data.Traversable

newtype Ref   = Ref String deriving Show
newtype Label = Label String deriving Show

type TexBlock  = [Either String Ref]

data SuchThat = SuchThat [MaybeJustified TexBlock] (Maybe Proof)
  deriving Show

data Comment = Comment (Maybe TexBlock) TexBlock
  deriving (Show)

data TheoremStatement = AssumeProve [TexBlock] [TexBlock]
  deriving Show

type MaybeJustified a = (a, Maybe Proof)

data Proof
  = Steps [Step]
  | Simple TexBlock
  deriving Show

type StepData = Maybe Label
type DeclarationData = Maybe Label

data StepF a
  = Cases a [(TexBlock, Proof)]
  | Let a [MaybeJustified TexBlock] (Maybe SuchThat)
  | Suppose a [TexBlock] [TexBlock] (Maybe Proof)
  | Take a [TexBlock] (Maybe SuchThat)
  | Claim a TexBlock (Maybe Proof)
  | CommentStep a Comment
  deriving (Show, Functor, Foldable, Traversable)

type Step = StepF StepData

data DeclarationF a
  = Theorem a String TexBlock TheoremStatement Proof
  | Definition a TexBlock [Either TexBlock Comment]
  | CommentDecl a Comment
  | Macros String
  deriving (Show, Functor, Foldable, Traversable)

type Declaration = DeclarationF DeclarationData

type Location = (Int, Int)

type Document = [Declaration]

