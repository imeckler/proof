{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
module Types where

import Data.Foldable
import Data.Traversable
import Data.Functor.Coproduct

newtype Ref   = Ref String deriving Show
newtype Label = Label String deriving (Eq, Ord, Show)

newtype TexBlock loc = TexBlock { unTexBlock :: [Either String loc] }
  deriving (Show, Functor, Traversable, Foldable)

data SuchThatF a loc = SuchThat [MaybeJustifiedF a loc] (Maybe (ProofF a loc))
  deriving (Show)

deriving instance Functor (StepF a)
deriving instance Functor (ProofF a)
deriving instance Functor (SuchThatF a)
deriving instance Functor (DeclarationF a)

deriving instance Foldable (StepF a)
deriving instance Foldable (ProofF a)
deriving instance Foldable (SuchThatF a)
deriving instance Foldable (DeclarationF a)

deriving instance Traversable (StepF a)
deriving instance Traversable (ProofF a)
deriving instance Traversable (SuchThatF a)
deriving instance Traversable (DeclarationF a)

data Comment loc = Comment (Maybe (TexBlock loc)) (TexBlock loc)
  deriving (Show, Functor, Foldable, Traversable)

data TheoremStatement loc = AssumeProve [TexBlock loc] [TexBlock loc]
  deriving (Show, Functor, Foldable, Traversable)

type MaybeJustifiedF a loc = (TexBlock loc, Maybe (ProofF a loc))

data ProofF a loc
  = Steps [StepF a loc]
  | Simple (TexBlock loc)
  deriving (Show)

type NodeData = Maybe Label

data StepF a loc
  = Cases a NodeData [(TexBlock loc, ProofF a loc)]
  | Let a NodeData [MaybeJustifiedF a loc] (Maybe (SuchThatF a loc))
  | Suppose a NodeData [TexBlock loc] [TexBlock loc] (Maybe (ProofF a loc))
  | Take a NodeData [TexBlock loc] (Maybe (SuchThatF a loc))
  | Claim a NodeData (TexBlock loc) (Maybe (ProofF a loc))
  | CommentStep a NodeData (Comment loc)
  deriving (Show)

data DeclarationF a loc
  = Theorem a NodeData String (TexBlock loc) (TheoremStatement loc) (ProofF a loc)
  | Definition a NodeData (TexBlock loc) [Coproduct TexBlock Comment loc]
  | CommentDecl a NodeData (Comment loc)
  | Macros String
  deriving (Show)

type Location = (Int, Int)

type DocumentF a loc = [DeclarationF a loc]

type Raw f = f () Ref

type FullLocation = (Ref, (Int, Int))

type Located f = f (Int, Int) FullLocation

type RawDocument        = DocumentF () Ref
type LocatedDocument    = DocumentF (Int, Int) FullLocation

