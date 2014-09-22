{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, StandaloneDeriving #-}
module Types where

import Data.Foldable
import Data.Traversable
import Data.Functor.Coproduct
import qualified DecoratedTex as Tex
-- TODO: Delete import of LaTeX
import Text.LaTeX.Base.Parser

newtype Label = Label String deriving (Eq, Ord, Show)

{-
data Chunk loc
  = Raw String
  | Env String (TexBlock loc)
  | Reference loc
  deriving (Show, Functor

newtype TexBlock loc = TexBlock { unTexBlock :: [Chunk loc] }
  deriving (Show, Functor, Traversable, Foldable)
-}
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

data Comment loc = Comment (Maybe (Tex.Block loc)) (Tex.Block loc)
  deriving (Show, Functor, Foldable, Traversable)

data TheoremStatement loc = AssumeProve [Tex.Block loc] [Tex.Block loc]
  deriving (Show, Functor, Foldable, Traversable)

type MaybeJustifiedF a loc = (Tex.Block loc, Maybe (ProofF a loc))

data ProofF a loc
  = Steps [StepF a loc]
  | Simple (Tex.Block loc)
  deriving (Show)

type NodeData = Maybe Label

data StepF a loc
  = Cases a NodeData [(Tex.Block loc, ProofF a loc)]
  | Let a NodeData [MaybeJustifiedF a loc] (Maybe (SuchThatF a loc))
  | Suppose a NodeData [Tex.Block loc] [Tex.Block loc] (Maybe (ProofF a loc))
  | Take a NodeData [Tex.Block loc] (Maybe (SuchThatF a loc))
  | Claim a NodeData (Tex.Block loc) (Maybe (ProofF a loc))
  | CommentStep a NodeData (Comment loc)
  deriving (Show)

data DeclarationF a loc
  = Theorem a NodeData String (Tex.Block loc) (TheoremStatement loc) (ProofF a loc)
  | Definition a NodeData (Tex.Block loc) [Coproduct Tex.Block Comment loc]
  | CommentDecl a NodeData (Comment loc)
  | Macros String
  deriving (Show)

type Location = (Int, Int)

type DocumentF a loc = [DeclarationF a loc]

type Raw f = f () Tex.Ref

type FullLocation = (Tex.Ref, (Int, Int))

type Located f = f (Int, Int) FullLocation

type RawDocument        = DocumentF () Tex.Ref
type LocatedDocument    = DocumentF (Int, Int) FullLocation

