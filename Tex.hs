{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Tex where

import Data.Foldable
import Data.Traversable

newtype Ref = Ref String deriving Show

data Chunk loc
  = Raw String
  | Env String (Block loc)
  | Command String (Maybe [Block loc]) -- TODO: Support different kinds of args properly.
  | Reference loc
  deriving (Show, Functor, Foldable, Traversable)

newtype Block loc = Block { unBlock :: [Chunk loc] }
  deriving (Show, Functor, Foldable, Traversable)

