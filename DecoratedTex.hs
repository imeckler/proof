{-# LANGUAGE DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable,
             StandaloneDeriving,
             FlexibleInstances,
             MultiParamTypeClasses #-}
module DecoratedTex where

import Prelude hiding (mapM)
import Data.Foldable
import Data.Traversable
import qualified Latex as L
import Control.Monad.Except hiding (mapM)
import Control.Applicative
import Utils
import Text.Parsec.Prim

newtype Ref = Ref String deriving (Show, Eq)

data Chunk loc
  = Raw String
  | Braced (Block loc)
  | Env String (Maybe [Arg loc]) (Block loc)
  | Command String (Maybe [Arg loc])
  | Reference loc
  deriving (Show, Functor, Foldable, Traversable, Eq)

data Arg loc
  = FixArg (Block loc)
  | OptArg (Block loc)
  deriving (Show, Functor, Foldable, Traversable, Eq)

newtype Block loc = Block { unBlock :: [Chunk loc] }
  deriving (Show, Functor, Foldable, Traversable, Eq)

decorate :: L.Block -> Except String (Block Ref)
decorate = fmap Block . mapM goChunk . L.unBlock where

  goChunk (L.Raw s)               = pure $ Raw s
  goChunk (L.Env name args block) = Env (strip name) <$> goArgs args
                                                     <*> decorate block
  goChunk (L.Braced block)        = Braced <$> decorate block

  goChunk (L.Command "ref" (Just [L.FixArg (L.Block [L.Raw lab])])) =
    pure $ Reference (Ref (strip lab))

  goChunk (L.Command "ref" _) = throwError "ref expects exactly one argument"
  goChunk (L.Command c args) = Command (strip c) <$> goArgs args

  goArgs = traverse (mapM goArg)

  goArg (L.FixArg b) = FixArg <$> decorate b
  goArg (L.OptArg b) = OptArg <$> decorate b

