{-# LANGUAGE DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable #-}

module DecoratedTex where

import Prelude hiding (mapM)
import Data.Foldable
import Data.Traversable
import Control.Monad.Except hiding (mapM)
import Control.Applicative
import Utils
import qualified Text.LaTeX.Base.Syntax as L
import Text.LaTeX.Base.Render
import qualified Data.Text as T
import Data.Char

newtype Ref = Ref String deriving (Show, Eq)

data Chunk loc
  = Raw Text
  | Braced (Block loc)
  | Env String [Arg loc] (Block loc)
  | Command String (Maybe [Arg loc])
  | Reference loc
  deriving (Show, Functor, Foldable, Traversable, Eq)

data Arg loc
  = FixArg (Block loc)
  | OptArg (Block loc)
  deriving (Show, Functor, Foldable, Traversable, Eq)

newtype Block loc = Block { unBlock :: [Chunk loc] }
  deriving (Show, Functor, Foldable, Traversable, Eq)

decorate :: L.LaTeX -> Except String (Block Ref)
decorate = fmap Block . mapM goChunk . flatten
  where
  flatten (L.TeXSeq x y)   = flatten x ++ flatten y
  flatten (L.TeXComment _) = []
  flatten L.TeXEmpty       = []
  flatten t                = [t]

  goChunk (L.TeXRaw s)                   = pure $ Raw s
  goChunk (L.TeXComm "ref" [L.FixArg r]) = Reference . Ref <$> extractLabel r
  goChunk (L.TeXComm s args)             = Command (strip s) . Just <$> goArgs args
  goChunk (L.TeXCommS s)                 = pure $ Command (strip s) Nothing
  goChunk (L.TeXEnv e args t)            = Env e <$> goArgs args <*> decorate t
  goChunk t                              = pure (Raw (render t))

  -- TODO: Line numbers
  extractLabel (L.TeXRaw s) -- TODO: Check if this is too restrictive on labels
    | T.all isAlphaNum s = pure . strip $ T.unpack s
  extractLabel _ = throwError "Invalid argument to ref"

  goArgs = mapM goArg

  goArg (L.FixArg t) = FixArg <$> decorate t
  goArg (L.OptArg t) = OptArg <$> decorate t
  goArg _            = error "DecoratedTex.decorate: arg translation not implemented"

{-
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
-}
