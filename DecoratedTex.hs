{-# LANGUAGE DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable #-}

module DecoratedTex where

import Prelude hiding (mapM)
import Data.Foldable hiding (all)
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

decorate :: (Monad m, Functor m) => L.LaTeX -> ExceptT String m (Block Ref)
decorate = fmap Block . mapM goChunk . flatten
  where
  flatten (L.TeXSeq x y)   = flatten x ++ flatten y
  flatten (L.TeXComment _) = []
  flatten L.TeXEmpty       = []
  flatten t                = [t]

  -- TODO: Add support for various commands a la pandoc
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

spaceChunk :: Chunk loc -> Bool
spaceChunk (Raw s)            = T.all isSpace s
spaceChunk (Braced (Block b)) = all spaceChunk b
spaceChunk (Env {})           = False
spaceChunk (Command {})       = False
spaceChunk (Reference _)      = False

