module Latex where

-- | An argument for a 'LaTeX' command or environment.
data Arg
 = FixArg Block    -- ^ Fixed argument.
 | OptArg Block    -- ^ Optional argument.
 deriving (Show)

data Chunk
  = Raw String
  | Env String (Maybe [Arg]) Block
  | Braced Block
  | Command String (Maybe [Arg]) -- TODO: Support different kinds of args properly.
  deriving (Show)

newtype Block = Block { unBlock :: [Chunk] }
  deriving (Show)

