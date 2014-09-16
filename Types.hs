module Types where

type Text = String
type Statement = Text
type Name = Text

data Comment = Comment (Maybe Name) Text
  deriving Show

data TheoremStatement = AssumeProve [Text] [Text]
  deriving Show

data Proof
  = Steps [Step]
  | Simple Text
  deriving Show

data Step
  = Cases [(Text, Proof)]
  | Let [Text]
  | Claim Statement Proof
  deriving Show

data Declaration
  = Theorem Name TheoremStatement Proof
  | Definition Name [Either Text Comment]
  deriving Show

type Document = [Declaration]

