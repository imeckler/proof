module Types where

type Text = String
type Statement = Text
type Name = Text

data SuchThat = SuchThat [MaybeJustified Text] (Maybe Proof)
  deriving Show

data Comment = Comment (Maybe Name) Text
  deriving Show

data TheoremStatement = AssumeProve [Text] [Text]
  deriving Show

type MaybeJustified a = (a, Maybe Proof)

data Proof
  = Steps [Step]
  | Simple Text
  deriving Show

data Step
  = Cases [(Text, Proof)]
  | Let [MaybeJustified Text] (Maybe SuchThat)
  | Suppose [Text] [Text] (Maybe Proof)
  | Take [Text] (Maybe SuchThat)
  | Claim Statement (Maybe Proof)
  | CommentStep Comment
  deriving Show

data Declaration
  = Theorem Name TheoremStatement Proof
  | Definition Name [Either Text Comment]
  | Macros Text
  deriving Show

type Document = [Declaration]

