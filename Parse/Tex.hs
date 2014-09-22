module Tex where

-- import Text.Pandoc.Readers.LaTeX
import Types

import Parse.Common

assumeProve = undefined

-- env name inner
env' :: Parser String -> Parser a -> Parser (String, a)
env' name p = do
  envName <- envOpen
  inner <- p <* symbol "\\close" <* symbol "{" <* symbol envName <* symbol "}"
  return (envName, inner)
  where
  envOpen = symbol "\\begin" *> symbol "{" *> name <* symbol "}"

env = env' . symbol

-- TODO: Simple proof
proof = fmap (Steps . snd) . env "proof" $ many step

-- If balanced is inefficient, use a Seq or something
oneArgCommand s = do
  symbol ('\\' : s)
  symbol "{"
  content <- balanced
  symbol "}"
  return content
  where
  balanced = do
    c <- anyChar
    case c of
      '{' -> do
        rest <- balanced 
        char '}'
        return (rest ++ "}")
      '}' -> pure ""
      _ -> (c:) <$> balanced


-- TODO: Labels
theorem = do
  (kind, (name, stmt)) <- theoremDecl
  prf <- proof
  return (Theorem () Nothing kind name stmt prf)
  where
  theoremStatement :: Parser (TheoremStatement Ref)
  theoremStatement = assumeProve

  theoremDecl = env' (symbol "theorem" <|> symbol "lemma") $ do
    name <- between (symbol "{") (symbol "}") texBlock
    stmt <- theoremStatement
    return (name, stmt)

step :: Parser (Raw StepF)
step = undefined
  where
  cases = fmap (Cases () Nothing) . env "cases" . many $ do
    oneArgCommand "case"

    
