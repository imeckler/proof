{-# LANGUAGE OverloadedStrings #-}
module Compile (compile) where

import Prelude hiding (div)

import qualified Data.Text as T
import Control.Applicative
import Data.Monoid

import Types
-- This is gonna be some ugly-ass HTML. Sorry Jesus.

indent :: Int -> T.Text -> T.Text
indent n = T.unlines . map (T.replicate (2 * n) " " <>) . T.lines

tag :: T.Text -> T.Text -> [T.Text] -> T.Text
tag tagName className children =
  "<" <> tagName <> " class='" <> className <> "'>\n"
  <> T.intercalate "\n" (map (indent 1) children)
  <> "</" <> tagName <> ">"

tag' :: T.Text -> [T.Text] -> T.Text
tag' tagName children =
  "<" <> tagName <> ">\n"
  <> T.intercalate "\n" (map (indent 1) children)
  <> "</" <> tagName <> ">"

div :: T.Text -> [T.Text] -> T.Text
div = tag "div"

compileComment :: Comment -> T.Text
compileComment (Comment mayName comm) =
  div "comment" (maybe id ((:) . T.pack) mayName [T.pack comm])

compileTheoremStatement :: TheoremStatement -> T.Text
compileTheoremStatement (AssumeProve assumps results) =
  div "theorem-statement" [
    tag' "h3" ["Assume"],
    tag' "ul" (map (tag' "li" . pure . T.pack) assumps),
    tag' "h3" ["Prove"],
    tag' "ul" (map (tag' "li" . pure . T.pack) results)
  ]

maybeToList = maybe [] pure

compileSuchThat :: SuchThat -> T.Text
compileSuchThat (SuchThat conds mayProof) =
  div "suchthat"
    (div "conditions" (map compileMaybeJustified conds)
    : maybeToList ((div "justification" . pure . compileProof) <$> mayProof))

    {-
  where
  
  compileCond (cond, mayLocalProof) =
    div "list-item"
      (div "statement" [T.pack cond] :
        maybeToList (compileProof <$> mayLocalProof))
-}
compileBinder name bindings suchThat =
  div name (
    div "bindings" (map (div "binding" . pure . T.pack) bindings) :
    maybe [] (pure . compileSuchThat) suchThat
  )

compileMaybeJustified (stmt, mayJustification) =
  div "list-item"
    (div "statement" [T.pack stmt] :
     maybeToList (compileProof <$> mayJustification))

compileStep :: Step -> T.Text
compileStep (Cases cases) = div "cases" (map compileCase cases) where
  compileCase (desc, proof) =
    div "case" [ div "case-description" [T.pack desc], compileProof proof ]

compileStep (Let bindings suchThat)  =
  div "let" (
    div "bindings" (map compileMaybeJustified bindings)
    : maybeToList (compileSuchThat <$> suchThat))

compileStep (Take bindings suchThat) = compileBinder "take" bindings suchThat
compileStep (Claim stmt proof)       =
  div "claim"  (
    div "statement" [T.pack stmt] : maybeToList (compileProof <$> proof))

compileStep (Suppose assumps results mayProof) =
  div "suppose" (
    div "assumptions" (map (div "list-item" . pure . T.pack) assumps)
    : div "results" (map (div "list-item" . pure . T.pack) results)
    : maybeToList (compileProof <$> mayProof))
  

compileProof :: Proof -> T.Text
compileProof (Simple proof) = div "proof simple-proof" [T.pack proof]
compileProof (Steps steps) = div "proof steps-proof" (map compileStep steps)

compileDecl :: Declaration -> T.Text
compileDecl (Theorem name stmt proof) =
  div "theorem" [
    div "name" [T.pack name],
    compileTheoremStatement stmt,
    compileProof proof
  ]

compileDecl (Macros macros) =
  div "macros" [ "$$" <> T.pack macros <> "$$" ]

compileDecl (Definition name clauses) =
  div "definition"
    (div "name" [T.pack name] : map (either T.pack compileComment) clauses)

compile :: Document -> T.Text
compile doc = tag' "html" [
    tag' "head" [],
    tag' "body" [
      T.intercalate "\n" $ map compileDecl doc
    ]
  ]

