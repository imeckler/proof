{-# LANGUAGE OverloadedStrings #-}
module Compile where

import Prelude hiding (div)

import qualified Data.Text as T
import Control.Applicative
import Data.Monoid

import Types
-- This is gonna be some ugly-ass HTML. Sorry Jesus.

tag :: T.Text -> T.Text -> [T.Text] -> T.Text
tag tagName className children =
  "<" <> tagName <> " class='" <> className <> "'>"
  <> T.intercalate "\n" children
  <> "</" <> tagName <> ">"

tag' :: T.Text -> [T.Text] -> T.Text
tag' tagName children =
  "<" <> tagName <> ">"
  <> T.intercalate "\n" children
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

compileStep :: Step -> T.Text
compileStep (Cases cases) = div "cases" (map compileCase cases) where
  compileCase (desc, proof) =
    div "case" [ div "case-description" [T.pack desc], compileProof proof ]

compileStep (Let bindings) = div "let" [ 

compileProof :: Proof -> T.Text
compileProof = undefined

compileDecl :: Declaration -> T.Text
compileDecl (Theorem name stmt proof) =
  div "theorem" [
    div "name" [T.pack name],
    compileTheoremStatement stmt,
    compileProof proof
  ]

compileDecl (Definition name clauses) =
  div "definition"
    (div "name" [T.pack name] : map (either T.pack compileComment) clauses)

compile :: Document -> T.Text
compile = T.concat . map compileDecl

