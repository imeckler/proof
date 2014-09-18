{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Compile (compile) where

import Prelude hiding (div)

import qualified Data.Text as T
import Control.Applicative
import Data.Monoid
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import Types

-- Consider allowing trailing commas or removing commas altogether
-- as there's really no need for them.

-- Things break if there's a label inside of mathmode.

data PState = PState { }

type P m a = StateT PState (ExcepT String m a)


collectLabels :: [Declaration (Maybe Label)] -> M.Map Label (Int, Int)
collectLabels = undefined
  where goDecl :: Declaration (Maybe Label) -> M.Map Label (Int, Int)

indent :: Int -> T.Text -> T.Text
indent n = T.unlines . map (T.replicate (2 * n) " " <>) . T.lines

showT = T.pack . show

attrTag :: T.Text -> [(T.Text, T.Text)] -> [T.Text] -> T.Text
attrTag tagName attrs children =
  "<" <> tagName <> " " <> T.intercalate " " (map (\(k,v) -> k <> "=" <> showT v) attrs) <> ">\n"
  <> T.intercalate "\n" (map (indent 1) children)
  <> "</" <> tagName <> ">"

tag :: T.Text -> T.Text -> [T.Text] -> T.Text
tag tagName className = attrTag tagName [("class", className)]

tag' :: T.Text -> [T.Text] -> T.Text
tag' tagName = attrTag tagName []

div :: T.Text -> [T.Text] -> T.Text
div = tag "div"

paragraph :: T.Text -> T.Text
paragraph = tag' "p" . pure

compileComment :: Comment -> T.Text
compileComment (Comment mayName comm) =
  div "comment"
    (maybe id ((:) . div "name" . pure . T.pack) mayName
      [div "node-content" [paragraph $ T.pack comm]])

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
    (ul "conditions" (map compileMaybeJustified conds)
    : maybeToList ((div "justification" . pure . compileProof) <$> mayProof))

    {-
  where
  
  compileCond (cond, mayLocalProof) =
    div "list-item"
      (div "statement" [T.pack cond] :
        maybeToList (compileProof <$> mayLocalProof))
-}

ul = tag "ul"
li = tag "li"

compileBinder name bindings suchThat =
  div name (
    ul "bindings" (map (li "binding" . pure . T.pack) bindings) :
    maybeToList (compileSuchThat <$> suchThat)
  )

compileMaybeJustified (stmt, mayJustification) =
  li "list-item"
    (div "statement" [T.pack stmt] :
     maybeToList (compileProof <$> mayJustification))

compileStep :: Step -> T.Text
compileStep (Cases _ cases) = div "cases" (map compileCase cases) where
  compileCase (desc, proof) =
    div "case" [ div "case-description" [T.pack desc], compileProof proof ]

compileStep (Let _ bindings suchThat)  =
  div "let" (
    ul "bindings" (map compileMaybeJustified bindings)
    : maybeToList (compileSuchThat <$> suchThat))

compileStep (Take _ bindings suchThat) = compileBinder "take" bindings suchThat
compileStep (Claim _ stmt proof)       =
  div "claim"  (
    div "statement" [T.pack stmt] : maybeToList (compileProof <$> proof))

compileStep (Suppose _ assumps results mayProof) =
  div "suppose" (
    ul "assumptions" (map (li "list-item" . pure . T.pack) assumps)
    : ul "results" (map (li "list-item" . pure . T.pack) results)
    : maybeToList (compileProof <$> mayProof))

compileStep (CommentStep _ comment) = compileComment comment

compileProof :: Proof -> T.Text
compileProof (Simple proof) = div "proof simple-proof" [T.pack proof]
compileProof (Steps steps) = div "proof steps-proof" (map compileStep steps)

compileDecl :: Declaration -> T.Text
compileDecl (Theorem _ kind name stmt proof) =
  attrTag "div" [("class", "theorem"), ("data-thmkind", T.pack kind)]  [
    div "name" [T.pack name],
    compileTheoremStatement stmt,
    compileProof proof
  ]

compileDecl (Macros macros) =
  div "macros" [ "$$" <> T.pack macros <> "$$" ]

compileDecl (Definition _ name clauses) =
  div "definition"
    [ div "name" [T.pack name] 
    , div "node-content" (map (either T.pack compileComment) clauses)
    ]

compile :: Document -> T.Text
compile doc = tag' "html" [
    tag' "head" headContent,
    tag' "body" [
      T.intercalate "\n" $ map compileDecl doc
    ]
  ]
  where
  css href = "<link rel='stylesheet' type='text/css' href='" <> href <> "'>"
  headContent =
    [ css "proof.css"
    , css "static/fonts/latinmodernroman_10regular_macroman/stylesheet.css"
    , css "static/fonts/latinmodernroman_10bold_macroman/stylesheet.css"
    , css "static/fonts/latinmodernroman_10italic_macroman/stylesheet.css"
    , css "static/fonts/latinmodernromancaps_10regular_macroman/stylesheet.css"
    , css "static/fonts/latinmodernromancaps_10regular_macroman/stylesheet.css"
    , css "static/fonts/latinmodernromandemi_10regular_macroman/stylesheet.css"
    , css "static/fonts/latinmodernromandemi_10oblique_macroman/stylesheet.css"
    , "<script src='jquery.min.js'></script>"
    , "<script type='text/javascript' src='http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>"
    , "<script type='text/x-mathjax-config'>"
    , "  MathJax.Hub.Config({"
    , "    tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}"
    , "  });"
    , "</script>"
    , "<script type='text/javascript' src='proof.js'></script>"
    ]

{-
tag' :: T.Text -> [T.Text] -> T.Text
tag' tagName children =
  "<" <> tagName <> ">\n"
  <> T.intercalate "\n" (map (indent 1) children)
  <> "</" <> tagName <> ">"
-}
{-
tag :: T.Text -> T.Text -> [T.Text] -> T.Text
tag tagName className children =
  "<" <> tagName <> " class='" <> className <> "'>\n"
  <> T.intercalate "\n" (map (indent 1) children)
  <> "</" <> tagName <> ">"
-}
