{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase, FlexibleContexts, NamedFieldPuns #-}
module Compile (compile, Resources (..)) where

import Prelude hiding (div, sequence, mapM)

import qualified Data.Text as T
import Control.Applicative
import Control.Arrow
import Data.Monoid
import Data.Functor.Coproduct
import Data.Traversable
import qualified Data.Map as M
import Control.Monad.State hiding (sequence, mapM)
import Control.Monad.Except hiding (sequence, mapM)
import Control.Monad.Identity hiding (sequence, mapM)

import Types
import DecoratedTex

-- Consider allowing trailing commas or removing commas altogether
-- as there's really no need for them.

-- Things break if there's a label inside of mathmode.

data CState = CState
  { labels :: M.Map Label (Int, Int)
  }

type Err = ExceptT String Identity
type C a = StateT CState Err a

labelString :: Label -> String
labelString (Label s) = s

insertErr :: Maybe Label -> (Int, Int) -> C ()
insertErr mayLab v = void (traverse f mayLab) where
  f lab =
    (M.lookup lab . labels <$> get) >>= \case
      Nothing -> modify (\s -> s {labels = M.insert lab v (labels s)}) -- TODO: Lensify
      Just _  -> throwError ("Duplicate label: " ++ labelString lab)

collectLabels :: [DeclarationF () Ref] -> C [DeclarationF (Int, Int) Ref]
collectLabels = sequence . zipWith goDecl [0..]
  where
  goDecl :: Int -> DeclarationF () Ref -> C (DeclarationF (Int, Int) Ref) -- M.Map Label (Int, Int)
  goDecl i (Theorem () lab kind name stmt prf) = do
    insertErr lab (0, i)
    Theorem (0, i) lab kind name stmt <$> goProof 1 prf

  goDecl i (Definition () lab name clauses) =
    Definition (0, i) lab name clauses <$ insertErr lab (0, i)

  goDecl i (CommentDecl () lab comm) =
    CommentDecl (0, i) lab comm <$ insertErr lab (0, i)

  goDecl _ (Macros macros) = return (Macros macros)

  goProof :: Int -> ProofF () Ref -> C (ProofF (Int, Int) Ref)
  goProof _ (Simple tb)  = pure (Simple tb)
  goProof d (Steps steps) = fmap Steps . sequence $ zipWith (goStep d) [0..] steps

  goStep :: Int -> Int -> StepF () Ref -> C (StepF (Int, Int) Ref)
  goStep d i (Cases () lab cases) = do
    insertErr lab (d,i)
    Cases (d,i) lab <$> mapM (traverse (goProof (d + 1))) cases

  goStep d i (Let () lab defs bc) = do
    insertErr lab (d,i)
    Let (d,i) lab <$> (traverse . traverse . traverse) (goProof (d + 1)) defs
                  <*> traverse (goSuchThat d) bc

  goStep d i (Suppose () lab assumps results prf) = do
    insertErr lab (d,i)
    Suppose (d,i) lab assumps results <$> traverse (goProof (d + 1)) prf

  goStep d i (Take () lab defs bc) = do
    insertErr lab (d,i)
    Take (d,i) lab defs <$> traverse (goSuchThat d) bc

  goStep d i (Claim () lab cla prf) = do
    insertErr lab (d,i)
    Claim (d,i) lab cla <$> traverse (goProof (d + 1)) prf

  goStep d i (CommentStep () lab comm) =
    CommentStep (d,i) lab comm <$ insertErr lab (d,i)

  goSuchThat :: Int -> SuchThatF () Ref -> C (SuchThatF (Int, Int) Ref)
  goSuchThat d (SuchThat claims prf) =
    SuchThat <$> (traverse . traverse . traverse $ goProof (d + 1)) claims
             <*> traverse (goProof (d + 1)) prf

-- Would be nice to have algebraic effects. This function only needs read
-- and error, not full state.
-- Anywho, beautiful example of the power of traversals.
locate :: 
  M.Map Label (Int,Int) -> DocumentF (Int, Int) Ref -> Err LocatedDocument
locate labs = traverse . traverse $ \r@(Ref lab) ->
  case (M.lookup (Label lab) labs) of
    Nothing  -> throwError $ "Refernce not found: " ++ lab
    Just pos -> return (r, pos)

indent :: Int -> T.Text -> T.Text
indent n = T.unlines . map (T.replicate (2 * n) " " <>) . T.lines

showT :: Show a => a -> T.Text
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

compileComment :: Comment FullLocation -> T.Text
compileComment (Comment mayName comm) =
  div "comment"
    (maybe id ((:) . div "name" . pure . compileBlock) mayName
      [div "node-content" [paragraph $ compileBlock comm]])

compileTheoremStatement :: TheoremStatement FullLocation -> T.Text
compileTheoremStatement (AssumeProve assumps results) =
  div "theorem-statement" [
    tag' "h3" ["Assume"],
    tag' "ul" (map (tag' "li" . pure . compileBlock) assumps),
    tag' "h3" ["Prove"],
    tag' "ul" (map (tag' "li" . pure . compileBlock) results)
  ]

maybeToList = maybe [] pure

compileSuchThat :: Located SuchThatF -> T.Text
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

mconcatMap f = mconcat . map f

compileBlock :: Block FullLocation -> T.Text
compileBlock = mconcatMap compileChunk . unBlock where

  compileChunk (Raw t) = t
  compileChunk (Braced b) = "{" <> compileBlock b <> "}"
  compileChunk (Env e args b) = T.concat
    [ "\\begin{", T.pack e, "}"
    , mconcatMap compileArg args
    , compileBlock b
    ]
  compileChunk (Command c args) = T.concat
    [ "\\", T.pack c, maybe "{}" (mconcatMap compileArg) args ]

  compileChunk (Reference (Ref lab, (d, i))) =
    attrTag "a" [("href", T.cons '#' (T.pack lab))] [
      "$\\langle" <> showT d <> "\\rangle" <> showT i <> "$"
    ]

  compileArg (FixArg b) = "{" <> compileBlock b <> "}"
  compileArg (OptArg b) = "[" <> compileBlock b <> "]"

posToAttr :: (Int, Int) -> T.Text
posToAttr (d, i) = showT d <> "," <> showT i

nodeDiv :: (Int, Int) -> Maybe Label -> T.Text -> [T.Text] -> T.Text
nodeDiv pos mayLabel className =
  attrTag "div" $
    ("data-pos", posToAttr pos)
    : ("class", className)
    : maybeToList (fmap (("id",) . T.pack . labelString) mayLabel)

ul = tag "ul"
li = tag "li"

compileMaybeJustified :: MaybeJustifiedF (Int, Int) FullLocation -> T.Text
compileMaybeJustified (stmt, mayJustification) =
  li "list-item"
    (div "statement" [compileBlock stmt] :
     maybeToList (compileProof <$> mayJustification))

compileStep :: Located StepF -> T.Text
compileStep (Cases pos lab cases) = nodeDiv pos lab "cases" (map compileCase cases) where
  compileCase (desc, proof) =
    div "case" [
      div "case-description" [compileBlock desc], compileProof proof
    ]

compileStep (Let pos lab bindings suchThat)  =
  nodeDiv pos lab "let" (
    ul "bindings" (map compileMaybeJustified bindings)
    : maybeToList (compileSuchThat <$> suchThat))

compileStep (Take pos lab bindings suchThat) =
  nodeDiv pos lab "take" (
    ul "bindings" (map (li "binding" . pure . compileBlock) bindings) :
    maybeToList (compileSuchThat <$> suchThat)
  )

compileStep (Claim pos lab stmt proof) =
  nodeDiv pos lab "claim"  (
    div "statement" [compileBlock stmt] : maybeToList (compileProof <$> proof))

compileStep (Suppose pos lab assumps results mayProof) =
  nodeDiv pos lab "suppose" (
    ul "assumptions" (map (li "list-item" . pure . compileBlock) assumps)
    : ul "results" (map (li "list-item" . pure . compileBlock) results)
    : maybeToList (compileProof <$> mayProof))

-- TODO: Refactor code so this shares with compileComment
compileStep (CommentStep pos lab (Comment mayName comm)) =
  nodeDiv pos lab "comment"
    (maybe id ((:) . div "name" . pure . compileBlock) mayName
      [div "node-content" [paragraph $ compileBlock comm]])

compileLocatedComment pos lab (Comment mayName comm) =
  nodeDiv pos lab "comment"
    (maybe id ((:) . div "name" . pure . compileBlock) mayName
      [div "node-content" [paragraph $ compileBlock comm]])

compileProof :: Located ProofF -> T.Text
compileProof (Simple proof) = div "proof simple-proof" [compileBlock proof]
compileProof (Steps steps)  = div "proof steps-proof" (map compileStep steps)

compileDecl :: Located DeclarationF -> T.Text
compileDecl (Theorem pos lab kind name stmt proof) =
  attrTag "div" attrs [
    div "name" [compileBlock name],
    compileTheoremStatement stmt,
    compileProof proof
  ]
  where
  attrs = ("class", "theorem") : ("data-thmkind", T.pack kind)
          : ("data-pos", posToAttr pos)
          : maybeToList (fmap (("id",) . T.pack . labelString) lab)

-- TODO: MathJax requires us to put everything in math mode so
-- input should be checked to make sure it is actually macros.
compileDecl (Macros macros) =
  div "macros" [ "$$" <> compileBlock macros <> "$$" ]

compileDecl (Definition pos lab name clauses) =
  nodeDiv pos lab "definition"
    [ div "name" [compileBlock name] 
    , div "node-content" (map (coproduct compileBlock compileComment) clauses)
    ]

compileDecl (CommentDecl pos lab comm) = compileLocatedComment pos lab comm

data Resources = Resources
  { cssFiles :: [T.Text]
  , jsFiles  :: [T.Text]
  }

toHtml :: Resources -> LocatedDocument -> T.Text
toHtml (Resources {cssFiles, jsFiles}) doc = tag' "html" [
    tag' "head" headContent,
    tag' "body" [
      T.intercalate "\n" $ map compileDecl doc
    ]
  ]
  where
  css href = "<link rel='stylesheet' type='text/css' href='" <> href <> "'>"
  headContent =
    [ css "lib/fonts/latinmodernroman_10regular_macroman/stylesheet.css"
    , css "lib/fonts/latinmodernroman_10bold_macroman/stylesheet.css"
    , css "lib/fonts/latinmodernroman_10italic_macroman/stylesheet.css"
    , css "lib/fonts/latinmodernromancaps_10regular_macroman/stylesheet.css"
    , css "lib/fonts/latinmodernromancaps_10regular_macroman/stylesheet.css"
    , css "lib/fonts/latinmodernromandemi_10regular_macroman/stylesheet.css"
    , css "lib/fonts/latinmodernromandemi_10oblique_macroman/stylesheet.css"
    ] ++ 
    map (tag' "style" . pure) cssFiles ++
    [ "<script type='text/javascript' src='lib/js/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>"
    , "<script type='text/x-mathjax-config'>"
    , "  MathJax.Hub.Config({"
    , "    tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}"
    , "  });"
    , "</script>"
    ] ++
    map (attrTag "script" [("type", "text/javascript")] . pure) jsFiles

compile :: Resources -> RawDocument -> Err T.Text
compile res doc = do
  (doc', CState labs) <- runStateT (collectLabels doc) (CState M.empty)
  toHtml res <$> locate labs doc'

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
