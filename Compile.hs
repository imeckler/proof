{-# LANGUAGE OverloadedStrings,
             TupleSections,
             LambdaCase,
             FlexibleContexts,
             NamedFieldPuns,
             RecordWildCards,
             ViewPatterns,
             ScopedTypeVariables #-}
module Compile (Resources(..), compile) where

import Prelude hiding (div, sequence, mapM)

import qualified Data.Text as T
import Control.Applicative
import Data.Monoid
import Data.Functor.Coproduct
import Data.Traversable
import qualified Data.Map as M
import Control.Monad.State hiding (sequence, mapM)
import Control.Monad.Except hiding (sequence, mapM)
import Utils
import Types
import DecoratedTex

-- Consider allowing trailing commas or removing commas altogether
-- as there's really no need for them.

-- Things break if there's a label inside of mathmode.

data CState = CState
  { labels :: M.Map Label (Int, Int)
  }

type C m a = StateT CState (Err m) a

labelString :: Label -> String
labelString (Label s) = s

insertErr :: (Monad m, Functor m) => Maybe Label -> (Int, Int) -> C m ()
insertErr mayLab v = void (traverse f mayLab) where
  f lab =
    (M.lookup lab . labels <$> get) >>= \case
      Nothing -> modify (\s -> s {labels = M.insert lab v (labels s)}) -- TODO: Lensify
      Just _  -> throwError ("Duplicate label: " ++ labelString lab)

collectLabels :: (Monad m, Functor m) => [DeclarationF () Ref] -> C m [DeclarationF (Int, Int) Ref]
collectLabels = sequence . zipWith goDecl [0..]
  where
  goDecl :: (Monad m, Functor m) => Int -> DeclarationF () Ref -> C m (DeclarationF (Int, Int) Ref) -- M.Map Label (Int, Int)
  goDecl i (Theorem () lab kind name stmt prf) = do
    insertErr lab (0, i)
    Theorem (0, i) lab kind name stmt <$> goProof 1 prf

  goDecl i (Definition () lab name clauses) =
    Definition (0, i) lab name clauses <$ insertErr lab (0, i)

  goDecl i (CommentDecl () lab comm) =
    CommentDecl (0, i) lab comm <$ insertErr lab (0, i)

  goDecl _ (Macros macros) = return (Macros macros)

  goProof :: (Monad m, Functor m) => Int -> ProofF () Ref -> C m (ProofF (Int, Int) Ref)
  goProof _ (Simple tb)  = pure (Simple tb)
  goProof d (Steps steps) = fmap Steps . sequence $ zipWith (goStep d) [0..] steps

  goStep :: (Monad m, Functor m) => Int -> Int -> StepF () Ref -> C m (StepF (Int, Int) Ref)
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

  goSuchThat :: (Monad m, Functor m) => Int -> SuchThatF () Ref -> C m (SuchThatF (Int, Int) Ref)
  goSuchThat d (SuchThat claims prf) =
    SuchThat <$> (traverse . traverse . traverse $ goProof (d + 1)) claims
             <*> traverse (goProof (d + 1)) prf

data Resources = Resources
  { cssFiles :: [T.Text]
  , jsFiles  :: [T.Text]
  }

-- Would be nice to have algebraic effects. This function only needs read
-- and error, not full state.
-- Anywho, beautiful example of the power of traversals.
locate :: (Monad m, Functor m) =>
  M.Map Label (Int,Int) -> DocumentF (Int, Int) Ref -> Err m LocatedDocument
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
  "<" <> tagName <> sep <> T.intercalate " " (map (\(k,v) -> k <> "=" <> showT v) attrs) <> ">\n"
  <> T.intercalate "\n" (map (indent 1) children)
  <> "</" <> tagName <> ">"
  where sep = case attrs of { [] -> ""; _ -> " " }

tag :: T.Text -> T.Text -> [T.Text] -> T.Text
tag tagName className = attrTag tagName [("class", className)]

tag' :: T.Text -> [T.Text] -> T.Text
tag' tagName = attrTag tagName []

div :: T.Text -> [T.Text] -> T.Text
div = tag "div"

paragraph :: T.Text -> T.Text
paragraph = tag' "p" . pure

compileComment :: (Monad m, Applicative m) => Comment FullLocation -> Err m T.Text
compileComment (Comment mayName comm) =
  (\mayNameBlock commBlock ->
    div "comment"
      (maybe id ((:) . div "name" . pure) mayNameBlock
        [div "node-content" [paragraph commBlock]]))
  <$> traverse compileBlock mayName <*> compileBlock comm

compileTheoremStatement :: (Monad m, Applicative m) => TheoremStatement FullLocation -> Err m T.Text
compileTheoremStatement (AssumeProve assumps results) =
  (\as rs ->
    div "theorem-statement" [
      tag' "h3" ["Assume"],
      tag' "ul" (map (tag' "li" . pure) as),
      tag' "h3" ["Prove"],
      tag' "ul" (map (tag' "li" . pure) rs)
    ])
  <$> mapM compileBlock assumps <*> mapM compileBlock results

maybeToList = maybe [] pure

compileSuchThat :: (Monad m, Applicative m) => Located SuchThatF -> Err m T.Text
compileSuchThat (SuchThat conds mayProof) =
  (\cs p -> div "suchthat"
      (ul "conditions" cs : maybeToList (div "justification" . pure <$> p)))
  <$> mapM compileMaybeJustified conds <*> traverse compileProof mayProof

mconcatMap f  = mconcat . map f
mconcatMapM f = liftM mconcat . sequence . map f

type CmdInterpreter m = Maybe [Arg FullLocation] -> Err m T.Text

compileBlock :: (Monad m, Applicative m) => Block FullLocation -> Err m T.Text
compileBlock = mconcatMapM compileChunk . unBlock where

  compileChunk (Raw t)        = return t
  compileChunk (Braced b)     = fmap (\s -> "{" <> s <> "}") (compileBlock b)

  compileChunk (Env e args b) = case M.lookup e specialEnvs of
    Just interp -> interp args b
    Nothing     -> 
      fmap ((\x -> beg <> x <> end) . T.concat) $ sequence
        [ compileArgs args
        , compileBlock b
        ]
    where beg = "\\begin{" <> T.pack e <> "}"
          end = "\\end{" <> T.pack e <> "}"

  compileChunk (Command c args) = case M.lookup c specialCommands of
    Nothing     -> fmap (\s -> "\\" <> T.pack c <> s) argStr
    Just interp -> interp args
    where argStr = maybe (return "") (\case { [] -> return "{}"; as -> compileArgs as }) args

  compileChunk (Reference (Ref lab, (d, i))) = return $
    attrTag "a" [("href", T.cons '#' (T.pack lab))] [
      "$\\langle" <> showT d <> "\\rangle" <> showT i <> "$"
    ]

  compileArg (FixArg b) = fmap (\x -> "{" <> x <> "}") (compileBlock b)
  compileArg (OptArg b) = fmap (\x -> "[" <> x <> "]") (compileBlock b)

  compileArgs = mconcatMapM compileArg

  specialCommands = M.fromList
    [ ("emph", emph)
    ]
    where
    -- TODO: Check what \emph should do given no arguments
    emph mayArgs = may (return "") mayArgs $ \case
      []         -> return ""
      [FixArg x] -> fmap (tag "span" "emph" . pure) (compileBlock x)
      _          -> throwError "Macro \"\\emph\" takes at most one argument"

  specialEnvs :: (Applicative m, Monad m) => M.Map String ([Arg FullLocation] -> Block FullLocation -> Err m T.Text)
  specialEnvs = M.fromList
    [("itemize", itemize)
    ]
    where
    itemize _ = fmap (ul "itemize") . (mapM item <=< group) . skipSpace . unBlock where
      item = fmap (\b -> tag' "li" [b]) . compileBlock

      group []                                               = pure []
      group (Command "item" _ : (break isItem -> (x, rest))) = (Block x :) <$> group rest
      group _                                                = throwError "Unexpected text in itemize"

      skipSpace = dropWhile spaceChunk
      isItem = \case {Command "item" _ -> True; _ -> False}
   

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

compileMaybeJustified :: (Monad m, Applicative m) => MaybeJustifiedF (Int, Int) FullLocation -> Err m T.Text
compileMaybeJustified (stmt, mayJustification) =
  (\stmtBlock justifBlock ->
    (li "list-item"
      (div "statement" [stmtBlock] : maybeToList justifBlock)))
  <$> compileBlock stmt <*> traverse compileProof mayJustification

compileStep :: (Monad m, Applicative m) => Located StepF -> Err m T.Text
compileStep (Cases pos lab cases) = nodeDiv pos lab "cases" <$> mapM compileCase cases where
  compileCase (desc, proof) =
    (\d p -> div "case" [div "case-description" [d], p])
    <$> compileBlock desc <*> compileProof proof

compileStep (Let pos lab bindings suchThat)  =
  (\bs st -> nodeDiv pos lab "let" (ul "bindings" bs : maybeToList st))
  <$> mapM compileMaybeJustified bindings <*> traverse compileSuchThat suchThat

compileStep (Take pos lab bindings suchThat) =
  (\bs st -> nodeDiv pos lab "take" (
      ul "bindings" (map (li "binding" . pure) bs) : maybeToList st))
  <$> mapM compileBlock bindings <*> traverse compileSuchThat suchThat


compileStep (Claim pos lab stmt proof) =
  (\s prf -> nodeDiv pos lab "claim"  (div "statement" [s] : maybeToList prf))
  <$> compileBlock stmt <*> traverse compileProof proof

compileStep (Suppose pos lab assumps results mayProof) =
  (\as rs prf -> nodeDiv pos lab "suppose" (
      ul "assumptions" (map (li "list-item" . pure) as)
    : ul "results" (map (li "list-item" . pure) rs)
    : maybeToList prf))
  <$> mapM compileBlock assumps
  <*> mapM compileBlock results
  <*> traverse compileProof mayProof

-- TODO: Refactor code so this shares with compileComment
compileStep (CommentStep pos lab (Comment mayName comm)) =
  (\n c ->
  nodeDiv pos lab "comment"
    (maybe id ((:) . div "name" . pure) n
      [div "node-content" [paragraph c]]))
  <$> traverse compileBlock mayName
  <*> compileBlock comm

compileLocatedComment pos lab (Comment mayName comm) =
  (\n c ->
  nodeDiv pos lab "comment"
    (maybe id ((:) . div "name" . pure) n
      [div "node-content" [paragraph c]]))
  <$> traverse compileBlock mayName
  <*> compileBlock comm

compileProof :: (Monad m, Applicative m) => Located ProofF -> Err m T.Text
compileProof (Simple proof) = (\p -> div "proof simple-proof" [p]) <$> compileBlock proof
compileProof (Steps steps)  = (div "proof steps-proof") <$> mapM compileStep steps

compileDecl :: (Monad m, Applicative m) => Located DeclarationF -> Err m T.Text
compileDecl (Theorem pos lab kind name stmt proof) =
  (\n s prf -> attrTag "div" attrs [ div "name" [n], s, prf ])
  <$> compileBlock name <*> compileTheoremStatement stmt <*> compileProof proof
  where
  attrs = ("class", "theorem") : ("data-thmkind", T.pack kind)
          : ("data-pos", posToAttr pos)
          : maybeToList (fmap (("id",) . T.pack . labelString) lab)

-- TODO: MathJax requires us to put everything in math mode so
-- input should be checked to make sure it is actually macros.
compileDecl (Macros macros) =
  (\ms -> div "macros" [ "$$\n" <> ms <> "$$" ])
  <$> compileBlock macros 

compileDecl (Definition pos lab name clauses) =
  (\n cs -> nodeDiv pos lab "definition" [ div "name" [n], div "node-content" cs ])
  <$> compileBlock name <*> mapM (coproduct compileBlock compileComment) clauses

compileDecl (CommentDecl pos lab comm) = compileLocatedComment pos lab comm

toHtml :: (Monad m, Applicative m) => Resources -> LocatedDocument -> Err m T.Text
toHtml (Resources {..}) doc =
  (\ds -> tag' "html" [
    tag' "head" headContent,
    tag' "body" [
      T.intercalate "\n" ds
    ]
  ]) <$> mapM compileDecl doc
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
    , css "css/proof.css"
    ]
    ++ map (tag' "style" . pure) cssFiles ++
    [ "<script type='text/javascript' src='lib/js/MathJax/MathJax.js?config=TeX-AMS_HTML'></script>"
    , "<script type='text/x-mathjax-config'>"
    , "  MathJax.Hub.Config({"
    , "    tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]},"
    , "    'HTML-CSS': {"
    , "      imageFont: null,"
    , "      availableFonts: ['STIX', 'TeX', 'Latin-Modern'],"
    , "      preferredFont: 'TeX',"
    , "      webFont: 'TeX'"
    , "    }"
    , "  });"
    , "</script>"
    ]
    ++ map (attrTag "script" [("type", "text/javascript")] . pure) jsFiles

--TODO : remove monadio
compile :: (Monad m, MonadIO m, Applicative m) => Resources -> RawDocument -> Err m T.Text
compile res doc = do
  liftIO $ print doc
  (doc', CState labs) <- runStateT (collectLabels doc) (CState M.empty)
  toHtml res =<< locate labs doc'

