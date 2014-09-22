module TranslateTex where

import DecoratedTex
import Types
import Control.Monad.Except
import Parse.Common

translate :: Block Ref -> Except String RawDocument
translate = translate' . unBlock


translate' :: [Chunk Ref] -> Error String RawDocument
translate' (Env e block : Env "proof" prf : rest)
  | e `elem` thmKinds = do -- TODO: Labels
    Theorem () Nothing e <$> 
  where thmKinds = ["theorem", "lemma"]

