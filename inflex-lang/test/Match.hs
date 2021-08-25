{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | A simple way to match a pattern on an expression.

module Match where

import           Data.Generics
import qualified Data.List as List
import           Data.Maybe
import           Language.Haskell.TH (Q, Pat, Exp)
import qualified Language.Haskell.TH as TH
import           Test.Hspec.Expectations

shouldReturnSatisfy :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldReturnSatisfy` expected = action >>= (`shouldSatisfy` expected)

-- | Take a pattern and see whether it matches.
match :: Q Exp -> Q Exp
match pat =
  TH.lamCaseE
    [ TH.match (expToPat pat) (TH.normalB (TH.conE 'True)) []
    , TH.match TH.wildP (TH.normalB (TH.conE 'False)) []
    ]

-- | Why from Exp instead of Pat? Because hindent and
-- structured-haskell-mode handle it better.
expToPat :: Q Exp -> Q Pat
expToPat e0 = do
  e <- e0
  pure (go (everywhere (mkT cleanName) e))
  where
    go =
      \case
        TH.AppE f arg
          | (TH.ConE name, args) <- unfold f [arg] ->
            TH.ConP name (map go args)
        TH.ConE name -> TH.ConP name []
        TH.RecConE name fields ->
          TH.RecP name (map (\(k, v) -> (k, go v)) fields)
        TH.LitE lit -> TH.LitP lit
        TH.ParensE e -> TH.ParensP (go e)
        TH.TupE es -> TH.TupP (map go es)
        TH.ListE es -> TH.ListP (map go es)
        TH.SigE e t -> TH.SigP (go e) t
        TH.UnboundVarE {} -> TH.WildP
        e ->
          TH.ViewP
            (TH.InfixE Nothing (TH.VarE '(==)) (pure e))
            (TH.ConP 'True [])
    unfold e args =
      case e of
        TH.AppE f a -> unfold f (a : args)
        _ -> (e, args)
    -- Clean disambiguated record fields: $sel:file:CsvImportSpec => file
    -- If we don't clean these up, TH complains about it being an invalid name:
    -- Illegal variable name: ‘$sel:file:CsvImportSpec’
    cleanName n = fromMaybe n (fmap TH.mkName .  clean . TH.nameBase $ n)
      where
        clean s =
          case List.stripPrefix "$sel:" s of
            Just rest -> pure (takeWhile (/= ':') rest)
            Nothing -> Nothing
