{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | A simple way to match a pattern on an expression.

module Match where

import qualified Data.List as List
import           Language.Haskell.TH (Q, Pat, Exp)
import qualified Language.Haskell.TH as TH

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
  pure (go e)
  where
    go =
      \case
        TH.AppE f arg
          | (TH.ConE name, args) <- unfold f [arg] ->
            TH.ConP (cleanName name) (map go args)
        TH.ConE name -> TH.ConP (cleanName name) []
        TH.RecConE name fields ->
          TH.RecP name (map (\(k, v) -> (cleanName k, go v)) fields)
        TH.LitE lit -> TH.LitP lit
        TH.ParensE e -> TH.ParensP (go e)
        TH.TupE es -> TH.TupP (map go es)
        TH.ListE es -> TH.ListP (map go es)
        TH.SigE e t -> TH.SigP (go e) t
        TH.UnboundVarE {} -> TH.WildP
        e@TH.VarE{} -> TH.ViewP (TH.InfixE Nothing (TH.VarE '(==)) (pure e)) (TH.ConP 'True [])
        e ->
          error ("Cannot convert expression to a pattern:\n" ++ show (TH.ppr e))
    unfold e args =
      case e of
        TH.AppE f a -> unfold f (a : args)
        _ -> (e, args)
    -- Clean disambiguated record fields: $sel:file:CsvImportSpec => file
    -- If we don't clean these up, TH complains about it being an invalid name:
    -- Illegal variable name: ‘$sel:file:CsvImportSpec’
    cleanName = TH.mkName . clean . TH.nameBase
      where
        clean s =
          case List.stripPrefix "$sel:" s of
            Just rest -> takeWhile (/=':') rest
            Nothing -> s
