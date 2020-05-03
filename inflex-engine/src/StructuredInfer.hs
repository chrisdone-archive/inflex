{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators, LambdaCase, StandaloneDeriving, GADTs #-}

module StructuredInfer where

import Control.Applicative.Free
import Control.Monad.Free
import Control.Monad.State
import Data.List
import Data.Typeable
import Numeric.Natural
import Text.Printf

--------------------------------------------------------------------------------
-- Types

data Type env kind where
  -- A constant type. Add kinds?
  -- Maybe, (), Int, Either, (->), etc.
  Constructor :: String -> kind -> Type env kind
  -- A free variable.
  -- a
  FreeVariable :: Natural -> kind -> Type env kind
  -- Type application.
  -- Maybe a
  Application :: (Show ki, Show ko) => Type env (ki -> ko) -> Type env ki -> Type env ko
  -- A generic variable used in a forall scheme.
  -- Rigid variables that will not unify with free variables.
  GenericVariable :: (env ~ (t, env')) => Var env kind -> Type env kind
  -- Forall scheme:
  -- forall. a -> ... a ...
  -- Forall :: (Show b, env' ~ (a,env)) => Type (x,env') b -> Type env' b
  -- -- This wouldn't be writable by a user.
  -- -- E.g. Show a => a -> String
  -- -- Instantiate with: Int
  -- -- Int -> String
  -- InstantiateForall :: Type env (Forall a b) -> Type env a -> Type env b
  -- -- Class qualification
  -- -- E.g. Show a => a -> String
  -- Qualify :: c -> Var env x -> Type env o -> Type env ((c, x) :=> o)
  -- -- Row types.
  -- -- E.g. { a :: Int, b :: String }
  -- Row :: Row env -> Type env RowKind

data SchemeOrType env kind where
  SchemeType :: SchemeOrType (a, env) kind -> SchemeOrType env kind
  TypeType :: Type env kind -> SchemeOrType env kind
instance Show (SchemeOrType e k) where show _ = "s"

functionCons :: Type e (() -> () -> ())
functionCons = undefined {-Constructor "(->)" (TypeK :*-> (TypeK :*-> TypeK))-}

functionType :: Type e TypeK -> Type e TypeK -> Type e TypeK
functionType i o = undefined {-Application (Application functionCons i) o-}

instance Show (Type e k) where
  show t0 = go 0 t0
    where
      go ::
           forall e k. Int
        -> Type e k
        -> String
      go nesting =
        \case
          Constructor str kind -> "" ++ str ++ "" -- " :: " ++ show kind ++
          FreeVariable nat kind -> "v" ++ show nat ++ "" -- ++ " :: " ++ show kind
          Application (Application (Constructor "(->)" _) x) y ->
            "(" ++ go nesting x ++ " -> " ++ go nesting y ++ ")"
          Application typeF typeX ->
            "(" ++ show typeF ++ " " ++ indent (show typeX) ++ ")"
          -- Forall t ->
          --   "(forall _" ++ show nesting ++ ". " ++ go (nesting + 1) t ++ ")"
          GenericVariable var -> "_" ++ show (varnesting var)

varnesting :: Var env t -> Int
varnesting =
  \case
     VZ -> 0
     VS v -> 1 + varnesting v

indent :: String -> String
indent = intercalate "\n" . map ("  " ++) . lines

--------------------------------------------------------------------------------
-- Kinds

data Kind a where
  TypeK :: Kind ()
  (:*->) :: Kind a -> Kind b -> Kind (a -> b)
instance Show (Kind a) where show _ = "Kind"
type TypeK = Kind ()

--------------------------------------------------------------------------------
-- Vars
data Var env t where
  VZ :: Var (t, env) t
  VS :: Var env t -> Var (a, env) t
deriving instance Show (Var env t)

--------------------------------------------------------------------------------
-- Expression

data E s
  = CE (SomeKindedType TypeK) -- this needs to be an arbitrary thing
  | LE (SType s (Type () TypeK)) Natural (E  s)
  | VE (SType s (Type () TypeK)) Natural
  | AE (SType s (Type () TypeK)) (E  s) (E  s)

eType :: E 'PostInfer -> SomeKindedType TypeK
eType =
  \case
    CE t -> t
    LE t _ _ -> SomeKindedType (TypeType t)
    VE t _ -> SomeKindedType (TypeType  t)
    AE t _ _ -> SomeKindedType (TypeType  t)

instance Show (E  'PreInfer) where
  show = \case
            CE t -> "(_ :: " ++ show t ++ ")"
            LE t p e -> "(\\p" ++ show p ++ " -> " ++ show e ++ ")"
            VE t v -> "p"++ show v
            AE t f x -> "(" ++ show f ++ " " ++ show x ++ ")"
instance Show (E  'PostInfer) where
  show = \case
            CE t -> "(_ :: " ++ show t ++ ")"
            LE t p e -> "((\\p" ++ show p ++ " -> " ++ show e ++ ") :: " ++ show t ++ ")"
            VE t v -> "(p" ++ show v ++ " :: " ++ show t ++ ")"
            AE t f x -> "((" ++ show f ++ " " ++ show x ++ ") :: " ++ show t ++ ")"

--------------------------------------------------------------------------------
-- Stages

data Stage = PreInfer | PostInfer

type family SType s t where
  SType 'PreInfer t = Maybe t
  SType 'PostInfer t = t

--------------------------------------------------------------------------------
-- Infer algebra

data InferStep a where
  NewVT :: (Typeable env, Typeable k, Show k) => k -> InferStep (Type env k)
  BindTV :: (Typeable env) => Natural -> Type env TypeK -> InferStep ()
  LookupP :: Natural -> InferStep (SomeKindedType TypeK)
  Equiv :: (Show k) => Type env k -> Type env k -> InferStep ()
deriving instance Show (InferStep a)

data SomeKindedType k =
  forall env. (Typeable env) =>
                SomeKindedType (SchemeOrType env k)
deriving instance Show k => Show (SomeKindedType k)

--------------------------------------------------------------------------------
-- Inferer

-- TODO:
--
-- We need to distinguish between
--
-- * /Defining/ an expression whose type is forall'd (make sure types are polymorphic)
-- * /Using/ a variable whose type is forall'd (instantiate scheme with fresh variables)
--
-- And make sure that this situation makes sense (this is a case of /using/):
--
--                     (Int -> v3)   ≡   v0 <--- this is a lambda parameter,
--                    (Char -> v4)   ≡   v0 <--- presenty we don't know its type because
                                             --- we don't provide a sig for the param,
                                             --- nor do we figure it out based on the
                                             --- lambda's type (we could)
--                      (v4 -> v1)   ≡   v2
--                      (v3 -> v2)   ≡   (forall _0. (_0 -> (forall _1. (_1 -> ()))))
-- ((forall _0. (_0 -> _0)) -> ())   ≡   (v0 -> v1)

inferE :: E  'PreInfer -> Infer (E  'PostInfer)
inferE = go
  where
    go =
      \case
        CE t -> pure (CE t)
        LE msig p e -> do
          pt <- liftStep (NewVT TypeK)
          liftStep (BindTV p pt)
          e' <- go e
          let lt = functionType pt (undefined (eType e'))
          lt' <-
            case msig of
              Nothing -> pure lt
              Just t -> do
                liftStep (Equiv t lt)
                pure t
          pure (LE lt' p e')
        VE msig p -> do
          sky <- liftStep (LookupP p)
          t <- instantiate sky
          case msig of
            Nothing -> pure (VE t p)
            Just sigt -> do
              liftStep (Equiv sigt t)
              pure (VE sigt p)
        AE msig f x -> do
          rt <- liftStep (NewVT TypeK)
          case msig of
            Nothing -> pure ()
            Just t -> liftStep (Equiv t rt)
          f' <- go f
          x' <- go x
          let ft_expected = functionType (undefined (eType x')) rt
          liftStep (Equiv ft_expected (undefined (eType f')))
          pure (AE rt f' x')

instantiate :: SomeKindedType TypeK -> Infer (Type () TypeK)
instantiate (SomeKindedType schemeOrType) = go schemeOrType
  where go =
          \case
             SchemeType t -> undefined

eval :: e -> Type e k -> k
eval e =
  \case
    Constructor s k ->  k
    FreeVariable s k -> k
    Application s k -> (eval e s) (eval e k)
    -- Forall inner -> pure (\x -> eval (x, e) inner)
    GenericVariable var -> (lookp var e)

lookp :: Var env t -> env -> t
lookp VZ (x,_)        = x
lookp (VS v) (_, env) = lookp v env

--------------------------------------------------------------------------------
-- Infer monad

newtype Infer a = Infer
  { unInfer :: Free (Ap InferStep) a
  } deriving (Functor, Applicative, Monad)
liftStep :: InferStep a -> Infer a
liftStep = Infer . liftF . liftAp

--------------------------------------------------------------------------------
-- Infer monad runner

data SomeEquivalence =
  forall env k. (Show k) =>
                SomeEquivalence (Type env k, Type env k)

data SomeType =
  forall env k. (Typeable k, Typeable env, Show k) =>
                SomeType (Type env k)
deriving instance Show SomeType

data S = S
  { idents :: !Natural
  , constraints :: ![SomeEquivalence]
  , env :: ![(Natural, SomeKindedType TypeK)]
  , vars :: ![SomeType]
  }

runInfer :: Infer (E  'PostInfer) -> IO (E  'PostInfer, S)
runInfer m =
  runStateT
    (foldFree (runAp (printit >=> reify)) (unInfer m))
    (S {idents = 0, constraints = [], env = [], vars = []})
  where
    printit x = x <$ lift (print x)
    reify :: InferStep a -> StateT S IO a
    reify =
      \case
        NewVT kind -> do
          nat <- gets idents
          let t = (FreeVariable nat kind)
          modify (\s -> s {idents = nat + 1, vars = vars s ++ [SomeType t]})
          pure t
        BindTV p t -> modify (\s -> s {env = (p, SomeKindedType (TypeType t)) : (env s)})
        LookupP p -> do
          r <- gets env >>= pure . lookup p
          case r of
            Nothing -> error "variable not in scope!"
            Just yes -> pure yes
        Equiv x y ->
          modify
            (\s -> s {constraints = (constraints s) ++ [SomeEquivalence (x, y)]})

instance Show S where
  show S {idents, constraints, env, vars} =
    unlines
      [ "Identifier counter: " ++ show idents
      , "Environment:"
      , unlines (map (\(p, t) -> show p ++ " :: " ++ show t) env)
      , unlines (map show vars)
      , "Constraints:"
      , tablize
          (map
             (\(SomeEquivalence (t, t')) -> [(False, show t), (False, " ≡ "), (True, show t')])
             constraints)
      ]

debug :: (Show (E  'PostInfer)) => E  'PreInfer -> IO ()
debug example = do
  (t, s) <- runInfer (inferE example)
  print example
  putStrLn "=>"
  print t
  print s
  putStrLn (show (eType t) ++ " -- from expression")


tablize :: (Foldable t, PrintfArg (t a)) => [[(Bool, t a)]] -> [Char]
tablize xs =
  intercalate "\n"
              (map (intercalate "  " . map fill . zip [0 ..]) xs)
  where fill (x',(left',text')) = printf ("%" ++ direction ++ show width ++ "s") text'
          where direction = if left'
                               then "-"
                               else ""
                width = maximum (map (length . snd . (!! x')) xs)


example0 :: E  'PreInfer
example0 =
  LE Nothing 0 (AE Nothing (VE Nothing 0) (CE (SomeKindedType(TypeType (Constructor "Int" TypeK :: Type () TypeK)))))

example1_err :: E  'PreInfer
example1_err =
  LE Nothing 0 (AE Nothing (CE (SomeKindedType(TypeType(Constructor "Int" TypeK :: Type () TypeK)))) (VE Nothing 0))

example2_err :: E  'PreInfer
example2_err =
  LE
    Nothing {-(Just (functionType (GenericVariable VZ) (GenericVariable VZ)))-}
    0
    (AE Nothing (CE (SomeKindedType(TypeType(Constructor "Int" TypeK :: Type () TypeK)))) (VE Nothing 0))

example3_correct :: E  'PreInfer
example3_correct =
  LE
    Nothing
    {-(Just (Forall (functionType (functionType (Constructor "Int" TypeK) (GenericVariable VZ)) (GenericVariable VZ))))-}
    0
    (AE Nothing (VE Nothing 0) (CE (SomeKindedType(TypeType(Constructor "Int" TypeK :: Type () TypeK)))))

example3_err :: E  'PreInfer
example3_err =
  LE
    Nothing{-(Just (Forall (functionType (functionType (GenericVariable VZ) (GenericVariable VZ)) (GenericVariable VZ))))-}
    0
    (AE Nothing (VE Nothing 0) (CE (SomeKindedType(TypeType(Constructor "Int" TypeK :: Type () TypeK)))))

example4_ok :: E  'PreInfer
example4_ok =
  LE
    Nothing
    {-(Just
       (functionType
          (Forall (functionType (GenericVariable VZ) (GenericVariable VZ)))
          (Constructor "()" TypeK)))-}
    0
    (AE
       Nothing
       (AE
          Nothing
          (CE
             (SomeKindedType @(TypeK) @(TypeK, (TypeK, ()))
                (SchemeType
                   (SchemeType
                      (TypeType
                         (functionType
                            (GenericVariable VZ)
                            (functionType
                               (GenericVariable (VS VZ))
                               (Constructor "()" TypeK))))))))
          (AE
             Nothing
             (VE Nothing 0)
             (CE
                (SomeKindedType
                   (TypeType (Constructor "Int" TypeK :: Type () TypeK))))))
       (AE
          Nothing
          (VE Nothing 0)
          (CE
             (SomeKindedType
                (TypeType (Constructor "Char" TypeK :: Type () TypeK))))))
