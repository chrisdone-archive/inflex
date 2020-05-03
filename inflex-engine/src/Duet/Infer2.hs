{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
-- 1. Produce skeletons for structure (-> for lambdas, int, etc.)
-- 3. Equality constraints t~t' are given as leftovers.

-- 2. Unknown types--type variables--are returned, too.
-- 3. Class constraints are bubbled up around each expression.

-- 4. Finally, we unify all the constraints.
-- 5. We resolve instances that are complete.
-- 6. We complain about class constraints that aren't filled in.
-- 7. Make sure that you can't define: id :: a -> a; id x = 6
-- <https://gitlab.haskell.org/ghc/ghc/issues/4499>

-- Note
--
-- It seems clear below that unknown and p0 can be unified (one-way;
-- rigids are like constants); i.e. all p0's become unknown's, same
-- for out/r2.
--
-- Elaboration would let me explicitly state this, as a kind of elab
-- :: [constraint] -> [constraint] that would yield further
-- constraints based on what we see here.
--
-- Furthermore, rigids are only relevant when checking the definition
-- of a toplevel or let with an explicit type signature. Otherwise;
-- they may also be relevant for using a name "read-only" without
-- changing its type-variables in-place to match our use; instead we
-- copy them. So f Int or f Char works fine.
--
--
-- Constraints:
--                                  p0 -> r3   ≡   p1
--                                 Int -> r2   ≡   r3
-- unknown -> (unknown -> Int -> out) -> out   ≡   p0 -> p1 -> r2
--
-- unknown -> (unknown -> Int -> out) -> out -- from expression

module Duet.Infer2 where

import Control.Applicative.Free
import Control.Monad.Free
import Control.Monad.State
import Data.List
import Text.Printf

--------------------------------------------------------------------------------
-- Types

data T
  = VT String -- variable
  | CT String -- constant
  | FT T T     -- func
  deriving (Eq)

instance Show T where
  show = go False
    where
      go o =
        \case
          VT v -> v

          CT s -> s
          FT a b -> opt "(" ++ go True a ++ " -> " ++ go False b ++ opt ")"
            where opt str =
                    if o
                      then str
                      else ""

--------------------------------------------------------------------------------
-- Expression

data E s
  = CE T
  | LE (SType s) String (E s)
  | VE (SType s) String
  | AE (SType s) (E s) (E s)

--------------------------------------------------------------------------------
-- Stages

data Stage = PreInfer | PostInfer

type family SType s where
  SType 'PreInfer = Maybe T
  SType 'PostInfer = T

--------------------------------------------------------------------------------
-- Inferer

inferE :: E 'PreInfer -> Infer (E 'PostInfer)
inferE = go
  where
    go =
      \case
        CE t -> pure (CE t)
        LE msig p e -> do
          pt <- liftStep (NewVT "p")
          liftStep (BindTV p pt)
          e' <- go e
          let lt = FT pt (eType e')
          lt' <-
            case msig of
              Nothing -> pure lt
              Just t -> do
                liftStep (Equiv t lt)
                pure t
          pure (LE lt' p e')
        VE msig p -> do
          mt <- liftStep (LookupP p)
          case mt of
            Nothing -> liftStep (MissingP p)
            Just t ->
              case msig of
                Nothing -> pure (VE t p)
                Just sigt -> do
                  liftStep (Equiv sigt t)
                  pure (VE sigt p)
        AE msig f x -> do
          rt <- liftStep (NewVT "r")
          case msig of
            Nothing -> pure ()
            Just t -> liftStep (Equiv t rt)
          f' <- go f
          x' <- go x
          let ft_expected = FT (eType x') rt
          liftStep (Equiv ft_expected (eType f'))
          pure (AE rt f' x')

--------------------------------------------------------------------------------
-- Infer monad

data InferStep a where
  NewVT :: String -> InferStep T
  BindTV :: String -> T -> InferStep ()
  LookupP :: String -> InferStep (Maybe T)
  MissingP :: String -> InferStep a
  Equiv :: T -> T -> InferStep ()

newtype Infer a = Infer
  { unInfer :: Free (Ap InferStep) a
  } deriving (Functor, Applicative, Monad)
liftStep :: InferStep a -> Infer a
liftStep = Infer . liftF . liftAp

data S = S
  { idents :: Int
  , constraints :: [(T, T)]
  , env :: [(String, T)]
  , vars :: [T]
  }

runInfer :: Infer (E 'PostInfer) -> (E 'PostInfer, S)
runInfer m =
  runState
    (foldFree (runAp reify) (unInfer m))
    (S {idents = 0, constraints = [], env = [], vars = []})
  where
    reify :: InferStep a -> State S a
    reify =
      \case
        NewVT prefix -> do
          i <- gets idents
          let t = (VT (prefix ++ show i))
          modify (\s -> s {idents = i + 1, vars = vars s ++ [t]})
          pure t
        BindTV p t -> modify (\s -> s {env = (p, t) : (env s)})
        LookupP p -> gets env >>= pure . lookup p
        MissingP i -> error ("Missing parameter " ++ i)
        Equiv x y ->
          modify (\s -> s {constraints = (constraints s) ++ [(x, y)]})

--------------------------------------------------------------------------------
-- Debug

instance Show S where
  show S {idents, constraints, env, vars} =
    unlines
      [ "Identifier counter: " ++ show idents
      , "Environment:"
      , unlines (map (\(p, t) -> p ++ " :: " ++ show t) env)
      , unlines (map show vars)
      , "Constraints:"
      , tablize
          (map
             (\(t, t') -> [(False, show t), (False, " ≡ "), (True, show t')])
             constraints)
      ]

debug example =
  let (t, state) = runInfer (inferE example)
   in do print t
         print state
         putStrLn (show (eType t) ++ " -- from expression")
         {-putStrLn
           (graph
              (Left (eType t) :
               map Left (vars state) ++ map Right (constraints state)))-}

graph :: [Either T (T,T)] -> String
graph cs = unlines ["strict digraph {", unlines (map go cs), "}"]
  where
    go (Left e) = unlines [show (show e) ++ ";", splice e]
    go (Right (x, y)) =
      unlines
        [ unlines
            [ unlines
              [ show (show yt) ++ "->" ++ show (show xt) ++ " [dir=none, color=\"black:invis:black\"];"

              ]
            | xt <- [x]
            , yt <- [y]
            ]
        , splice x
        , splice y
        ]
    splice x =
      unlines
        [show (show t) ++ "->" ++ show (show x) ++ ";" | t <- tVars x, t /= x]

tVars :: T -> [T]
tVars =
  \case
    FT x y -> concatMap tVars [x, y]
    t -> [t]

-- | Make a table out of a list of rows.
tablize :: [[(Bool,String)]] -> String
tablize xs =
  intercalate "\n"
              (map (intercalate "  " . map fill . zip [0 ..]) xs)
  where fill (x',(left',text')) = printf ("%" ++ direction ++ show width ++ "s") text'
          where direction = if left'
                               then "-"
                               else ""
                width = maximum (map (length . snd . (!! x')) xs)

instance Show (E 'PreInfer) where
  show = \case
            CE t -> "(_ :: " ++ show t ++ ")"
            LE t p e -> "(\\" ++ p ++ " -> " ++ show e ++ ")"
            VE t v -> v
            AE t f x -> "(" ++ show f ++ " " ++ show x ++ ")"
instance Show (E 'PostInfer) where
  show = \case
            CE t -> "(_ :: " ++ show t ++ ")"
            LE t p e -> "((\\" ++ p ++ " -> " ++ show e ++ ") :: " ++ show t ++ ")"
            VE t v -> "(" ++ v ++ " :: " ++ show t ++ ")"
            AE t f x -> "((" ++ show f ++ " " ++ show x ++ ") :: " ++ show t ++ ")"

eType :: E 'PostInfer -> T
eType =
  \case
    CE t -> t
    LE t _ _ -> t
    VE t _ -> t
    AE t _ _ -> t

example0 :: E 'PreInfer
example0 =
  LE Nothing "f" (AE Nothing (VE Nothing "f") (CE (CT "Int")))

example1 :: E 'PreInfer
example1 =
  LE
    (Just
       (FT
          (RT "unknown")
          (FT (FT (RT "unknown") (FT (CT "Int") (RT "out"))) (RT "out"))))
    "x"
    (LE
       Nothing
       "f"
       (AE
          Nothing
          (AE Nothing (VE Nothing "f") (VE Nothing "x"))
          (CE (CT "Int"))))

example2_wrong :: E 'PreInfer
example2_wrong =
  LE Nothing "f" (AE Nothing (VE Nothing "f") (VE Nothing "f"))
