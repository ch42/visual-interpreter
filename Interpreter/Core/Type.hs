{-# LANGUAGE DeriveFunctor
            ,TypeSynonymInstances
            ,FlexibleInstances
            ,TemplateHaskell
            ,QuasiQuotes
            #-}

module Interpreter.Core.Type where

import Data.Map(Map)
import qualified Data.Map as Map

import Data.Set(Set)
import qualified Data.Set as Set

import Data.List(foldl')

import Control.Applicative((<$>),(<*>))
import Control.Monad.State
import Control.Lens

import Interpreter.Core.Expr
import Interpreter.Core.Parser
import Interpreter.Core.Eval
import Interpreter.Core.Semantics
--------------------------------------------------------------------------------
type Assignments = Map VarName (Expr Type)
type TypeInstantiations = Map VarName (Maybe (Expr Type))
type TypeInference a = StateT TypeEnv (Either String) a

data Assumption =
    Forall VarName              -- all-quantified variable
  | IsType VarName (Expr Type)  -- type to type assignment
  | HasType VarName (Expr Type) -- type to data assignment
  deriving Show

data TypeEnv = TypeEnv
  {
    _assumptions     :: Map VarName [Expr Type]
  , _typeAssignments :: Map VarName (Expr Type)
  , _quantifiedVars  :: Set VarName
  , _varPool         :: VarPool
  }
makeLenses ''TypeEnv

showTypeEnv :: TypeEnv -> String
showTypeEnv tenv =
  "assumptions = "     ++ show (tenv^.assumptions)     ++ "\n" ++
  "typeAssignments = " ++ show (tenv^.typeAssignments) ++ "\n" ++
  "quantifiedVars = "  ++ show (tenv^.quantifiedVars)  ++ "\n"

type TypeVar = VarName

newtype SubstMap a = SubstMap (Map VarName (Expr a))

(|->) :: VarName -> Expr a -> SubstMap a
x |-> expr = SubstMap (Map.insert x expr Map.empty)

(@@) :: SubstMap a -> Expr a -> Expr a
(SubstMap m) @@ expr =
  foldl' (\e (k,v) -> subst e (k := v)) expr (Map.assocs m)

(>+) :: SubstMap a -> SubstMap a -> SubstMap a
(SubstMap s1) >+ s@(SubstMap s2) = SubstMap (s2 `Map.union` Map.map (s@@) s1)

emptySubst :: SubstMap a
emptySubst = SubstMap Map.empty
--------------------------------------------------------------------------------
renameTypeVars :: Expr Type -> Expr Type
renameTypeVars = renameTypeVars' newVarPool
  where
    renameTypeVars' :: [VarName] -> Expr Type -> Expr Type
    renameTypeVars' (a:as) expr@[τ|∀_._|] =
      let t' = renameTypeVars' as (eval emptyEnvironment [τ|{expr} {Var a}|])
      in [τ|∀{a}.{t'}|]
    renameTypeVars' _ t = t
--------------------------------------------------------------------------------
inferType :: Env Lambda -> Expr Lambda -> Either String (Expr Type)
inferType env lambdaExpr =
  renameTypeVars <$> evalStateT (inferType' lambdaExpr >>= quantifyType) newTypeEnv
  where
    newTypeEnv :: TypeEnv
    newTypeEnv = TypeEnv Map.empty Map.empty Set.empty newVarPool

    newVar :: TypeInference TypeVar
    newVar = use varPool >>= \(v:vs) -> varPool .= vs >> return v
    ----------------------------------------------------------------------------
    -- lookup environment/assumptions for a given identifier 
    -- or return a fresh type variable
    getTypeFor :: VarName -> TypeInference (Expr Type)
    getTypeFor varname = do
      result <- Map.lookup varname <$> use assumptions
      case result of
        Just (t:_) -> return t
        _          ->
          case Map.lookup varname env of
            Just expr -> do
                t <- newVar
                makeAssumptions
                  [
                    Forall t,
                    varname `HasType` (Var t) -- varname :: t
                  ]
                t' <- inferType' expr
                unify t' (Var t)
                getTypeFor varname

            Nothing   -> do
                t <- Var <$> newVar
                assumptions %= Map.insert varname [t]
                return t
    ----------------------------------------------------------------------------
    -- make a lookup for a given TYPE variable name and return a type expression
    -- that can be s1ubstituted for it
    getType :: TypeVar -> TypeInference (Expr Type)
    getType typename =
      (Map.lookup typename <$> use typeAssignments) >>= 
        maybe (lift $ Left ("getType: found no type for " ++ typename)) return
    ----------------------------------------------------------------------------
    -- add assumptions: stack assumptions for duplicate variables to achieve
    -- local "shadowing"
    makeAssumptions :: [Assumption] -> TypeInference ()
    makeAssumptions = mapM_ addAssumption
      where
        addAssumption (Forall v) = do
          quantifiedVars %= Set.insert v
          typeAssignments %= Map.insert v (Var v)
        addAssumption (x `HasType` t) = assumptions %= Map.insertWith (++) x [t]
        addAssumption (v `IsType` t) = typeAssignments %= Map.insert v t

    -- remove (first layer of) assumptions for a set of variables
    removeAssumptionsFor :: [VarName] -> TypeInference ()
    removeAssumptionsFor = mapM_ (\v -> assumptions %= Map.adjust tail v)
    ----------------------------------------------------------------------------
    isQuantified :: TypeVar -> TypeInference Bool
    isQuantified v = Set.member v <$> use quantifiedVars

    quantifyType :: Expr Type -> TypeInference (Expr Type)
    quantifyType t = quantifyWith
      <$> (Set.intersection (Set.fromList (freeVars t)) <$> use quantifiedVars)
      <*> return t

    quantifyWith :: Set VarName -> Expr Type -> Expr Type
    quantifyWith varSet t =  Set.foldr (\v t' -> [τ|∀{v}.{t'}|]) t varSet
    ----------------------------------------------------------------------------
    applySubstitution :: Subst Type -> TypeInference ()
    applySubstitution s@(v := t) = do
      quantified <- isQuantified v
      when quantified (quantifiedVars %= Set.delete v)
      typeAssignments %= Map.insert v t
      assumptions.mapped.mapped %= (`subst` s)
      typeAssignments.mapped %= (`subst` s)
    ----------------------------------------------------------------------------
    -- resolve types as far as possible, i.e. until all free variables cannot be
    -- substituted any further
    resolveType :: Expr Type -> TypeInference (Expr Type)
    resolveType typeExpr = do
      assignments <- use typeAssignments
      let resolveTypeVar e v
            | v == "->" = return e
            | otherwise =
                case Map.lookup v assignments of
                    Nothing -> return e
                    Just (Var v') | v == v' -> return e
                    Just t -> resolveType t >>= \t' -> return (subst e (v:=t'))
      foldM resolveTypeVar typeExpr (freeVars typeExpr)
    ----------------------------------------------------------------------------
    unify :: Expr Type -> Expr Type -> TypeInference ()
    unify t1' t2' = do
      qf <- flip Set.member <$> use quantifiedVars
      t1 <- resolveType t1'
      t2 <- resolveType t2'
      case unifyP qf t1 t2 of
        [] -> lift $ Left ("cannot unify type '" ++ show t1 ++
                                      "' with '" ++ show t2)
        (SubstMap m:_) -> mapM_ (\(k,v) -> applySubstitution (k := v)) (Map.assocs m)

      where
        unifyP :: (VarName -> Bool) -> Expr Type -> Expr Type -> [SubstMap Type]
        unifyP quantified t1 t2 = case (t1,t2) of
          (Var x, Var y)
              | x == y        -> [emptySubst]
              | quantified y  -> [y |-> t1]
              | quantified x  -> [x |-> t2]
              | otherwise     -> []
          (Var v, _)          -> [ v |-> t2 | quantified v, not (v `occursIn` t2) ]
          (_, Var v)          -> [ v |-> t1 | quantified v, not (v `occursIn` t1) ]
          ([τ|a b|],[τ|c d|]) -> unifylist quantified [a, b] [c, d] emptySubst
          _                   -> []

        unifylist :: (VarName -> Bool) -> [Expr Type] -> [Expr Type]
                  -> SubstMap Type -> [SubstMap Type]
        unifylist _  [] [] s             = [s]
        unifylist qf (t1:ts1) (t2:ts2) s =
            [s'' | s'  <- unifyP qf t1 t2
                 , s'' <- unifylist qf
                                    [ s' @@ t | t <- ts1 ]
                                    [ s' @@ t | t <- ts2 ]
                                    (s >+ s')
            ]
        unifylist _ _ _ _ = []
    ----------------------------------------------------------------------------
    inferType' :: Expr Lambda -> TypeInference (Expr Type)
    inferType' (Sym (Number _)) = return (Var "Int")
    inferType' (Var v) = getTypeFor v
    ----------------------------------------------------------------------------
    inferType' [λ|λx.expr|] = do
        a <- newVar
        b <- newVar
        t <- newVar
        makeAssumptions
          [
            Forall a, Forall b,
            t `IsType` [τ|{Var a} -> {Var b}|], -- t := a -> b
            x `HasType` (Var a)                 -- x :: a
          ]
        b' <- inferType' expr
        unify b' (Var b)
        t' <- getType t
        removeAssumptionsFor [x]
        return t'
    ----------------------------------------------------------------------------
    inferType' [λ|f x|] = do
        a <- newVar
        b <- newVar
        t1 <- inferType' f
        t2 <- inferType' x
        makeAssumptions [Forall a, Forall b]
        unify t1 [τ|{Var a} -> {Var b}|]
        unify t2 (Var a)
        getType b
--------------------------------------------------------------------------------
