{-# LANGUAGE DeriveDataTypeable
            ,FlexibleInstances
  #-}

module Interpreter.Core.Expr(
  Abstraction(..), Expr(..), Primitive(..), AnyAbstr(..), AnyExpr,
  Lambda(..), LambdaExpr, Type(..), TypeExpr, Pattern(..),
  VarName, VarPool, newVarPool, freshVar, showExplicitly, keywords,
  isValidVarName, varNameGen, isVar, isSym, isAbstr, isApp)
  where

import Control.Applicative((<$>),(<*>))
import Control.Monad(void)
import Data.Data(Data, Typeable)
import Data.Char(isAsciiLower, isAsciiUpper, isDigit)
import Text.Parsec.String(Parser)
import Text.Parsec(char, (<|>), string, satisfy)
import Test.QuickCheck

import Interpreter.Core.Names
import Interpreter.Core.Operators(operators, defaultFixities, Fixity(..), FixityDirection(..))
--------------------------------------------------------------------------------


keywords :: [String]
keywords = ["let"]

isValidVarName :: VarName -> Bool
isValidVarName ""         = False
isValidVarName var@(v:vs) =
  var `notElem` keywords &&
  isAsciiLower v &&
  all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '\'') vs

-- | infinite pool of variable names
newVarPool :: VarPool
newVarPool = 
  [if i < 0 then [v] else v : show i
  | i <- [-1,0..] :: [Integer], v <- ['a'..'z']]

-- | fresh variable name that satisfies a given predicate
freshVar :: (VarName -> Bool) -> VarName
freshVar p = let (fresh:_) = [x | x <- newVarPool, p x] in fresh

type LambdaExpr = Expr Lambda
type TypeExpr   = Expr Type
type AnyExpr    = Expr AnyAbstr

data Expr a =
    Var VarName                -- X
  | Sym Primitive
  | Abstr a VarName (Expr a)   -- λX.Λ
  | App (Expr a) (Expr a)      -- (Λ Λ)
  deriving (Show, Eq, Typeable, Data)


data Primitive =
  Number Integer
  deriving (Eq, Typeable, Data)

instance Show Primitive where
  show (Number n) = show n  

--------------------------------------------------------------------------------
-- simple predicates that check for different constructor types
isVar :: Expr a -> Bool
isVar (Var _) = True
isVar _       = False

isSym :: Expr a -> Bool
isSym (Sym _) = True
isSym _       = False

isAbstr :: Expr a -> Bool
isAbstr (Abstr _ _ _) = True
isAbstr _             = False

isApp :: Expr a -> Bool
isApp (App _ _) = True
isApp _         = False
--------------------------------------------------------------------------------

-- Show functions/instances
showExplicitly :: Abstraction a => Expr a -> String
showExplicitly (Var v)
    | v `elem` operators        = "(" ++ v ++ ")"
    | otherwise                 =  v
showExplicitly (Sym s)          = show s
showExplicitly (Abstr a x expr) = show a ++ x ++ "." ++ showExplicitly expr
showExplicitly (App func arg)   = parantheses func ++ " " ++ parantheses arg
    where
      parantheses expr =
          case expr of
              Var v | v `elem` operators -> "(" ++ v ++ ")" 
                    | otherwise          -> v
              Sym s -> show s
              _     -> "(" ++ showExplicitly expr ++ ")"
--------------------------------------------------------------------------------

instance Show Pattern where
  showsPrec _ UnitPat = showString "()"
  showsPrec _ (VarPat v) = showString v
  showsPrec _ (p1 :# p2) = showChar '(' . shows p1 . showChar ',' . shows p2 . showChar ')'


--------------------------------------------------------------------------------
-- Arbitrary instances to generate random samples for QuickCheck tests

instance Arbitrary Primitive where
  -- only generate positive numbers
  arbitrary = Number <$> (arbitrary `suchThat` (>0))

instance Arbitrary (Expr Lambda) where
  arbitrary = sized (arbitrary' [])
    where
      varGen bVars = do
        useBound <- arbitrary :: Gen Bool
        if useBound && (not . null $ bVars)
        then elements bVars
        else do
          i <- choose (1,2) :: Gen Int
          case i of
            1 -> Var <$> varNameGen
            2 -> Sym <$> arbitrary

      arbitrary' bVars n
        | n <= 0 = varGen bVars
        | n > 0  = do
          i <- choose (1,3) :: Gen Int
          case i of
            1 -> varGen bVars
            2 -> do v <- varNameGen
                    Abstr Lambda v <$> arbitrary' (Var v:bVars) (n-1)
            3 -> App <$> arbitrary' bVars (n-1) <*> arbitrary' bVars (n-1)

  shrink (Abstr _ _ expr) = [expr]
  shrink (App e1 e2) = [e1,e2]
  shrink _ = []

-- only generate valid var names
varNameGen = arbitrary `suchThat` (\xs -> isValidVarName xs)
--------------------------------------------------------------------------------
