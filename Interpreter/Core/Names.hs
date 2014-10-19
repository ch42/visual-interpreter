{-# LANGUAGE DeriveDataTypeable #-}


module Interpreter.Core.Names where

import Data.Data(Data, Typeable)
import Control.Monad(void)
import Text.Parsec.String(Parser)
import Text.Parsec(char, (<|>), string, satisfy)


type AnalysisResult = (AppMeta, VarMeta, VarMeta)

emptyAnalysisResult :: AnalysisResult
emptyAnalysisResult = ((1,[]),(1,[]),(1,[])) 


type VarID = Int
type FreeVarID = VarID
type BoundedVarID = VarID

type VarName = String
type VarRecord = [(VarID, VarName)]
type VarPool = [VarName]

type AppID = Int

type Redexes = [AppID]
type Keywords = [String]

type AppMeta = (AppID, [AppID])
type VarMeta = (Int, [(VarName, VarID)])
type Meta = (AppMeta, VarMeta, VarMeta)	

                      
filterVars :: (VarID -> Bool) -> VarRecord -> VarRecord                       
filterVars _ [] = []
filterVars f (var@(varID,_):vs) | f varID = var:filterVars f vs
                                | otherwise = filterVars f vs    

class (Show a, Read a, Eq a, Typeable a, Data a) => Abstraction a where
  symbolParser :: Parser a
 
-- The concept of an abstraction "abstracts" over the symbol used to describe
-- an abstraction, e.g. λ for the known lambda-expressions, ∀ for polymorphic
-- type expressions

instance Abstraction Lambda where
  symbolParser = (char 'λ' <|> char '\\') >> return Lambda

instance Abstraction Type where
  symbolParser = (void (char '∀') <|> void (string "forall")) >> return Type

instance Abstraction AnyAbstr where
  symbolParser = AnyAbstr `fmap` satisfy (`elem` ['α'..'ω'])

data Lambda = Lambda
  deriving (Eq, Typeable, Data)

data Type = Type
  deriving (Eq, Typeable, Data)

data AnyAbstr = AnyAbstr Char
  deriving (Eq, Typeable, Data)

-------------------------------------------------------------------------------- 
 
-------  
instance Show Lambda where
  show _  = "lambda"

instance Show Type where
  show _  = "∀"

instance Show AnyAbstr where
  show (AnyAbstr c) = [c]
-------  
 
-------  
instance Read Lambda where
  readsPrec _  v = [(Lambda, [])]

instance Read Type where
  readsPrec _  v = []

instance Read AnyAbstr where
  readsPrec _ v = []
------    

data Pattern =
	UnitPat
  | VarPat VarName
  | Pattern :# Pattern
  deriving (Eq, Typeable, Data)