{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable, QuasiQuotes #-}

module Interpreter.Core.AnalyzedExpr where

import qualified Interpreter.Core.Expr as E
import Interpreter.Core.Parser(λ)		
import Interpreter.Core.Names
import Interpreter.Core.Church


import Control.Monad.State	

import qualified Data.Map as Map(lookup)
import Data.List(nub)
import Data.Tuple(swap)
import Data.Data(Data, Typeable)


-- represents an analyzed λ-expression
data AnalyzedExpr a where 
	
	AnalyzedExpr :: Abstraction a => {
		exprMeta 	:: ExprMeta, 
		exprStrct 	:: ExprStrct a	
	} -> AnalyzedExpr a

type AnalyzedLambdaExpr = AnalyzedExpr Lambda
deriving instance Show a => Show (AnalyzedExpr a)
deriving instance Eq a => Eq (AnalyzedExpr a)


-- additional data for λ-expressions, e.g. redexes 
data ExprMeta = 
	
	ExprMeta {
		bVars		:: VarRecord,
		fVars 		:: VarRecord,
		redexes		:: [AppID]
	} deriving (Show, Eq)
	
	
-- represents the structure of a λ-expression
data ExprStrct a where


	-----------------
	-- a wrapper can be used to attach arbitrary data to subexpressions
	-- use-cases: Church-encoding, keywords, previous versions etc.
	Wrapper :: Abstraction a => {
		 attachment		:: WrapperAttachment,
		 content			:: ExprStrct a
	} -> ExprStrct a
	------------------
	

 	BoundedVar :: Abstraction a => {
 		varID 			:: BoundedVarID
 	} -> ExprStrct a

 	FreeVar :: Abstraction a => {
 	 	varID 			:: FreeVarID
 	} -> ExprStrct a
	
	
 	Abstr :: Abstraction a => {
 	 	abstrType 		:: a, 
 	 	varID 			:: BoundedVarID, 
 	 	body 			:: ExprStrct a	
 	} -> ExprStrct a

 	App :: Abstraction a => {
 		appID 			:: AppID,
 		left 			:: ExprStrct a,
 		right 			:: ExprStrct a	
 	} -> ExprStrct a



type LambdaExprStrct = ExprStrct Lambda
deriving instance Show a => Show (ExprStrct a)
deriving instance Eq a => Eq (ExprStrct a)

-----
isAbstr :: LambdaExprStrct -> Bool
isAbstr (Abstr _ _ _) = True
isAbstr (Wrapper _ content) = isAbstr content
isAbstr _ = False

isVar :: LambdaExprStrct -> Bool
isVar (BoundedVar _) = True
isVar (FreeVar _) = True
isVar (Wrapper _ content) = isVar content
isVar _ = False
-----


foldExprStrct :: Abstraction a => 	(WrapperAttachment -> b -> b) -> 	-- Wrapper
								 	(VarID -> b) -> 						-- BoundedVar
								 	(VarID -> b) -> 						-- FreeVar
								 	(a -> VarID -> b -> b) -> 			-- Abstraction
								 	(AppID -> b -> b -> b) -> 			-- Application
								 	ExprStrct a -> 
								 	b

foldExprStrct fWrapper fBoundedVar fFreeVar fAbstr fApp = helper
	where
		helper (Wrapper attachment content) = fWrapper attachment $ helper content
		helper (BoundedVar varID) = fBoundedVar varID
		helper (FreeVar varID) = fFreeVar varID
		helper (Abstr abstrType varID body) = fAbstr abstrType varID $ helper body
		helper (App appID left right) = fApp appID (helper left) (helper right)
	
		
		
		


----- Helper functions for WRAPPER
-----
data WrapperAttachment = Keyword String | Annotation String | Predecessor LambdaExprStrct 
	deriving (Show, Eq)


-- throw away all wrappers and return the content
unwrap :: Abstraction a => ExprStrct a -> ExprStrct a
unwrap (Wrapper _ content) = unwrap content
unwrap strct = strct	


-- adds an annotation to the given subexpression using wrappers
annotate :: Abstraction a => String -> ExprStrct a -> ExprStrct a
annotate str = Wrapper (Annotation str)


-- replaces the content of the innermost wrapper 
replaceWrapperContent :: Abstraction a => ExprStrct a -> ExprStrct a -> ExprStrct a
replaceWrapperContent newContent = helper
	where
		helper (Wrapper attachment content) = Wrapper attachment (helper content)
		helper _ = newContent
-----	
		



----- Helper functions for VARIABLES
-----
maxVarID :: VarRecord -> VarID
maxVarID =  maximum . fst . unzip


substVar :: BoundedVarID -> LambdaExprStrct -> LambdaExprStrct -> LambdaExprStrct
substVar varID substitute oldStrct = evalState (helper oldStrct) 1 
	 where
		helper :: LambdaExprStrct -> State Int LambdaExprStrct
		helper var@(BoundedVar varID') 	| varID == varID' = do
			 								i <- get
											put (i+1)
											if i == 1 
												then return substitute
												else return $ annotate "dirty" substitute
		 								| otherwise = return var

 		helper (App appID left right) = do
 			left'  	<- helper left
 			right' 	<- helper right	
 			return $ App appID left' right'

 		helper (Abstr abstrType varID body) = do
 			body'   <- helper body
 			return $ Abstr abstrType varID body'

 		helper (Wrapper wrapperType content) = do 
 			content' <- helper content
 			return $ Wrapper wrapperType content'

		helper strct = return strct	
		
		
		
renameBoundedVar :: BoundedVarID -> VarName -> VarRecord -> VarRecord
renameBoundedVar varID newVarName = helper
	where
		helper [] = []
		helper (v@(varID',_):vs) 	| varID == varID' = (varID, newVarName):vs
								  	| otherwise = v:helper vs
-----								  	 




			
----- Helper functions for APPLICATIONS
-----		 	
lookupApp :: AppID -> LambdaExprStrct -> Maybe LambdaExprStrct
lookupApp appID = helper
	where
		helper app@(App appID' left right) 	| appID == appID' = Just app
											| otherwise = helper left `mplus` helper right
		helper (Abstr _ _ body) = helper body										 
		helper _ = Nothing
		



replaceApp :: AppID -> LambdaExprStrct -> LambdaExprStrct -> LambdaExprStrct
replaceApp appID strct = foldExprStrct Wrapper BoundedVar FreeVar Abstr fApp
	where 
		fApp appID' left right 	| appID == appID' = Wrapper (Predecessor $ helper (App appID' left right)) strct
							   	| otherwise 	= App appID' left right

		
		helper (App appID left right) = case unwrap left of
			(Abstr abstrType varID body) -> let 
												---
												leftContent' = Abstr abstrType varID $ markArrowTargets body 
												left'  = replaceWrapperContent leftContent' left
												right' = annotate "arrowSource" right
												---
											 in App appID left' right'
												where
													markArrowTargets = foldExprStrct Wrapper fBoundedVar FreeVar Abstr App
													
													fBoundedVar varID' | varID == varID' = annotate "arrowTarget" $ BoundedVar varID
																	   | otherwise = BoundedVar varID'
			strct						 ->  strct								 	
-----

	
	   	
pack :: (LambdaExprStrct, AnalysisResult) -> AnalyzedLambdaExpr
pack (strct, ((_,rs),(_,bs),(_,fs))) = AnalyzedExpr meta strct
	where
		meta = ExprMeta (map swap bs) (map swap fs) rs
		
		

analyzeExpr :: E.LambdaExpr -> AnalyzedLambdaExpr
analyzeExpr expr = pack $ runState (helper expr) emptyAnalysisResult
	where 
		helper :: E.LambdaExpr -> State Meta LambdaExprStrct
		helper (E.App left right) = do	
			(appMeta@(a, rs), bVarMeta@(b, bs), fVarMeta) <- get
		
			let
				----	 			       
				s1 = ((a+1, rs), (b, bs), fVarMeta)
				(left', ((a', rs'), bVarMeta'@(b', bs'), fVarMeta')) = runState (helper left) s1
		
				-- check left', because left might be a keyword that gets replaced by an expression
				appMeta' | isAbstr left' 	= (a', a:rs')
						 | otherwise 		= (a',   rs')
		
				s2 = (appMeta', (b', bs), fVarMeta')
				(right', (appMeta'',  (b'', bs''), fVarMeta'')) = runState (helper right) s2
				----
		
			put (appMeta'', (b'', nub $ bs' ++  bs''), fVarMeta'')	
			return $ App a left' right'
			

		helper (E.Sym p) = case p of 
			(E.Number n)  -> do
				expr <- helper $ churchNumeral n	
				return $ Wrapper (Keyword (show n)) expr
						

		helper (E.Abstr a varName body) = do
			(appMeta, bVarMeta@(b, bs), fVarMeta) <- get
			put (appMeta, (b+1, (varName, b):bs), fVarMeta)
	
			body' <- helper body
			return $ Abstr a b body'
	
	
		helper (E.Var varName) = 
			case Map.lookup varName defaultEnvironment of
				-- keyword found, e.g. succ
				-- replace keyword with corresponding expression, return expression in a wrapper  
				Just expr	-> do
					expr' <- helper expr
					return $ Wrapper (Keyword varName) expr'
				-- varName is not a reserved keyword
				_		 	-> do
					----
					(appMeta, bVarMeta@(_, bs), fVarMeta@(f, fs)) <- get	
			
					case lookup varName bs of
						-- bounded var found
						Just varID 	-> do
							return $ BoundedVar varID
						-- free var found	
						_		 	-> case lookup varName fs of
							Just varID	-> return $ FreeVar varID
							_			-> do
								put (appMeta, bVarMeta, (f+1, (varName, f):fs))
								return $ FreeVar f     





			
