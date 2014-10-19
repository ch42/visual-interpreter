
module Interpreter.Core.Housekeeping where

import Interpreter.Core.Names
import Interpreter.Core.AnalyzedExpr

import Control.Monad.State	
import Control.Monad.Trans.Maybe

import qualified Data.Map as Map(lookup)
import Data.List(nub)
import Data.Tuple(swap)


-- removes all wrappers except for keywords 
prune :: ExprStrct Lambda -> ExprStrct Lambda
prune = foldExprStrct fWrapper BoundedVar FreeVar Abstr App 
	where
		fWrapper attachment@(Keyword _) content = Wrapper attachment content
		fWrapper _ content = content


-- resolves conflicts (e.g. duplicate IDs)
-- updates ExprMeta (e.g. searches for new redexes)
cleanUp :: ExprMeta -> LambdaExprStrct -> Maybe AnalyzedLambdaExpr
cleanUp oldMeta@(ExprMeta oldBS oldFS _) oldStrct = do
	result <- runStateT (helper oldStrct) emptyAnalysisResult
	return $ pack result

	where
		-- Monad transformer combining State- and Maybe-Monad
		helper :: LambdaExprStrct -> StateT AnalysisResult Maybe LambdaExprStrct
		helper (Abstr abstrType varID body) = do
			(appMeta, (bVarID, bs), fVarMeta) <- get
			varName <- lift $ lookup varID oldBS

			let 
				--------------------
				bVarMeta = (bVarID+1, (varName, bVarID):bs)
				--------------------

			put $ (appMeta, bVarMeta, fVarMeta)
			body' <- helper body
			return $ Abstr abstrType bVarID body'

		helper (App _ left right) = do
			((appID, rs), bVarMeta@(bVarID, bs), fVarMeta) <- get

			let
				--------------------
				appMeta | isAbstr (unwrap left) = (appID+1, appID:rs)
						| otherwise				= (appID+1, rs)

				s1 = (appMeta, bVarMeta, fVarMeta)
				--------------------
				
			(left', (appMeta', (bVarID', bs'), fVarMeta')) <- lift $ runStateT (helper left) s1

			let 
				--------------------
				s2 = (appMeta', (bVarID', bs), fVarMeta')
				--------------------
				
			(right', (appMeta'', (bVarID'', bs''), fVarMeta'')) <- lift $ runStateT (helper right) s2			

			put (appMeta'', (bVarID'', nub $ bs' ++ bs''), fVarMeta'')
			return $ App appID left' right'


		helper (Wrapper (Annotation "dirty") content) = helper content



		helper (Wrapper (Predecessor p) content) = do
			s1@(_, (_, bs), _) <- get

			(p', (appMeta', (bVarID', bs'), fVarMeta')) <- lift $ runStateT (helper p) s1

			let
				--------------------
				s2 = (appMeta', (bVarID', bs), fVarMeta')
				--------------------

			(content', (appMeta'', (bVarID'', bs''), fVarMeta'')) <- lift $ runStateT (helper content) s2

			put (appMeta'', (bVarID'', nub $ bs' ++ bs''), fVarMeta'')
			return $ Wrapper (Predecessor p') content'

		helper (Wrapper attachment content) = do
			content' <- helper content
			return $ Wrapper attachment content'


		helper (BoundedVar varID) = do
			(_, (_, bs), _) <- get
			varName <- lift $ lookup varID oldBS
			varID'  <- lift $ lookup varName bs 
			return $ BoundedVar varID'


		helper (FreeVar varID) = do
			varName <- lift $ lookup varID oldFS		
			(appMeta, bVarMeta@(_, bs), (fVarID, fs)) <- get	

			case lookup varName bs of
				-- unresolvable conflict, user has to apply α-conversion first
				Just varID' -> lift Nothing
				_			-> case lookup varName fs of
					Just varID' -> return $ FreeVar varID'
					_			-> do
						put (appMeta, bVarMeta, (fVarID+1, (varName, fVarID):fs))
						return $ FreeVar fVarID