module Interpreter.Interface
	(parser, keywords, alphaConversion, betaReduction) where

import Interpreter.Core.Names		
import Interpreter.Core.AnalyzedExpr
import Interpreter.Core.Parser
import Interpreter.Core.Church(defaultEnvironment)
import Interpreter.Core.Housekeeping

import Data.Map(keys) 
import Control.Monad.State	
import qualified Text.Parsec as P


--- Parser
parser :: String -> Maybe AnalyzedLambdaExpr
parser string = case P.parse lambdaExpression "" string of
	Left _ 		-> Nothing
	Right expr	-> Just $ analyzeExpr expr
	

--- Keywords		
keywords :: Keywords
keywords = keys defaultEnvironment	


--- α-Conversion
alphaConversion :: BoundedVarID -> VarName -> AnalyzedLambdaExpr -> Maybe AnalyzedLambdaExpr	
alphaConversion varID newVarName (AnalyzedExpr (ExprMeta bs fs rs) oldStrct) = do
	
	let 
		oldStrct' 	= prune oldStrct
		bs' 	  		= renameBoundedVar varID newVarName bs 
		
	return $ AnalyzedExpr (ExprMeta bs' fs rs) oldStrct'	
	
		
--- 	β-reduction	
betaReduction :: AppID -> AnalyzedLambdaExpr -> Maybe AnalyzedLambdaExpr 
betaReduction appID (AnalyzedExpr oldMeta oldStrct) = do
	let oldStrct' 	= prune oldStrct
	
	(App _ left right) <- lookupApp appID oldStrct'
	
	case unwrap left of
		(Abstr _ varID body) 	-> 
			let result = substVar varID right body
			in  cleanUp oldMeta $ replaceApp appID result oldStrct'
						
		_						-> Nothing	
		
				
			
			
	






