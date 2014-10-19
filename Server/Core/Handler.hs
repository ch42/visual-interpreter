module Server.Core.Handler where
	
import Server.Core.XML
import Server.Core.ErrorMessages	

import qualified Interpreter.Interface as Interpreter	
import Interpreter.Core.Names
import Interpreter.Core.AnalyzedExpr
	
import Happstack.Lite
	
import Data.Maybe (isJust, fromJust)
import Data.Char (toLower)

import Data.Text.Lazy (unpack)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.UTF8 as U

import Control.Applicative (optional, (<$>))

import Text.Parsec.String
import Text.Parsec hiding (optional)



----- Helper functions
-----

--- Sets content type etc. for HTTP-response 
createResponse :: ToXML a => a -> ServerPart Response
createResponse responseContent = do
	return $ toResponseBS (C.pack "application/xml") (L.pack $ toXML responseContent)


--- Get value of HTTP-parameter with given key 
getRequestParam :: String -> ServerPart (Maybe String)
getRequestParam key = do 
	param <- optional $ lookText key
	return $ unpack <$> param 
-----	
	

	

----- Handler for all functions exported by the web interface
-----

--- Parser
parserHandler :: ServerPart Response
parserHandler = do 
	string 		<- getRequestParam "string"

	if isJust string 
		then 		
			case Interpreter.parser $ fromJust string of
				Just analyzedExpr 	-> createResponse analyzedExpr
				Nothing				-> badRequest invalidExprErr

		else badRequest noExprErr


--- Keywords
keywordsHandler :: ServerPart Response
keywordsHandler = createResponse Interpreter.keywords


--- α-Conversion
alphaConversionHandler :: ServerPart Response
alphaConversionHandler = do 
	varID 				<- getRequestParam "varID"
	newVarName 			<- getRequestParam "newVarName" 
	analyzedExprXML		<- getRequestParam "analyzedExprXML"

	if isJust varID && isJust newVarName && isJust analyzedExprXML
		then 		
			case (fromXML $ fromJust analyzedExprXML) :: Maybe AnalyzedLambdaExpr of
				Just analyzedExpr 	-> case Interpreter.alphaConversion (read $ fromJust varID :: VarID) (fromJust newVarName) analyzedExpr of
			 								Just analyzedExpr' 	-> createResponse analyzedExpr'
			 								Nothing 			-> badRequest reductionFailedErr
				Nothing				-> badRequest invalidExprErr

		else badRequest noExprErr
		

--- 	β-reduction	
betaReductionHandler :: ServerPart Response
betaReductionHandler = do 
	appID 			<- getRequestParam "appID"
	analyzedExprXML 	<- getRequestParam "analyzedExprXML"

	if isJust appID && isJust analyzedExprXML
		then 		
			case (fromXML $ fromJust analyzedExprXML) :: Maybe AnalyzedLambdaExpr of
				Just analyzedExpr 	-> case Interpreter.betaReduction (read $ fromJust appID :: AppID) analyzedExpr of
			 								Just analyzedExpr' 	-> createResponse analyzedExpr'
			 								Nothing 			-> badRequest reductionFailedErr
				Nothing				-> badRequest invalidExprErr

		else badRequest noExprErr
-----	



----- Other Handlers
-----

--- The GUI
clientHandler :: ServerPart Response
clientHandler = serveDirectory EnableBrowsing ["index.html"] "./Client"

	