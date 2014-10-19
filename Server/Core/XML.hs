{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Server.Core.XML where
	
import Interpreter.Core.Names
import Interpreter.Core.AnalyzedExpr
import Interpreter.Interface	

import Data.Maybe(fromJust)

import qualified Text.XML.HXT.Core as X
import Text.XML.HXT.Arrow.Pickle.Xml


	
type XML = String


----- Type classes "ToXML" and "FromXML" hide HXT related overhead
-----
class ToXML b where
	toXML :: b -> XML	
	
class FromXML b where
	fromXML :: XML -> Maybe b

instance ToXML ExprMeta where
	toXML = showPickled []
	
instance Abstraction a => ToXML (AnalyzedExpr a) where
	toXML = showPickled []
	
instance Abstraction a => FromXML (AnalyzedExpr a) where
	fromXML = unpickleDoc xpAnalyzedExpr . head . parseXML
		where
			parseXML = X.runLA $ X.xread 
			
			
instance ToXML Keywords where
	toXML = showPickled []						
-----	


----- XmlPickler instance declarations
-----
instance Abstraction a => XmlPickler (AnalyzedExpr a) where
	xpickle = xpAnalyzedExpr

instance XmlPickler ExprMeta where
	xpickle = xpExprMeta
	
instance Abstraction a => XmlPickler (ExprStrct a) where
	xpickle = xpExprStrct
	
instance XmlPickler Keywords where
	xpickle = xpKeywords	
-----	


	

----- The necessary picklers for "AnalyzedExpr a"	
-----
xpAnalyzedExpr :: Abstraction a => PU (AnalyzedExpr a)
xpAnalyzedExpr = xpElem "expr" $
		xpWrap (uncurry AnalyzedExpr, \expr -> (exprMeta expr, exprStrct expr)) $
			xpPair xpExprMeta (xpElem "strct" $ xpExprStrct)
		


xpExprMeta :: PU ExprMeta
xpExprMeta = xpElem "meta" $
		xpWrap (X.uncurry3 ExprMeta, \meta -> (bVars meta, fVars meta, redexes meta)) $
			xpTriple bvars fvars (xpElem "redexes" xpRedex)
				where 
					bvars = (xpElem "bvars" xpBoundedVars)
					fvars = (xpElem "fvars" xpFreeVars)
					
					
xpExprStrct :: Abstraction a => PU (ExprStrct a)
xpExprStrct = xpAlt tag ps
	where
		tag (Wrapper _ _) 	= 0
		tag (BoundedVar _) 	= 1
		tag (FreeVar _) 		= 2
		tag (Abstr _ _ _) 	= 3
		tag (App _ _ _) 		= 4
		ps = [xpWrapper, xpBoundedVar, xpFreeVar, xpAbstr, xpApp]		
					


					
---					
xpWrapper :: Abstraction a => PU (ExprStrct a)
xpWrapper = xpElem "wrapper" $ 
		xpWrap (uncurry Wrapper, \(Wrapper wrapperType content) -> (wrapperType, content)) $
			xpPair xpWrapperAttachment (xpElem "content" $ xpExprStrct)

xpAbstr :: Abstraction a => PU (ExprStrct a)
xpAbstr = xpElem "abstr" $
	xpWrap (X.uncurry3 Abstr, \abstr -> (abstrType abstr, varID abstr, body abstr)) $
		xpTriple (xpAttr "type" xpPrim) (xpAttr "bvar" xpickle) xpExprStrct

xpApp :: Abstraction a => PU (ExprStrct a)
xpApp = xpElem "app" $
	xpWrap (X.uncurry3 App, \app -> (appID app, left app, right app)) $
		xpTriple (xpAttr "id" xpickle) (xpElem "left" xpExprStrct) (xpElem "right" xpExprStrct)

xpBoundedVar :: Abstraction a => PU (ExprStrct a)
xpBoundedVar = xpWrap (BoundedVar, \(BoundedVar v) -> v) $
					xpElem "bvar" xpAttrID

xpFreeVar :: Abstraction a => PU (ExprStrct a)
xpFreeVar = xpWrap (FreeVar, \(FreeVar v) -> v) $
					xpElem "fvar" xpAttrID
---			




---
xpRedex :: PU [VarID]
xpRedex = xpList $ xpElem "app" xpAttrID

xpBoundedVars :: PU VarRecord
xpBoundedVars = xpList $ xpElem "bvar" $ xpPair xpAttrID (xpAttr "name" xpText)

xpFreeVars :: PU VarRecord
xpFreeVars = xpList $ xpElem "fvar" $ xpPair xpAttrID (xpAttr "name" xpText)
---
		

 		
					
---
xpWrapperAttachment :: PU WrapperAttachment
xpWrapperAttachment = xpElem "attachment" $ xpAlt tag ps
	where
		tag (Keyword _) 		= 0
		tag (Annotation _) 	= 1
		tag (Predecessor _) 	= 2
		ps = [xpKeyword, xpAnnotation, xpPredecessor]
		
		
xpKeyword :: PU WrapperAttachment
xpKeyword = xpElem "keyword" $
		xpWrap (Keyword, \(Keyword k) -> k) xpText
		
xpAnnotation :: PU WrapperAttachment
xpAnnotation = xpElem "annotation" $
		xpWrap (Annotation, \(Annotation a) -> a) xpText	

xpPredecessor :: PU WrapperAttachment
xpPredecessor = xpElem "predecessor" $
		xpWrap (Predecessor, \(Predecessor p) -> p) xpExprStrct
---

	
				
		
----- Pickler for list of keywords
-----
xpKeywords :: PU Keywords
xpKeywords = xpElem "keywords" $ xpList $ xpElem "keyword" xpText	
-----



--- adds attribute "id"
xpAttrID :: PU Int
xpAttrID = xpAttr "id" xpickle		
		
					