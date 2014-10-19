{-# LANGUAGE QuasiQuotes #-}

module Interpreter.Core.Church where
	
import Interpreter.Core.Parser(λ, abstr)
import Interpreter.Core.Expr

import qualified Data.Map as Map
import Data.Map(Map)


type Env a = Map VarName (Expr a)

-- all currently supported keywords 
defaultEnvironment :: Env Lambda
defaultEnvironment = Map.fromList
  [  
     ("true",   [λ|λx y.x|])
	,("false",  [λ|λx y.y|])
	,("ifelse", [λ|λp a b.p a b|])
	,("and",    [λ|λp q.p q p|])
	,("or",     [λ|λp q.p p q|])
	,("not",    [λ|λp a b.p b a|])
	,("pair",   [λ|λa b f.f a b|])
	,("first",  [λ|λp.p (λa b.a)|])
	,("second", [λ|λp.p (λa b.b)|])
	,("cons",   [λ|λx y c.c x y|])
	,("head",   [λ|λd.d true|])
	,("tail",   [λ|λd.d false|])
	,("nil",    [λ|λx.true|])
	,("null",   [λ|λd.d (λx y.false)|])
	,("succ",   [λ|λn f x.f (n f x)|])
	,("pred",   [λ|λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)|])
	,("add",    [λ|λn m f x.n f (m f x)|])
	,("mult",   [λ|λn m f.m (n f)|])
	,("isZero", [λ|λn x y.n (λz.y) x|])
	-- bug in parser ??? 
	--,("Y",      [λ|λf.(λx.f (x x)) (λx.f (x x))|])
  ]

-- generates Church numeral representing given number n  
churchNumeral :: Integral n => n -> LambdaExpr
churchNumeral n = [λ|λf x.{helper n}|]
	where
		helper 0 = [λ|x|]
		helper n = [λ|f ({helper (n-1)})|]
		  

