{-# LANGUAGE QuasiQuotes
            ,TemplateHaskell
            ,TypeSynonymInstances
            ,FlexibleInstances
            ,MultiParamTypeClasses
            ,DeriveDataTypeable
  #-}

module Interpreter.Core.Parser(varName, lambdaExpression, typeExpression, λ, abstr, τ) where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec.String
import Text.Parsec
import Data.Char(isAsciiLower, isAsciiUpper, isDigit)
import Data.Data(Data)
import Data.Typeable(Typeable, cast)

import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse(parseExp, parsePat)

import Test.QuickCheck

import Interpreter.Core.Expr(
  Abstraction(..), Expr(..), Lambda(..), Type(..), AnyAbstr(..),
  Primitive(..), VarName, keywords, varNameGen)

import Interpreter.Core.Operators(operators, shunt)
-----------------------------------------------------------------------------
data ParseMode = QQMode | Interactive
  deriving (Eq, Enum)

allModes :: [ParseMode]
allModes = [QQMode .. Interactive]

lambdaExpression :: Parser (Expr Lambda)
lambdaExpression = qqExprToExpr <$> expressionParser Interactive

typeExpression :: Parser (Expr Type)
typeExpression = qqExprToExpr <$> expressionParser Interactive

--------------------------------------------------------------------------------
type HaskellCode = String
-- | The data type for parsed expressions, which is isomorphic to normal lambda
-- expressions with an additional QuasiQuoter constructor
data QQExpr a =
    QQVar VarName
  | QQSym Primitive
  | QQAbstr a (Either HaskellCode VarName) (QQExpr a)
  | QQApp (QQExpr a) (QQExpr a)
  | QQ (QQ a)
  deriving (Eq, Typeable, Data)

-- Some things should only be parsed for quasi-quoters and not in the
-- interactive evaluation mode.
-- The 'UnQuote' constructor is only ever used for "anti-quoting", while
-- WildCards (_) are used in pattern matching.
-- Also, the strictness operator '!' is only available for quasi-quoting.
-- The implementation is not as beautiful as it should be, so the code
-- should be refactored when I know how.

data QQ a =
    UnQuote HaskellCode                         -- explicitly unquoted Haskell code
  | WildCard                                    -- wildcard _ for patterns
  | WildCardAbstr a [Maybe VarName] (QQExpr a)  -- maybe these can be allowed for regular abstractions: e.g. λx _.x
  deriving (Eq, Typeable, Data)

-- One-to-one translation of parsed expressions to expressions
qqExprToExpr :: QQExpr a -> Expr a
qqExprToExpr (QQVar v) = Var v
qqExprToExpr (QQSym p) = Sym p
qqExprToExpr (QQAbstr a (Right v) e) = Abstr a v (qqExprToExpr e)
qqExprToExpr (QQApp e1 e2) = App (qqExprToExpr e1) (qqExprToExpr e2)
-- This case should not occur. Maybe there is a way to avoid a partial function.
qqExprToExpr _ = error "qqExprToExpr: unexpected Constructor"
-----------------------------------------------------------------------------
operator :: Parser VarName
operator = choice (map (try . string) operators)

varName :: Parser VarName
varName =
  (do v <- variable
      if v `elem` keywords
      then fail $ "expected: variable name\nencountered keyword: '" ++ v ++ "'"
      else return v
  )
    <|> between (char '(') (char ')') operator
  where
    variable = (:) <$> satisfy startSymbol
                   <*> many (satisfy (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || (x == '\'')))
    startSymbol = isAsciiLower
-----------------------------------------------------------------------------
expressionParser :: Abstraction a => ParseMode -> Parser (QQExpr a)
expressionParser mode = expr <* eof
  where
    expr = shunt (\op -> QQApp . QQApp (QQVar op)) <$> app <*> many infixOp
    infixOp = (,) <$> between spaces spaces operator <*> app
    app = foldl1 QQApp <$> operand `sepEndBy1` spaces
    num = many1 digit
    abstrSym = string "."
    unquote = between (char '{') (char '}') (many $ noneOf "{}")
    wildcard = char '_'
    maybewildcard = (Just <$> varName) <|> (wildcard >> return Nothing)
    operand = choice
        [try p | (p, modes) <-
          [
            ((\a args e -> foldr (QQAbstr a) e args)            <$>
              symbolParser                                      <*>
             (spaces *> (Right <$> varName) `sepEndBy1` spaces) <*>
             (abstrSym *> spaces *> expr)
            ,allModes)

          , ((\a args e -> foldr (QQAbstr a) e args)               <$>
              symbolParser                                         <*>
             (spaces *> ((Right <$> varName)
                     <|> (Left <$> unquote)) `sepEndBy1` spaces)   <*>
             (abstrSym *> spaces *> expr)
            ,[QQMode])

          , ((\a args body -> QQ (WildCardAbstr a args body)) <$>
              symbolParser                                    <*>
             (spaces *> maybewildcard `sepEndBy1` spaces)     <*>
             (abstrSym *> spaces *> expr)
            ,[QQMode])
          , (QQ . UnQuote <$> unquote, [QQMode])
          , (wildcard >> return (QQ WildCard), [QQMode])
          , (QQSym . Number . read <$> num, allModes)
          , (QQVar <$> varName, allModes)
          , (between (spaces *> char '(' *> spaces)
                     (spaces *> char ')' *> spaces)
                     expr, allModes)
          ]
        , mode `elem` modes]
-----------------------------------------------------------------------------
class LambdaVar a b where
  toVarName :: a -> VarName
  toExpr :: Abstraction b => a -> Expr b

instance LambdaVar VarName b where
  toVarName = id
  toExpr = Var

instance LambdaVar (Expr a) a where
  toVarName (Var x) = x
  toVarName _ = error "toVarName: encountered non-variable"
  toExpr = id
-----------------------------------------------------------------------------
-- QuasiQuoter stuff:

lambdaToExp :: (Abstraction a, Typeable a, Data a) => Parser (QQExpr a) -> String -> ExpQ
lambdaToExp parser s = case parse parser "" s of
  Left err   -> error (show err)
  Right expr -> toExp expr
  where
    -- If our abstraction type is "AnyAbstr", we don't want to fix the type of the created
    -- expression. Instead we return a polymorphic variable.
    abstrE a = case cast a :: Maybe AnyAbstr of
      Nothing -> dataToExpQ (const Nothing) a
      Just (AnyAbstr c) -> varE (mkName [c])

    toExp :: (Abstraction a, Typeable a, Data a) => QQExpr a -> ExpQ
    toExp (QQSym sym)        = [|Sym $(dataToExpQ (const Nothing) sym)|]
    toExp (QQApp e1 e2)      = [|App $(toExp e1) $(toExp e2)|]
    toExp (QQVar v)          = [|Var v|]
    toExp (QQAbstr a (Right v) expr) = [|Abstr $(abstrE a) v $(toExp expr)|]
    toExp (QQAbstr a (Left code) expr) = [|Abstr $(abstrE a) $(v) $(toExp expr)|]
      where
        v = case parseExp code of
              Left _  -> error "toExp: could not parse"
              Right e -> return e

    -- Unquoting means, we have Haskell code to parse and splice in.
    toExp (QQ (UnQuote code)) =
      case parseExp code of
        Left _     -> error "toExp: could not parse"
        Right expr -> return expr

    toExp (QQ _) = error "toExp: encountered unsupported QQ construct"
-----------------------------------------------------------------------------
lambdaToPat :: (Abstraction a, Typeable a, Data a) => Parser (QQExpr a) -> String -> Q Pat
lambdaToPat parser s = case parse parser "" s of
  Left err   -> error (show err)
  Right expr -> toPat expr
  where
    abstrP a = case cast a :: Maybe AnyAbstr of
      Nothing -> dataToPatQ (const Nothing) a
      Just (AnyAbstr c) -> varP (mkName [c])

    toPat (QQSym sym)     = conP (mkName "Sym") [dataToPatQ (const Nothing) sym]
    toPat (QQVar v)
      | v `elem` operators = conP (mkName "Var") [litP $ StringL v]
      | otherwise          = varP (mkName v)
    toPat (QQApp e1 e2) = conP (mkName "App") [toPat e1, toPat e2]
    toPat (QQAbstr a (Right v) expr) = conP (mkName "Abstr") [abstrP a, varP (mkName v), toPat expr]
    toPat (QQAbstr a (Left code) expr) = conP (mkName "Abstr") [abstrP a, v, toPat expr]
      where
        v = case parsePat code of
              Left _    -> fail "toPat: could not parse"
              Right pat -> return pat

    toPat (QQ WildCard) = wildP

    toPat (QQ (UnQuote code)) =
      case parsePat code of
        Left _    -> fail "toPat: could not parse"
        Right pat -> return pat

    toPat (QQ (WildCardAbstr a args expr)) = foldl
      (\pat arg -> conP (mkName "Abstr") [abstrP a, maybe wildP (varP . mkName) arg, pat])
      (toPat expr)
      args
-----------------------------------------------------------------------------
λ :: QuasiQuoter
λ = QuasiQuoter
  { quoteExp  = lambdaToExp parser
  , quotePat  = lambdaToPat parser
  , quoteType = fail "λ: quoteType"
  , quoteDec  = fail "λ: quoteDec"
  }
  where
    parser :: Parser (QQExpr Lambda)
    parser = expressionParser QQMode

τ :: QuasiQuoter
τ = QuasiQuoter
  { quoteExp  = lambdaToExp parser
  , quotePat  = lambdaToPat parser
  , quoteType = fail "τ: quoteType"
  , quoteDec  = fail "τ: quoteDec"
  }
  where
    parser :: Parser (QQExpr Type)
    parser = expressionParser QQMode

abstr :: QuasiQuoter
abstr = QuasiQuoter
  { quoteExp  = lambdaToExp parser
  , quotePat  = lambdaToPat parser
  , quoteType = fail "abstr: quoteType"
  , quoteDec  = fail "abstr: quoteDec"
  }
  where
    parser :: Parser (QQExpr AnyAbstr)
    parser = expressionParser QQMode

-----------------------------------------------------------------------------
-- QuickCheck tests:
-----------------------------------------------------------------------------
-- | test whether a printed expression can be parsed to the same expression
testShowEqualsParsed :: IO ()
testShowEqualsParsed = verboseCheckWith (stdArgs {maxSuccess = 10000, maxSize = 10})
  ((\expr ->
    case parse (expressionParser Interactive) "" (show expr) of
      Left _      -> False
      Right expr' -> expr == qqExprToExpr expr'
  ) :: Expr Lambda -> Bool)

testVariableParsing :: IO ()
testVariableParsing = verboseCheckWith (stdArgs {maxSuccess = 10000, maxSize = 10})
  (forAll varNameGen $ \v ->
    case parse (expressionParser Interactive :: Parser (QQExpr Lambda)) "" v of
      Right (QQVar v') -> v == v'
      _                    -> False
  )
-----------------------------------------------------------------------------
