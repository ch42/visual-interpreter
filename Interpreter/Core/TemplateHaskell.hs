{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interpreter.Core.TemplateHaskell(generateCommands, generateCommandParser) where

import Text.Parsec

import Language.Haskell.TH
import Control.Monad(void)
import Control.Applicative((<$>),(<*>),(<*))

import Interpreter.Core.Expr hiding (Type)
import Interpreter.Core.Parser
import Interpreter.Core.Commands
import Interpreter.Core.Semantics(Subst(..))

--------------------------------------------------------------------------------
-- | generate the @Command@ algebraic data type
--------------------------------------------------------------------------------
generateCommands :: DecsQ
generateCommands = (:[]) <$>
  dataD (cxt []) (mkName "Command") [] (map genCons (getInfo commands)) [mkName "Show"]
  where
    genCons :: CommandInfo -> ConQ
    genCons (CommandInfo name _ args _) = 
      normalC (mkName name) (concatMap (map (strictType isStrict) . typeFromArg) args)
    ----------------------------------------------------------------------------
    typeFromArg :: Arg -> [TypeQ]
    typeFromArg ArgExpr  = [[t|Expr Lambda|]]
    typeFromArg ArgVar   = [[t|VarName|]]
    typeFromArg ArgSubst = [[t|Subst Lambda|]]
    typeFromArg ArgDef   = [[t|VarName|],[t|Expr Lambda|]]
--------------------------------------------------------------------------------
-- | generate a parser for the @Command@ data structure
--------------------------------------------------------------------------------
generateCommandParser :: Name -> ExpQ
generateCommandParser name = do
  i <- reify name
  case i of
    TyConI (DataD _ _ _ constructors _) ->

        let mkParserForCmd :: Con -> CommandInfo -> ExpQ
            mkParserForCmd (NormalC cName _) (CommandInfo {cmdSyntax, cmdArgs}) =
                    [|try $ $(command) >> $(args)|]
              where
                cmdParser ""  = [|return ()|]
                cmdParser cmd = [|(void . try . string) cmd >> (void (many1 space) <|> eof)|]
                command       = [|choice $(listE (map cmdParser cmdSyntax))|]
                con           = conE cName
                args          = foldArgs (con : concatMap parserForArg cmdArgs)

            mkParserForCmd _ _ = fail "mkParserForCmd: expected 'NormalC'"

        in [|choice $(listE (zipWith mkParserForCmd constructors (getInfo commands) )) <* eof|]

    _ -> fail "generateCommandParser: expected 'TyConI' as result of reify"

  where
    foldArgs :: [ExpQ] -> ExpQ
    foldArgs []         = fail "foldArgs: expected non-empty list"
    foldArgs [c]        = [|return $(c)|]
    foldArgs (con:a:as) = foldl1 (\x y -> [|$(x) <*> (spaces >> $(y))|])
                                 ([|$(con) <$> $(a)|]:as)
    ----------------------------------------------------------------------------
    parserForArg :: Arg -> [ExpQ]
    parserForArg ArgExpr  = [[|lambdaExpression|]]
    parserForArg ArgVar   = [[|varName|]]
    parserForArg ArgSubst = [[|between (char '[' >> spaces) (spaces >> char ']')
                                  ((:=) <$> varName
                                        <*> (spaces >> string ":=" >>
                                             spaces >> lambdaExpression))|]]
    parserForArg ArgDef   = [[|varName|],
                             [|spaces >> char '=' >> spaces >> lambdaExpression|]]
--------------------------------------------------------------------------------
