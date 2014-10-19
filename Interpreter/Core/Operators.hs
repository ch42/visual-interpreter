module Interpreter.Core.Operators(
  defaultFixities, operators, Op(..), shunt, Fixity(..), FixityDirection(..))
  where

import Control.Arrow(first)
import Data.List.Zipper as Zipper
import Data.Maybe(fromMaybe)
import Language.Haskell.TH(defaultFixity, Fixity(..), FixityDirection(..))
--------------------------------------------------------------------------------
defaultFixities :: [(String, Fixity)]
defaultFixities = 
    [
      ("->", Fixity 0 InfixR)
     ,("+",  Fixity 6 InfixL)
     ,("-",  Fixity 6 InfixL)
     ,("*",  Fixity 7 InfixL)
     ,("/",  Fixity 7 InfixL)
     ,(">",  Fixity 4 InfixN)
     ,("<",  Fixity 4 InfixN)
    ]

operators :: [String]
operators = map fst defaultFixities

data Op = Op String Fixity deriving (Eq)

instance Ord Op where
  Op _ (Fixity precA assocA) > Op _ (Fixity precB _) =
    precA > precB || (precA == precB && assocA == InfixL)
--------------------------------------------------------------------------------
-- | implementation of the shunting yard algorithm for infix operator application
shunt :: (String -> expr -> expr -> expr) -> expr -> [(String, expr)] -> expr
shunt binOpFunction e xs = shunt' e (Zipper.fromList . map (first toOp) $ xs)
  where
    toOp opName = Op opName (fromMaybe defaultFixity (lookup opName defaultFixities))
    -- Invariant: expR is always the right subexpression in the infix expression we want
    -- to build. ops is the list of operators we're traversing and consuming until it's empty.
    shunt' expR ops
      | emptyp ops = expR       -- no more operators: we're done
      | beginp ops = moveRight  -- just beginning? move right (otherwise we have no left expression)
      | endp ops   = applyOp    -- at the end? start applying (all) operators
      | opL > opR  = applyOp    -- do we have higher precedence? apply the operator
      | otherwise  = moveRight  -- if not, look further to the right
      where
        -- Note: the following values are evaluated lazily.
        (opL,expL)  = cursor (left ops)
        (opR,expR') = cursor ops

        -- moveRight: expR will become the new exprL in the next step,
        -- while expR' becomes the new expR
        moveRight = shunt' expR' (right (replace (opR, expR) ops))
        applyOp   = shunt' (bin opL expL expR) (pop ops)

    -- a binary expression is a curried function application
    bin (Op opName _) = binOpFunction opName --App . App (Var opName)
--------------------------------------------------------------------------------
