{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Visitor.CalcVisitor
  ( CalcVisitor
  , CalcState (..)
  , CalcError (..)
  ) where

import Control.Lens (abbreviatedFields, cons, makeLensesWith, uncons, use, (%=), (.=))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Extra (unlessM)
import Control.Monad.State (State)
import Types (SomeToken (..), Token (..), TokenOperation (..), Visitor (..), accept)

data CalcVisitor

data CalcError
  = InsufficientElementsError
  | UnexpectedParenthesisError
  | UnexpectedTokenError
  | EmptyStackError
  | EndTokenError SomeToken
  | EndStackNonEmpty

instance Show CalcError where
  show = \case
    InsufficientElementsError -> "Insufficient number of elements in stack"
    UnexpectedParenthesisError -> "Unexpected parenthesis"
    UnexpectedTokenError -> "Unexpected token was met while evaluating operation"
    EmptyStackError -> "Stack is empty after evaluation"
    EndTokenError token -> "Unexpected token after evaluation: " <> show token
    EndStackNonEmpty -> "Stack has other elements after evaluation"

newtype CalcState = CalcState { csStack :: [SomeToken] }

makeLensesWith abbreviatedFields ''CalcState

getElemFromStack :: VisitorMonad CalcVisitor (Maybe SomeToken)
getElemFromStack = uncons <$> use stack >>= \case
  Nothing -> pure Nothing
  Just (el, rest) -> do
    stack .= rest
    pure $ Just el

getOperation :: TokenOperation -> Integer -> Integer -> Integer
getOperation = \case
  Plus -> (+)
  Minus -> (-)
  Mult -> (*)
  Div -> div

instance Visitor CalcVisitor where
  type VisitorMonad CalcVisitor = ExceptT CalcError (State CalcState)
  type VisitorError CalcVisitor = CalcError
  type VisitorResult CalcVisitor = Integer

  visitParen _ = throwError UnexpectedParenthesisError
  visitNumber num = stack %= cons (SomeToken num)
  visitOperation (Operation operation) = do
    elRhsMb <- getElemFromStack
    elLhsMb <- getElemFromStack
    case (elLhsMb, elRhsMb) of
      (Just (SomeToken (Number numLhs)), Just (SomeToken (Number numRhs))) -> do
        let numOp = getOperation operation
        let result = numLhs `numOp` numRhs
        stack %= cons (SomeToken $ Number result)
      (Just{}, Just{}) -> throwError @_ @_ @() UnexpectedTokenError
      _ -> throwError InsufficientElementsError

  visitAll tokens = do
    mapM_ (accept @CalcVisitor) tokens
    getElemFromStack >>= \case
      Nothing -> throwError EmptyStackError
      Just (SomeToken (Number num)) -> do
        unlessM (null <$> use stack) do
          throwError EndStackNonEmpty

        pure num
      Just other -> throwError $ EndTokenError other
