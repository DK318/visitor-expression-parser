{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Visitor.ParserVisitor
  ( ParserVisitor
  , ParserState (..)
  ) where

import Control.Lens (abbreviatedFields, cons, makeLensesWith, uncons, use, (%=), (.=))
import Control.Monad.Except (ExceptT, MonadError (throwError), forM_)
import Control.Monad.State (State)
import Types
  (SomeToken (..), Token (..), TokenOperation (..), TokenParenthesis (..), Visitor (..), accept)

data ParserVisitor

data ParseError
  = UnexpectedEndOfStackError
  | ExtraParenthesisError SomeToken

instance Show ParseError where
  show = \case
    UnexpectedEndOfStackError -> "Can't find \"(\" in stack while popping elements"
    ExtraParenthesisError token -> "Unexpected token after parsing: " <> show token

data ParserState = ParserState
  { psStack :: [SomeToken]
  , psResult :: [SomeToken]
  }

makeLensesWith abbreviatedFields ''ParserState

getElemFromStack :: VisitorMonad ParserVisitor (Maybe SomeToken)
getElemFromStack = uncons <$> use stack >>= \case
  Just (el, rest) -> do
    stack .= rest
    pure $ Just el
  Nothing -> pure Nothing

getPriority :: TokenOperation -> Integer
getPriority = \case
  Plus -> 1
  Minus -> 1
  Mult -> 2
  Div -> 2

instance Visitor ParserVisitor where
  type VisitorMonad ParserVisitor = ExceptT ParseError (State ParserState)
  type VisitorError ParserVisitor = ParseError
  type VisitorResult ParserVisitor = [SomeToken]

  visitParen (Paren paren) = case paren of
    LParen -> stack %= cons (SomeToken $ Paren paren)
    RParen -> do
      let go :: VisitorMonad ParserVisitor ()
          go = getElemFromStack >>= \case
            Nothing -> throwError UnexpectedEndOfStackError
            Just (SomeToken (Paren LParen)) -> pure ()
            Just otherToken -> result %= cons otherToken >> go

      go

  visitNumber num = result %= cons (SomeToken num)
  visitOperation (Operation operation) = do
    let go :: VisitorMonad ParserVisitor ()
        go = getElemFromStack >>= \case
          Nothing -> pure ()
          Just (SomeToken (Operation otherOperation))
            | getPriority operation <= getPriority otherOperation -> do
                result %= cons (SomeToken $ Operation otherOperation)
                go
            | otherwise -> stack %= cons (SomeToken $ Operation otherOperation)
          Just otherToken -> stack %= cons otherToken

    go
    stack %= cons (SomeToken $ Operation operation)

  visitAll tokens = do
    mapM_ (accept @ParserVisitor) tokens
    st <- use stack

    forM_ st \case
      operation@(SomeToken Operation{}) -> result %= cons operation
      other -> throwError @_ @_ @() $ ExtraParenthesisError other

    reverse <$> use result
