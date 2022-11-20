module Visitor.PrintVisitor
  ( PrintVisitor
  ) where

import Control.Exception (IOException)
import Control.Monad (forM_)
import Types (Token (..), TokenOperation (..), TokenParenthesis (..), Visitor (..), accept)

data PrintVisitor

instance Visitor PrintVisitor where
  type VisitorMonad PrintVisitor = IO
  type VisitorError PrintVisitor = IOException
  type VisitorResult PrintVisitor = ()

  visitParen (Paren paren) = case paren of
    LParen -> putStr "("
    RParen -> putStr ")"

  visitNumber (Number n) = putStr $ "NUMBER(" <> show n <> ")"
  visitOperation (Operation operation) = case operation of
    Plus -> putStr "PLUS"
    Minus -> putStr "MINUS"
    Mult -> putStr "MULT"
    Div -> putStr "DIV"

  visitAll tokens = do
    forM_ tokens \token -> do
      accept @PrintVisitor token
      putStr " "
    putStrLn ""
