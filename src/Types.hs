module Types
  ( TokenType (..)
  , TokenParenthesis (..)
  , TokenOperation (..)
  , Token (..)
  , SomeToken (..)
  , Visitor (..)
  , accept
  ) where

import Control.Monad.Except (MonadError)
import Data.Kind (Type)

data TokenType
  = TParen
  | TNumber
  | TOperation
  deriving stock (Show, Eq, Ord)

data TokenParenthesis
  = LParen
  | RParen
  deriving stock (Show, Eq, Ord)

data TokenOperation
  = Plus
  | Minus
  | Mult
  | Div
  deriving stock (Show, Eq, Ord)

data Token (t :: TokenType) where
  Paren :: TokenParenthesis -> Token 'TParen
  Number :: Integer -> Token 'TNumber
  Operation :: TokenOperation -> Token 'TOperation

deriving stock instance Show (Token t)
deriving stock instance Eq (Token t)
deriving stock instance Ord (Token t)

data SomeToken where
  SomeToken :: Token t -> SomeToken

instance Show SomeToken where
  show (SomeToken token) = show token

type VisitorConstraints visitor =
  (MonadError (VisitorError visitor) (VisitorMonad visitor))

type VisitorFunction visitor t =
    (VisitorConstraints visitor)
    => Token t
    -> VisitorMonad visitor ()

class Visitor visitor where
  type VisitorMonad visitor :: Type -> Type
  type VisitorResult visitor :: Type
  type VisitorError visitor :: Type

  visitParen :: VisitorFunction visitor 'TParen
  visitNumber :: VisitorFunction visitor 'TNumber
  visitOperation :: VisitorFunction visitor 'TOperation

  visitAll :: [SomeToken] -> VisitorMonad visitor (VisitorResult visitor)

accept
  :: forall visitor
   . (Visitor visitor, VisitorConstraints visitor)
  => SomeToken
  -> VisitorMonad visitor ()
accept (SomeToken token) = case token of
  Paren{} -> visitParen @visitor token
  Number{} -> visitNumber @visitor token
  Operation{} -> visitOperation @visitor token
