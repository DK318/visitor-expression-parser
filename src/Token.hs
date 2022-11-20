{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Token
  ( tokenize
  ) where

import Control.Lens (abbreviatedFields, cons, makeLensesWith, uncons, use, (%=), (.=))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Extra (unlessM, whenJust)
import Control.Monad.State (State, runState)
import Data.Char (isSpace)
import Types (SomeToken (..), Token (..), TokenOperation (..), TokenParenthesis (..))

data TokenState
  = StartState
  | NumberState Integer
  | EndState
  deriving stock (Show, Eq, Ord)

data TokenError
  = UnexpectedTokenError Char
  | StreamRemainsError String
  deriving stock (Eq, Ord)

instance Show TokenError where
  show = \case
    UnexpectedTokenError ch -> "Unexpected symbol met: " <> [ch]
    StreamRemainsError str -> "Stream is not empty after tokenizing: " <> str

data TokenizeState = TokenizeState
  { tsInputStream :: String
  , tsTokenState :: TokenState
  , tsParsedTokens :: [SomeToken]
  }

makeLensesWith abbreviatedFields ''TokenizeState

type TokenizeMonad = ExceptT TokenError (State TokenizeState)

tokenize :: String -> Either TokenError [SomeToken]
tokenize expr = case eError of
  Left err -> Left err
  Right () -> Right $ reverse (tsParsedTokens endState)
  where
    (eError, endState) = runState (runExceptT go) initState

    initState = TokenizeState
      { tsInputStream = expr
      , tsTokenState = StartState
      , tsParsedTokens = []
      }

    go :: TokenizeMonad ()
    go = use tokenState >>= \case
      StartState -> do
        chMb <- getCharMb
        whenJust chMb \case
          '(' -> parsedTokens %= cons (SomeToken $ Paren LParen)
          ')' -> parsedTokens %= cons (SomeToken $ Paren RParen)
          '+' -> parsedTokens %= cons (SomeToken $ Operation Plus)
          '-' -> parsedTokens %= cons (SomeToken $ Operation Minus)
          '*' -> parsedTokens %= cons (SomeToken $ Operation Mult)
          '/' -> parsedTokens %= cons (SomeToken $ Operation Div)
          other
            | '0' <= other && other <= '9' -> do
                tokenState .= NumberState 0
                inputStream %= cons other
            | isSpace other -> pure ()
            | otherwise -> throwError $ UnexpectedTokenError other
        go
      NumberState n -> do
        let stopParsing :: TokenizeMonad ()
            stopParsing = do
              parsedTokens %= cons (SomeToken $ Number n)
              tokenState .= StartState

        getCharMb >>= \case
          Just ch -> do
            if '0' <= ch && ch <= '9'
            then do
              let parsed = read @Integer [ch]
              tokenState .= NumberState (n * 10 + parsed)
            else do
              stopParsing
              inputStream %= cons ch
          Nothing -> stopParsing

        go
      EndState -> do
        unlessM (null <$> use inputStream) do
          use inputStream >>= throwError . StreamRemainsError

    getCharMb :: TokenizeMonad (Maybe Char)
    getCharMb = uncons <$> use inputStream >>= \case
      Nothing -> do
        tokenState .= EndState
        pure Nothing
      Just (ch, cont) -> do
        inputStream .= cont
        pure $ Just ch
