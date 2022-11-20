module Main (main) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import Data.Function ((&))
import Token (tokenize)
import Types (Visitor (visitAll))
import Visitor.CalcVisitor (CalcState (CalcState), CalcVisitor)
import Visitor.ParserVisitor (ParserState (ParserState), ParserVisitor)
import Visitor.PrintVisitor (PrintVisitor)

main :: IO ()
main = do
  input <- getLine
  case tokenize input of
    Left err -> print err
    Right tokens -> do
      let (eParsedTokens, _) = visitAll @ParserVisitor tokens
            & runExceptT
            & flip runState (ParserState [] [])

      case eParsedTokens of
        Left err -> print err
        Right parsedTokens -> do
          visitAll @PrintVisitor parsedTokens
          let (eResult, _) = visitAll @CalcVisitor parsedTokens
                & runExceptT
                & flip runState (CalcState [])

          case eResult of
            Left err -> print err
            Right num -> print num
