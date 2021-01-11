module Main where

import Eval
import Parser
import Syntax

import Data.Maybe

import Control.Monad.Trans
import System.Console.Haskeline

import qualified Data.Map as Map
import qualified Data.Text.Lazy as L

emptyValCtx = Map.empty

runEval' = runEval emptyValCtx

-- runEval' = runEval emptyValCtx
process :: String -> IO ()
process line = do
  let res = parseExpr (L.pack line)
  case res of
    Left err -> print err
    Right expr -> do
      print expr
      val <- runEval' (eval expr)
      print val

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "PCF> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
