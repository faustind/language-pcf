{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.Functor ((<&>))
import Data.List (foldl')
import System.Exit
import Test.Tasty
import Test.Tasty.Golden

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Cli hiding (hoistErr)
import Eval
import Parser
import Syntax

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Golden Tests"
    [tastyGoldenRun "add" "test_files/add.pcf" "test_files/ans/add.txt"]

tastyGoldenRun :: TestName -> T.Text -> FilePath -> TestTree
tastyGoldenRun testName testFile correct =
  goldenVsString testName correct $ evalTextTest "lib/stdlib.pcf" testFile <&>
  C.pack .
  show

hoistErr :: Show e => Either e a -> IO a
hoistErr (Right val) = return val
hoistErr (Left err) = die (show err)

evalTextTest :: T.Text -> T.Text -> IO Expr
evalTextTest stdlib testfile = do
  stdlib' <- L.readFile (T.unpack stdlib)
  stdbindings' <- hoistErr $ parseModule "<stdin>" stdlib'
  let stdenv = foldl' evalDef emptyValCtx stdbindings'
  testfile' <- L.readFile (T.unpack testfile)
  mod <- hoistErr $ parseModule "<stdin>" testfile'
  let bindings = foldl' evalDef stdenv mod
  case lookup "it" mod of
    Nothing -> return Undefined
    Just ex -> do
      let (val, _) = runEval bindings "it" ex
      return val
