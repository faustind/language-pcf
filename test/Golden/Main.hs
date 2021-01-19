{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad (foldM)
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
    "Test eval"
    [ tastyGoldenRun "add" "test_files/add.pcf" "test_files/ans/add.txt"
    , tastyGoldenRun
        "list_cons"
        "test_files/list_cons.pcf"
        "test_files/ans/list_cons.txt"
    , tastyGoldenRun
        "list_head"
        "test_files/list_head.pcf"
        "test_files/ans/list_head.txt"
    , tastyGoldenRun
        "list_null_1"
        "test_files/list_null_1.pcf"
        "test_files/ans/list_null_1.pcf"
    , tastyGoldenRun
        "list_null_2"
        "test_files/list_null_2.pcf"
        "test_files/ans/list_null_2.pcf"
    , tastyGoldenRun
        "list_null_3"
        "test_files/list_null_3.pcf"
        "test_files/ans/list_null_3.pcf"
    , tastyGoldenRun
        "list_tail"
        "test_files/list_tail.pcf"
        "test_files/ans/list_tail.pcf"
    , tastyGoldenRun
        "stdlib_list_indx"
        "test_files/list_indx.pcf"
        "test_files/ans/list_indx.txt"
    , tastyGoldenRun
        "stdlib_map_nil"
        "test_files/map_nil.pcf"
        "test_files/ans/map_nil.txt"
    , tastyGoldenRun "stdlib_map" "test_files/map.pcf" "test_files/ans/map.txt"
    ]

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
  -- execute stdlib
  stdbindings' <- hoistErr $ parseModule "<stdin>" stdlib'
  stdenv <- foldM evalDef emptyValCtx stdbindings'
  -- get the code to test
  testfile' <- L.readFile (T.unpack testfile)
  mod <- hoistErr $ parseModule "<stdin>" testfile'
  -- execute the code using the standard library
  bindings <- foldM evalDef stdenv mod
  -- return the output
  case lookup "it" mod of
    Nothing -> return Undefined
    Just ex -> do
      (val, _) <- runEval bindings "it" ex
      return val
