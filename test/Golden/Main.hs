{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Eval

import qualified Data.Text as T

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Functor ((<&>))
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = putStrLn "Not implemented"
