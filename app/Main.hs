module Main where

import Control.Monad (void)
import System.Environment

import Cli

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> shell (void (load ["lib/stdlib.pcf"])) -- A rpel with a stdlib
    ["bare"] -> shell (return ()) -- Do not load standard lib in the repel
    [fname] -> shell (load ["lib/stdlib.pcf"] >> load [fname]) --  load a file with the stdlib in rpel
    ["run", fname] -> shell (load ["lib/stdlib.pcf"] >> load [fname] >> quit ()) -- run the file
    _ -> putStrLn "invalid arguments"
