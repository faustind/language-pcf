{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Cli where

import Control.Monad (void)
import Control.Monad.State.Strict
import Data.List (foldl', isPrefixOf)
import Data.Monoid
import System.Console.Repline
import System.Environment
import System.Exit

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Eval
import Parser
import Syntax

newtype IState =
  IState
    { tmctx :: ValCtx
    }

initState :: IState
initState = IState emptyValCtx

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

evalDef :: ValCtx -> (Binder, Expr) -> ValCtx
evalDef env (bndr, ex) = env'
  where
    (val, env') = runEval env bndr ex

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  st <- get
  mod <- hoistErr $ parseModule "<stdin>" source
  let st' = st {tmctx = foldl' evalDef (tmctx st) mod}
  when update (put st')
  case lookup "it" mod of
    Nothing -> return ()
    Just ex -> do
      let (val, _) = runEval (tmctx st') "it" ex
      showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st = liftIO $ putStrLn arg

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ L.readFile (unwords args)
  exec True contents

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [(":load", fileCompleter)]

comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":quit"]
  return $ filter (isPrefixOf n) cmds

options :: [(String, [String] -> Repl ())]
options = [("load", load), ("quit", quit)]

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

shell :: Repl () -> IO ()
shell pre =
  flip evalStateT initState $
  evalRepl (pure "PCF> ") cmd options (Just ':') completer pre

cli :: IO ()
cli = do
  args <- getArgs
  case args of
    [] -> shell (void (load ["lib/stdlib.pcf"]))
    [fname] -> shell (load ["lib/stdlib.pcf"] >> load [fname])
    ["run", fname] -> shell (load ["lib/stdlib.pcf"] >> load [fname] >> quit ())
    _ -> putStrLn "invalid arguments"
