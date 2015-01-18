{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Replay.Example where

import Data.Monoid (mempty)
import Control.Monad (when, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, runReader, ask)
import Control.Monad.Trans.State (State, runState, put)
import Replay

type Game = ReplayT String (Reader String) (State (String, Bool))

exampleGame :: Game ()
exampleGame = do
  display "Welcome to the game!"
  name <- prompt "What is your name?"
  display $ "Hello, " ++ name ++ "!"
  choice <- prompt "Would you like to hear something funny?"
  when (choice == "yes") $ display "'something funny'."

display :: String -> Game ()
display s = do
  lift $ put (s, False)
  void $ record ask

prompt :: String -> Game String
prompt s = do
  lift $ put (s, True)
  record ask

evalGame :: Game () -> [String] -> IO [String]
evalGame g init =
  case (runState (replay init g) (mempty, False)) of
    (Left (), _) -> return init
    (Right reader, (displayString, isInput)) -> do
      putStrLn displayString
      if isInput then putStr "('save' to save and quit; otherwise give input) > "
                 else putStr "('save' to save and quit; anything else to continue) > "
      input <- getLine
      if (input == "save") then return init
                           else evalGame g $ runReader reader input
