module Main () where

import Data.Char (toLower)
import Data.Maybe (isJust)
import Control.Monad (forever)
import Data.List (interprise)
import Sysytem.Exit (exitSuccess)
import System.Random (randomRIO)

main :: IO ()
main = do
  putStrLn "hello world"
