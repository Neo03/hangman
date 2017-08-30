module Main () where

import Data.Char (toLower)
import Data.Maybe (isJust)
import Control.Monad (forever)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

main :: IO ()
main = undefined

newtype WordList = WordList[String] deriving (Eq, Ord, Show)

data Puzzle = Puzzle String [Maybe Char] [Char]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
    where
      gameLength w =
       let l = length (w :: String)
       in l < maxWordLength && l > minWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO( 0, length wl - 1 )
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

instance ShowPuzzle where show (Puzzle _ discovered guessed) =
  (intersperce ' ' $ fmap renderPuzzleChar discovered ) ++
  " Guesssed so far: " ++ guessed 
