module Main  where

import Data.Char (toLower)
import Data.Maybe (isJust)
import Control.Monad (forever)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

main :: IO ()
main = do
        word <- randomWord'
        let puzzle = freshPuzzle (fmap toLower word)
        runGame puzzle

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


instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered ) ++
    " Guesssed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s [Nothing | x <- s] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s ) c =
  Puzzle word newFilledInSoFar (c:s) where
    zipper guessed wordChar guessChar =
      if guessed == wordChar
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess ) of
    (_, True) -> do
      putStrLn "You already guessed this word, please pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly."
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character n't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO()
gameOver (Puzzle wordToGuess discovered guessed) =
  if (length guessed - length (filter isJust discovered)) > 7
    then
      do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
    else return ()

gameWin :: Puzzle -> IO()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then
      do
        putStrLn "You win!"
        exitSuccess
    else return()

runGame :: Puzzle -> IO()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin  puzzle
  putStrLn $ "Current puzzle was: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a character."
