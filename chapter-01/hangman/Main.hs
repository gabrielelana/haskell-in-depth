{-# LANGUAGE OverloadedStrings #-}

module Main where

import Fmt
import System.Random (randomRIO)
import Control.Monad (unless, when)
import Data.Maybe (isJust)

type Dictionary = [String]

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Buildable Puzzle where
  build p@(Puzzle word filledIn _) = nameF "guess" $ unwordsF [guessW filledIn, parenF $ guessesLeft p, build word]
    where
      guessW = foldMap (build . guessC)
      guessC Nothing = "_"
      guessC (Just c) = [c]

parenF :: Buildable a => a -> Builder
parenF b = "(" +| b |+ ")"

guessesLeft :: Puzzle -> Int
guessesLeft (Puzzle _ _ cs) = guessesMax - (length cs)

guessesMax :: Int
guessesMax = 6

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (const Nothing <$> w) []

gameOver :: Puzzle -> Bool
gameOver p = (gameWon p) || (guessesLeft p) <= 0

gameWon :: Puzzle -> Bool
gameWon (Puzzle _ filledIn _) = all isJust filledIn

handleGuess :: Puzzle -> Char -> Puzzle
handleGuess = fillInChar

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

fillInChar :: Puzzle -> Char -> Puzzle
fillInChar p@(Puzzle w filledIn g) c =
  if alreadyGuessed p c
  then p
  else Puzzle w (zipWith zipper w filledIn) (c:g)
  where
    zipper wc gc
      | c == wc = Just wc
      | otherwise = gc

dictionary :: IO Dictionary
dictionary = do
  content <- readFile "./chapter-01/hangman/words.txt"
  return $ lines content

pickWord :: Dictionary -> IO String
pickWord d = do
  index <- randomRIO (0, length d - 1)
  return $ d !! index

runHand :: Puzzle -> IO ()
runHand p = do
  fmt $ build p
  putStrLn "Guess a letter: "
  guess <- getLine
  p' <- case guess of
          [c] -> pure $ handleGuess p c
          _   -> putStrLn "Your guess must be a single character." >> pure p
  when (gameWon p') $ putStrLn "You guessed right!"
  unless (gameOver p') (runHand p')

runGame :: Dictionary -> IO ()
runGame d = do
  p <- freshPuzzle <$> pickWord d
  runHand p
  putStrLn "Another game? (y/n)"
  another <- getLine
  case another of
    "y" -> runGame d
    _ -> putStrLn "See you space cowboy"

main :: IO ()
main = do
  dictionary >>= runGame
