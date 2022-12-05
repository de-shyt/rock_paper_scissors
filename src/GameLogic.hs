{-# LANGUAGE OverloadedStrings #-}

module GameLogic where

import System.Random
import Text.Read

import qualified Telegram.Bot.API as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data Type = Rock | Paper | Scissors deriving (Show, Eq)
data Winner = User | Computer | Draw deriving (Show, Eq)

getUserInput :: IO Type
getUserInput = do
    putStrLn "Please, choose a number:\n  1) Rock\n  2) Paper\n  3) Scissors"
    answer <- getLine
    let amount = read answer :: Int
    case amount of
        1 -> return Rock
        2 -> return Paper
        3 -> return Scissors
        _ -> do
            putStrLn "The number should be from 1 to 3."
            getUserInput

getComputerInput :: IO Type
getComputerInput = do
    g <- getStdGen
    let result = (head $ take 1 (randoms g :: [Int])) `mod` 3 in
        case result of
            0 -> return Rock
            1 -> return Paper
            2 -> return Scissors

winLoseDraw :: Type -> Type -> Winner
winLoseDraw userInput computerInput =
    case (userInput, computerInput) of
        (Rock, Paper) -> Computer
        (Rock, Scissors) -> User
        (Paper, Scissors) -> Computer
        (Paper, Rock) -> User
        (Scissors, Rock) -> Computer
        (Scissors, Paper) -> User
        (_, _) -> Draw