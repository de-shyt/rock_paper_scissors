{-# LANGUAGE OverloadedStrings #-}

module GameLogic where

import System.Random
import Text.Read
import qualified Data.Text as Text

import qualified Telegram.Bot.API as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data Type = Rock | Paper | Scissors deriving (Show, Eq)
data Winner = User | Computer | Draw deriving (Show, Eq)

getComputerInput :: IO Type
getComputerInput = do
    g <- getStdGen
    let result = (head $ take 1 (randoms g :: [Int])) `mod` 3 in
        case result of
            0 -> return Rock
            1 -> return Paper
            2 -> return Scissors

findWinner :: Type -> Type -> Winner
findWinner userInput computerInput =
    case (userInput, computerInput) of
        (Rock, Paper) -> Computer
        (Rock, Scissors) -> User
        (Paper, Scissors) -> Computer
        (Paper, Rock) -> User
        (Scissors, Rock) -> Computer
        (Scissors, Paper) -> User
        (_, _) -> Draw

winnerInfoOutput :: Winner -> Text.Text
winnerInfoOutput winner =
    case winner of
        User -> "You won!"
        Computer -> "Computer won!"
        Draw -> "Nobody won:("

introduction :: Text.Text
introduction = Text.unlines [ "Welcome to rock-paper-scissors game!\n\n"] <> listOfCommands

listOfCommands :: Text.Text
listOfCommands = Text.unlines
    [ "Use /play to start a new round\n"
    , "Use /end or /exit to finish a current game\n"
    , "Use /help to see the list of commands"
    ]

exitGame :: Text.Text
exitGame = "Thank you for visiting us! Tschüß!"
