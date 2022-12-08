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

getUserInput ::  Text.Text -> BotM (Maybe Type)
getUserInput userChoice
    | userChoice == "Rock" = return $ Just Rock
    | userChoice == "Paper" = return $ Just Paper
    | userChoice == "Scissors" = return $ Just Scissors
    | otherwise = return Nothing

getComputerInput :: BotM Type
getComputerInput = do
    res <- randomIO :: BotM Int
    case res `mod` 3 of
        0 -> return Rock
        1 -> return Paper
        2 -> return Scissors

getComputerInputInfo :: Type -> Text.Text
getComputerInputInfo computerInput =
    Text.pack $ "Computer's choice is: " <> show computerInput <> "\n"

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

getWinnerInfo :: Winner -> Text.Text
getWinnerInfo winner =
    case winner of
        User -> "You won!"
        Computer -> "Computer won!"
        Draw -> "Nobody won:("

printScore :: Int -> Int -> Text.Text
printScore computerScore userScore = Text.unlines
    [ Text.pack $ "Computer's score is " ++ (show computerScore)
    , Text.pack $ "Your score is " ++ (show userScore)
    ]

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
