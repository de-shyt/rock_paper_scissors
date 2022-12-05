{-# LANGUAGE OverloadedStrings #-}

module GameIO where
import Data.Text as Text

import qualified GameLogic as GL

winnerInfoOutput :: GL.Winner -> IO()
winnerInfoOutput winner =
    putStrLn sentence where
        sentence =
            case winner of
                GL.User -> ("You won!")
                GL.Computer -> ("Computer won!")
                GL.Draw -> ("Nobody won:(")

nextRound :: IO()
nextRound = do
    putStrLn "Do you want to play more? (yes/no)"
    userInput <- getLine
    case userInput of
        "yes" -> runRound
        "y" -> runRound
--        "no" -> exitGame
--        "n" -> exitGame
        _ -> do
            putStrLn "The answer should be yes or no."
            nextRound

introduction :: Text
introduction = Text.unlines [ "Welcome to rock-paper-scissors game!\n"] <> listOfCommands

listOfCommands :: Text
listOfCommands = Text.unlines
    [ "Use /start to start a new game"
    , "Use /play to start a new round"
    , "Use /end or /exit to finish a current game"
    , "Use /help to see the list of commands"
    ]

exitGame :: Text
exitGame = "Thank you for playing! Tschüß!"

runRound :: IO ()
runRound = do
    putStrLn "I'm runRound"
--    userInput <- GL.getUserInput
--    computerInput <- GL.getComputerInput
--    winnerInfoOutput $ GL.winLoseDraw userInput computerInput
--    nextRound

--startGame :: IO()
--startGame = do
--  introduction
--  runRound