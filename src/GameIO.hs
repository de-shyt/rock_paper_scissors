module GameIO where

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
    "no" -> exitGame
    "n" -> exitGame
    _ -> do
      putStrLn "The answer should be yes or no."
      nextRound

introduction :: IO()
introduction = do
  putStrLn "Welcome to rock-paper-scissors game!\n"

exitGame :: IO()
exitGame = do
  putStrLn "\nThank you for playing! Bye!"
  return ()

runRound :: IO ()
runRound = do
  userInput <- GL.getUserInput
  computerInput <- GL.getComputerInput
  let gameResult = GL.winLoseDraw userInput computerInput
  winnerInfoOutput gameResult
  nextRound

startGame :: IO()
startGame = do
  introduction
  runRound