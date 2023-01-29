{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI where

import qualified GameLogic as GL
import System.Random

import Control.Monad.Trans (liftIO)  -- to transform BotM() to IO()

import           Control.Applicative              ((<|>))
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Char
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

data State
    = Start
    | Play
    | Wait
    | Stopped
    deriving (Show)


data Model = Model
    { state :: State
    , computerScore :: Int
    , userScore :: Int
    }
    deriving (Show)


data Action
    = NoAction
    | Welcome
    | Help
    | DoYouWantToPlay
    | RunRound
    | Input Text
    | ExitGame
    deriving (Show)

bot :: BotApp Model Action
bot = BotApp
    { botInitialModel = Model { state = Start, computerScore = 0, userScore = 0 }
    , botAction = flip handleUpdate
    , botHandler = handleAction
    , botJobs = []  -- a sequence jobs that a bot needs to do
    }

startInlineKeyboard :: Telegram.ReplyKeyboardMarkup
startInlineKeyboard = Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard = [ [ "/start" ] ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just False
    , Telegram.replyKeyboardMarkupSelective = Just True
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
}

playInlineKeyboard :: Telegram.ReplyKeyboardMarkup
playInlineKeyboard = Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard = [ [ "/play" ] ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just False
    , Telegram.replyKeyboardMarkupSelective = Just True
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
}

doYouWantToPlayInlineKeyboard :: Telegram.ReplyKeyboardMarkup
doYouWantToPlayInlineKeyboard = Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard =
        [ [ "yes", "no" ]
        -- , [ "enter the amount of rounds" ]
        ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just False
    , Telegram.replyKeyboardMarkupSelective = Just True
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
}

chooseFigureInlineKeyboard :: Telegram.ReplyKeyboardMarkup
chooseFigureInlineKeyboard = Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard =
        [ [ "🪨", "📄", "✂️" ]
        , [ "/end" ]
        ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just False
    , Telegram.replyKeyboardMarkupSelective = Just True
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
}

-- | Process incoming 'Telegram.Update's and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
    $   Welcome <$  command "start"
    <|> ExitGame <$ (command "end" <|> command "exit")
    <|> RunRound <$ (command "play")
    <|> Help <$ (command "help")
    <|> Input <$> text

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of
    NoAction -> pure model
    Welcome -> model { state = Start, computerScore = 0, userScore = 0 } <# do
      replyText GL.introduction
      pure DoYouWantToPlay
    DoYouWantToPlay -> model { state = Wait } <# do
      reply (toReplyMessage "Do you want to play?") {
        replyMessageReplyMarkup = Just $ Telegram.SomeReplyKeyboardMarkup doYouWantToPlayInlineKeyboard
      }
      pure NoAction
    ExitGame -> model { state = Stopped } <# do
      reply (toReplyMessage GL.exitGame) {
        replyMessageReplyMarkup = Just $ Telegram.SomeReplyKeyboardMarkup startInlineKeyboard
      }
      pure NoAction
    Help -> model { state = Start } <# do
      reply (toReplyMessage GL.listOfCommands) {
        replyMessageReplyMarkup = Just $ Telegram.SomeReplyKeyboardMarkup playInlineKeyboard
      }
      pure NoAction
    RunRound -> model { state = Play } <# do
      reply (toReplyMessage "What's your choice?") {
        replyMessageReplyMarkup = Just $ Telegram.SomeReplyKeyboardMarkup chooseFigureInlineKeyboard
      }
      pure NoAction
    Input msg -> do
      case (state model) of
        Start -> model <# do
          pure Welcome
        Stopped -> model <# do
          pure ExitGame
        Wait -> model <# do
          case (msg) of
            "yes" -> pure RunRound
            "no" -> pure ExitGame
            "enter the amount of rounds" -> do
              replyText "This mode is not implemented yet"
              pure DoYouWantToPlay
            _ -> do
              replyText "I don't understand what you want:("
              pure DoYouWantToPlay
        Play ->
          if (msg == "/end") then
            model <# do
            pure ExitGame
          else
            updateModel model GL.Computer <# do
            computerInput <- getComputerInput
            userInput <- getUserInput msg
            if (userInput == Nothing) then
              do
              replyText "I don't understand what you want:("
              pure RunRound
            else
              let winner = GL.findWinner (removeMaybe userInput) computerInput in
                do
--                let model = updateModel model winner
                replyText $ GL.printComputerInput computerInput
                replyText $ GL.getWinnerInfo winner -- <> "\n\n" <> GL.printScore (computerScore model) (userScore model)
                pure RunRound
              where
                removeMaybe :: Maybe a -> a
                removeMaybe (Just x) = x
                removeMaybe _ = error ("`removeMaybe` got Nothing")

updateScore :: Int -> GL.Winner -> GL.Winner -> Int
updateScore prevScore player winner
    | player == winner = prevScore + 1
    | otherwise        = prevScore

updateModel :: Model -> GL.Winner -> Model
updateModel model winner =
    model { computerScore = updateScore (computerScore model) GL.Computer winner
          , userScore = updateScore (userScore model ) GL.User winner
          }

getUserInput ::  Text -> BotM (Maybe GL.Type)
getUserInput userChoice
    | userChoice == "🪨" = return $ Just GL.Rock
    | userChoice == "📄" = return $ Just GL.Paper
    | userChoice == "✂️" = return $ Just GL.Scissors
    | otherwise = return Nothing

getComputerInput :: BotM GL.Type
getComputerInput = do
    res <- randomIO :: BotM Int
    case res `mod` 3 of
        0 -> return GL.Rock
        1 -> return GL.Paper
        2 -> return GL.Scissors


-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
    env <- Telegram.defaultTelegramClientEnv token
    startBot_ (traceBotDefault (conversationBot Telegram.updateChatId bot)) env
