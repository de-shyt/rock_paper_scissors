{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI where

import qualified GameLogic as GL

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
    { state :: State }
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
    { botInitialModel = Model { state = Start }
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
        , [ "enter the amount of rounds" ]
        ]
    , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
    , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just False
    , Telegram.replyKeyboardMarkupSelective = Just True
    , Telegram.replyKeyboardMarkupInputFieldSelector = Nothing
}

chooseFigureInlineKeyboard :: Telegram.ReplyKeyboardMarkup
chooseFigureInlineKeyboard = Telegram.ReplyKeyboardMarkup
    { Telegram.replyKeyboardMarkupKeyboard =
        [ [ "Rock", "Paper", "Scissors" ]   --[[ "🪨", "📄", "✂️" ]]
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
        Welcome -> model { state = Start } <# do
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
        Input msg -> model <# do
            case (state model) of
                Start -> pure Welcome
                Stopped -> pure ExitGame
                Wait -> case (msg) of
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
                        pure ExitGame
                    else do
                        computerInput <- GL.getComputerInput
                        let userInput = GL.getUserInput msg in
                            if (userInput == Nothing) then do
                                replyText "I don't understand what you want:("
                                pure RunRound
                            else do
                                let winner = GL.findWinner (removeMaybe userInput) computerInput in do
                                    replyText $ GL.getComputerInputInfo computerInput
                                    replyText $ GL.winnerInfoOutput winner
                                    pure RunRound
                                where
                                    removeMaybe :: Maybe a -> a
                                    removeMaybe (Just x) = x
                                    removeMaybe _ = error ("`removeMaybe` got Nothing")




-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
    env <- Telegram.defaultTelegramClientEnv token
    startBot_ (traceBotDefault bot) env
