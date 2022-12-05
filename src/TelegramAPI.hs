{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI where

import qualified GameIO

import Data.Text (Text)
import           Control.Applicative              ((<|>))

import Control.Monad.Trans (liftIO)  -- to transform BotM() to IO()

import qualified Telegram.Bot.API as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser

data Model = Model
    { lastMessage :: Text }
    deriving (Show)


data Action
    = NoAction
    | Welcome
    | ExitGame
    | Help
    | RunRound
    | Input Text
--    | WriteLastMessage
    deriving (Show)

bot :: BotApp Model Action
bot = BotApp
    { botInitialModel = Model { lastMessage = "" }
    , botAction = flip handleUpdate
    , botHandler = handleAction
    , botJobs = []  -- a sequence jobs that a bot needs to do
    }

-- | Process incoming 'Telegram.Update's and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
    $   Welcome <$  command "start"
    <|> ExitGame <$ (command "end" <|> command "exit")
    <|> RunRound <$ (command "play")
    <|> Help <$ (command "help")
--    <|> WriteLastMessage <$ (command "write")
    <|> Input <$> text

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
    case action of
        NoAction -> pure model
        Welcome -> model <# do
            replyText GameIO.introduction
            pure NoAction
        ExitGame -> model <# do
            replyText GameIO.exitGame
            pure NoAction
        Help -> model <# do
            replyText GameIO.listOfCommands
            pure NoAction
        RunRound -> model <# do
            replyText "You're playing"
--            liftIO (runRound model)
            pure NoAction
--        WriteLastMessage -> model <# do
--            replyText (lastMessage model)
        Input msg -> model {lastMessage = msg} <# do
          replyText "Got it"
          pure NoAction

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
    env <- Telegram.defaultTelegramClientEnv token
    startBot_ (traceBotDefault bot) env
