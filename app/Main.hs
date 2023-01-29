{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified TelegramAPI

import qualified Telegram.Bot.API as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= TelegramAPI.run

