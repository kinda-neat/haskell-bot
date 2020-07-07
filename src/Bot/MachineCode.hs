module Bot.MachineCode where

import Control.Monad.Reader

import Bot.Core (Env(..))
import Bot.Core.Telegram.Telegram (RuntimeTP(..))
import Bot.Domain (app)
import Bot.Infrastructure.Telegram.Telegram (unTelegramM)
import ReadConfig (Config)

data Bot
  = VK { vkKey :: String
       , vkServer :: String
       , vkTs :: Int }
  | Telegram

runBot :: Config -> Bot -> IO ()
runBot config bot =
  case bot of
    Telegram -> runReaderT (unTelegramM (app $ RuntimeTP Nothing)) env
    -- VK -> runReaderT (unVKM app) env
  where
    env = (Env {envConfig = config})
