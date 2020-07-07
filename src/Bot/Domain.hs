{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Bot.Domain where

import Control.Monad.Identity
import Control.Monad.Reader

import Bot.Core

class (Monad m, MonadReader Env m) =>
      AwaitCommand m p h r me s
  | m -> p h r me s
  where
  awaitCommand :: p -> m (p, (Command h r me s))

class (Monad m, MonadReader Env m) =>
      EchoMessage m p
  | m -> p
  where
  echoMessage :: p -> m ()

class (Monad m, MonadReader Env m) =>
      ShowBotDescription m c
  | m -> c
  where
  showBotDescription :: c -> m ()

class (Monad m, MonadReader Env m) =>
      AskTimesToEchoMessage m p
  | m -> p
  where
  askTimesToEchoMessage :: p -> m ()

class (Monad m, MonadReader Env m) =>
      SaveNewTimesToEcho m s
  where
  saveNewTimesToEcho :: s -> m ()

class (Monad m, MonadReader Env m) =>
      ConfirmTimesToEchoSaved m p
  where
  confirmTimesToEchoSaved :: p -> m ()

app ::
     ( AwaitCommand m p h r me s
     , ShowBotDescription m h
     , EchoMessage m me
     , AskTimesToEchoMessage m r
     , SaveNewTimesToEcho m s
     , ConfirmTimesToEchoSaved m s
     )
  => p
  -> m ()
app botPayload = do
  (newBotPayload, command) <- awaitCommand botPayload
  case command of
    Message p -> echoMessage p
    Repeat p -> askTimesToEchoMessage p
    Help p -> showBotDescription p
    SelectOption p -> do
      saveNewTimesToEcho p
      confirmTimesToEchoSaved p
  app newBotPayload
