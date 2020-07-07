module Bot.Core where

import ReadConfig (Config)

data Env = Env
  { envConfig :: Config
  }

data Command h r me s
  = Help h
  | Repeat r
  | Message me
  | SelectOption s
