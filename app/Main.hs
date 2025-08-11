module Main where

import Data.Semigroup ((<>))
import Network.Wai
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, setBeforeMainLoop, setPort, setTimeout)
import Options.Applicative
import Prelude.Compat (IO, ($))
import Servant
import System.IO (hPutStrLn, stderr)
import Prelude (Int, show)

import Server (API, server)

userAPI :: Proxy API
userAPI = Proxy

app :: Application
app = serve userAPI server

data Params = Params
  { port :: Int
  , timeout :: Int
  }

paramsParser :: Parser Params
paramsParser = Params <$> port <*> timeout
 where
  port =
    option
      auto
      ( long "port"
          <> short 'p'
          <> help "Port to run the server"
          <> showDefault
          <> value 3000
          <> metavar "INT"
      )
  timeout =
    option
      auto
      ( long "timeout"
          <> short 't'
          <> help "Server response timeout"
          <> showDefault
          <> value 300
          <> metavar "INT"
      )

opts :: ParserInfo Params
opts = info (paramsParser <**> helper) (progDesc "Run KYC server")

run :: Port -> Int -> IO ()
run port timeout = do
  runSettings settings app
 where
  settings =
    setPort port $
      setTimeout timeout $
        setBeforeMainLoop (hPutStrLn stderr ("Listening on port " <> show port <> ", timeout " <> show timeout)) $
          defaultSettings

main :: IO ()
main = do
  Params port timeout <- execParser opts
  run port timeout
