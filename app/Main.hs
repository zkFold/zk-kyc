module Main (main) where

import Network.Wai
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, setBeforeMainLoop, setPort, setTimeout)
import Options.Applicative
import Servant
import Server (API, server)
import System.IO (hPutStrLn, stderr)
import Prelude

userAPI :: Proxy API
userAPI = Proxy

app :: Application
app = serve userAPI server

data Config = Config
  { port :: Port
  , timeout :: Int
  }

paramsParser :: Parser Config
paramsParser = Config <$> port <*> timeout
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

opts :: ParserInfo Config
opts = info (paramsParser <**> helper) (progDesc "Run KYC server")

run :: Config -> IO ()
run Config {..} = do
  runSettings settings app
 where
  settings =
    setPort port $
      setTimeout timeout $
        setBeforeMainLoop (hPutStrLn stderr ("Listening on port " <> show port <> ", timeout " <> show timeout)) $
          defaultSettings

main :: IO ()
main = do
  config <- execParser opts
  run config
