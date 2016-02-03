{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Control.Monad (when)

import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp
import System.Console.CmdArgs

main :: IO ()
main = do
  settings <- cmdArgs $
    simpleServerCmdArgs
    &= summary "SimpleServer v0.1.1"
  let p = port settings
  putStrLn $ "SimpleServer running on port " ++ show p
  run p $ waiApp $ application settings

application :: SimpleServer -> RouteM ()
application settings = do
  when (loglevel settings > 0) $ middleware logStdoutDev
  catchall $ staticApp $ defaultFileServerSettings $ static settings

-- Command line arguments
data SimpleServer = SimpleServer
  { port :: Int
  , static :: String
  , loglevel :: Int
  }
  deriving (Data, Typeable)

simpleServerCmdArgs :: SimpleServer
simpleServerCmdArgs = SimpleServer
    { port = 8000
        &= help "Port on which the server runs (default 8000)"
        &= opt (8000::Int)
    , static = "."
        &= help "Folder with the static files (default (\".\"))"
        &= opt ("."::String)
    , loglevel = 0
        &= help "Logging level (default 0)"
        &= opt (0::Int)
    }
