{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Control.Monad (when)

import qualified Config.Dyre as Dyre
import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp
import System.Console.CmdArgs

main :: IO ()
main = do
  settings <- fetchCmdArgs
  let p = port settings
  putStrLn $ "SimpleServer running on port " ++ show p
  run p $ waiApp $ application settings


---------------------
-- WAI Application --
---------------------

application :: SimpleServerConfig -> RouteM ()
application settings = do
  when (loglevel settings > 0) $ middleware logStdoutDev
  catchall $ staticApp $ defaultFileServerSettings $ static settings


----------------------------
-- Command line arguments --
----------------------------

data SimpleServerConfig = SimpleServerConfig
  { port :: Int
  , static :: String
  , loglevel :: Int
  }
  deriving (Data, Typeable)

fetchCmdArgs :: IO SimpleServerConfig
fetchCmdArgs = cmdArgs $
    simpleServerCmdArgs
    &= program "simpleserver"
    &= summary "SimpleServer v0.1.1"

simpleServerCmdArgs :: SimpleServerConfig
simpleServerCmdArgs = SimpleServerConfig
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


-----------------
-- Dyre Config --
-----------------

confError :: String -> String -> String
confError cfgMessage error = "Error:" ++ error ++ "\n" ++ cfgMessage

realMain message = do
    putStrLn "Entered Main Function"
    putStrLn message

dyreExample = Dyre.wrapMain Dyre.defaultParams
    { Dyre.projectName  = "dyreExample"
    , Dyre.showError    = confError
    , Dyre.realMain     = realMain
    }
