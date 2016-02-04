{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module SimpleServer
( module Network.Wai.Middleware.Routes
, module Control.Monad.IO.Class
, simpleServer
)
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Config.Dyre
import Config.Dyre.Paths
import System.Console.CmdArgs
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Routes


---------------------
-- WAI Application --
---------------------

runHandlers :: Handlers -> IO ()
runHandlers handlers = do
  settings <- cmdArgs simpleServerCmdArgs
  if paths settings
    then do
      (_,_,p,_,_) <- getPaths simpleServerDyreParams
      putStrLn p
    else do
      v <- getVerbosity
      let p = port settings
      logVerbose v $ "SimpleServer running on port " ++ show p
      run p $ waiApp $ application settings v handlers

application :: SimpleServerConfig -> Verbosity -> Handlers -> RouteM ()
application settings v handlers = do
  when (v == Loud) $ middleware logStdoutDev
  handlers
  catchall $ staticApp $ defaultFileServerSettings $ static settings

logVerbose :: Verbosity -> String -> IO ()
logVerbose Quiet _ = return ()
logVerbose _ s = putStrLn s


----------------------------
-- Command line arguments --
----------------------------

data SimpleServerConfig = SimpleServerConfig
  { port :: Int
  , static :: String
  , paths :: Bool
  }
  deriving (Data, Typeable)

simpleServerCmdArgs :: SimpleServerConfig
simpleServerCmdArgs = SimpleServerConfig
    { port = 8000
        &= help "Port on which the server runs (default 8000)"
        &= opt (8000::Int)
        &= name "p"
    , static = "."
        &= help "Folder with the static files (default (\".\"))"
        &= opt ("."::String)
    , paths = False
        &= help "Print the expected path to the simpleserver config"
    }
    &= verbosity
    &= program "simpleserver"
    &= summary "SimpleServer v0.1.1"


-----------------
-- Dyre Config --
-----------------

type Handlers = RouteM ()

-- TODO: This only prints errors on incoming requests
confError :: Handlers -> String -> Handlers
confError _ err = handler $ runHandlerM $ liftIO $ putStrLn $ "Error:" ++ err

simpleServer :: Handlers -> IO ()
simpleServer = wrapMain simpleServerDyreParams

simpleServerDyreParams :: Params Handlers
simpleServerDyreParams = defaultParams
    { projectName  = "simpleServer"
    , showError    = confError
    , realMain     = runHandlers
    }
