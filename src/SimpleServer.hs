{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module SimpleServer where

import Control.Monad (when)

import qualified Config.Dyre as Dyre
import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp
import System.Console.CmdArgs
import System.Console.CmdArgs.Verbosity (isLoud)
import Control.Monad.IO.Class (liftIO)

realMain :: Handlers -> IO ()
realMain handlers = do
  settings <- cmdArgs simpleServerCmdArgs
  loud <- isLoud
  let p = port settings
  putStrLn $ "SimpleServer running on port " ++ show p
  run p $ waiApp $ application settings loud handlers


---------------------
-- WAI Application --
---------------------

application :: SimpleServerConfig -> Bool -> Handlers -> RouteM ()
application settings loud handlers = do
  when loud $ middleware logStdoutDev
  handlers
  catchall $ staticApp $ defaultFileServerSettings $ static settings


----------------------------
-- Command line arguments --
----------------------------

data SimpleServerConfig = SimpleServerConfig
  { port :: Int
  , static :: String
  }
  deriving (Data, Typeable)

simpleServerCmdArgs :: SimpleServerConfig
simpleServerCmdArgs = SimpleServerConfig
    { port = 8000
        &= help "Port on which the server runs (default 8000)"
        &= opt (8000::Int)
    , static = "."
        &= help "Folder with the static files (default (\".\"))"
        &= opt ("."::String)
    }
    &= verbosity
    &= program "simpleserver"
    &= summary "SimpleServer v0.1.1"


-----------------
-- Dyre Config --
-----------------

type Handlers = RouteM ()

-- TODO
confError :: Handlers -> String -> Handlers
confError handlers err = handler $ runHandlerM $ liftIO $ print $ "Error:" ++ err

simpleServer :: Handlers -> IO ()
simpleServer = Dyre.wrapMain Dyre.defaultParams
    { Dyre.projectName  = "simpleServer"
    , Dyre.showError    = confError
    , Dyre.realMain     = realMain
    }
