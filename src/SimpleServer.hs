{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module SimpleServer
( module Network.Wai.Middleware.Routes
, module Control.Monad.IO.Class
, simpleServer
)
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Paths as Dyre
import Network.Wai.Middleware.Routes
import Network.Wai.Handler.Warp
import System.Console.CmdArgs

realMain :: Handlers -> IO ()
realMain handlers = do
  settings <- cmdArgs simpleServerCmdArgs
  if paths settings
    then do
      (_,_,p,_,_) <- Dyre.getPaths simpleServerDyreParams
      putStrLn p
    else do
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
simpleServer = Dyre.wrapMain simpleServerDyreParams

simpleServerDyreParams :: Dyre.Params Handlers
simpleServerDyreParams = Dyre.defaultParams
    { Dyre.projectName  = "simpleServer"
    , Dyre.showError    = confError
    , Dyre.realMain     = realMain
    }
