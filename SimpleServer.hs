{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import System.Console.CmdArgs

main :: IO ()
main = do
  simpleServer <- cmdArgs $ SimpleServer
    { port = 8000 &= help "Port on which the server runs" &= opt (8000::Int) }
    &= summary "SimpleServer v0.1"
  let p = port simpleServer
  putStrLn $ "Running simpleserver on port " ++ (show p)
  run p $ staticApp $ defaultFileServerSettings "."


-- Command line arguments
data SimpleServer = SimpleServer
  { port :: Int
  }
  deriving (Data, Typeable)

