module Main where

import SimpleServer

handlers :: Monad m => m ()
handlers = return ()

main :: IO ()
main = simpleServer handlers
