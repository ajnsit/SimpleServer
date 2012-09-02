{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Application.Static
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8080 $ staticApp $ defaultFileServerSettings "."

