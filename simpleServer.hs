{- SAMPLE SIMPLESERVER CONFIG -}
import SimpleServer
import Data.IORef

-- Sample handlers
handlers :: IORef Int -> RouteM ()
handlers timesref = do
  -- Sample handler to print the request number to the console
  handler $ runHandlerM $ do
    liftIO $ do
      times <- readIORef timesref
      putStrLn $ "Request number " ++ show times
      writeIORef timesref (times + 1)
    -- Omit the call to next if you don't want
    --  the standard processing to continue
    next
  -- Feel free to add more handlers, routes, or whatever

main :: IO ()
main = do
  times <- newIORef 0
  simpleServer $ handlers times
