SimpleServer (SimpleServer 0.1.1.1)
===================================

This package provides a simple file server which can be configured using haskell files (similar to xmonad).

Features:
  - Provides folder index listings
  - Serves static files from the specified folder
  - Fully configurable using haskell configuration files

Usage
======

First install the executable -

For `cabal` users -

    $ cabal install SimpleServer

Or if you prefer to use `stack` -

    $ stack install SimpleServer

Run with `--help` for usage information -

    $ simpleserver --help
    SimpleServer v0.1.1.1

    simpleserver [OPTIONS]

    Common flags:
      -p --port[=INT]       Port on which the server runs (default 8000)
      -s --static[=ITEM]    Folder with the static files (default ("."))
         --paths            Print the expected path to the simpleserver config
      -? --help             Display help message
      -V --version          Print version information
         --numeric-version  Print just the version number
      -v --verbose          Loud verbosity
      -q --quiet            Quiet verbosity


Basically run from any directory that you wish to serve files from.

Configuration
=============

You can install custom handlers on top of vanilla simple-server using a haskell configuration file.

For example, lets say you want to keep a count of the number of requests made and print them out to the console. Then you can add a config file that looks like this -

```haskell
-- You must import SimpleServer
import SimpleServer
-- IORefs can be used to persist data in volatile memory
import Data.IORef

-- The entry point into simpleServer
main = do
  -- Create a new IORef to keep track of the number of requests made so far
  timesRef <- newIORef 0
  -- Call simpleServer with custom handlers
  simpleServer $ do

    -- Our one and only handler, uses a special DSL syntax (executed with `runHandlerM`)
    handler $ runHandlerM $ do

      -- Fetch the current count
      times <- liftIO$ readIORef timesRef
      
      -- Print current count
      liftIO $ putStrLn $ "Request number " ++ show times
      
      -- Increment current count and save it back
      liftIO $ writeIORef timesRef (times + 1)

    -- Continue processing the request with simpleServer
    -- You can omit this call to abort processing
    next
```

SimpleServer uses wai-routes to immplement handlers. See the documentation for [wai-routes](https://github.com/ajnsit/wai-routes) to see more examples of how to write custom handlers.


Options
=======

## 1. Port

To serve files at the default port (8000), run the executable without any arguments -

    $ simpleserver
    Running on port 8000

To serve files at any other port, specify the port as a parameter -

    $ simpleserver -p8888
    Running on port 8888

## 2. Static folder

To serve static files from any folder other than the current folder, use the '-s' argument.

For example, to serve files from 'static' folder -

    $ simpleserver -sstatic
    Running on port 8000

## 3. Log level

To enable logging, use '-v' or '--verbose. To disable any log output altogether use '-q' or '--quiet'


    $ simpleserver
    Running on port 8000

    $ simpleserver --verbose
    Running on port 8000
    127.0.0.1 - - [03/Feb/2016:11:10:49 +0530] "GET / HTTP/1.1" 200 - "" "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:44.0) Gecko/20100101 Firefox/44.0"

    $ simpleserver --verbose
    

## 4. Paths

To see the path simpleserver expects the config file to be in, use '--paths'



Changelog
=========

* 0.1.1.1 : Only cabal metadata changes
* 0.1.1   : Added static and logging. Use wai-routes
* 0.1     : Intial release
