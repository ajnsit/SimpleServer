SimpleServer (SimpleServer 0.1)
===============================

This package provides a simple static file server.

Features:
  - Provides folder index listings
  - Serves static files from the current folder

Usage
======

First install the executable -

For `cabal` users -

    $ cabal install SimpleServer

Or if you prefer to use `stack` -

    $ stack install SimpleServer

Run with `--help` for usage information -

    $ simpleserver --help
    SimpleServer v0.1
    
    simpleserver [OPTIONS]
    
    Common flags:
      -p --port[=INT]       Port on which the server runs
      -? --help             Display help message
      -V --version          Print version information
         --numeric-version  Print just the version number

Basically run from any directory that you wish to serve files from.

Options
=======

To serve files at the default port (8000), run the executable without any arguments -

    $ simpleserver
    Running on port 8000

To serve files at any other port, specify the port as a parameter -

    $ simpleserver -p=8888
    Running on port 8888


Changelog
=========

0.1 : Intial release

