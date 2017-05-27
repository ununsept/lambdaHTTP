module Configuration
    ( Config (..)
    , defaultConfig
    ) where

import System.IO                ( FilePath )

data Config = Config
    { port          :: Int
    , maxListen     :: Int
    , saveLog       :: Bool
    , fileLog       :: FilePath
    , status404     :: FilePath
    , rootDirectory :: FilePath
    , indexFile     :: FilePath
    }

defaultConfig :: Config
defaultConfig = Config
    { port          = 80
    , maxListen     = 2
    , saveLog       = True
    , fileLog       = "asdf"
    , status404     = "error.html"
    , rootDirectory = "."
    , indexFile     = "/index.html"
    }
