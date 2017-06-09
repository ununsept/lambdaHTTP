module Configuration
    ( Config (..)
    , Mime
    , defaultConfig
    ) where

import System.IO                ( FilePath )

type Mime = String

data Config = Config
    { port          :: Int
    , maxListen     :: Int
    , saveLog       :: Bool
    , fileLog       :: FilePath
    , status404     :: FilePath
    , rootDirectory :: FilePath
    , indexFile     :: FilePath
    , blackList     :: [Mime]
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
    , blackList     = []
    }
