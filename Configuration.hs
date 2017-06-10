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
    , indexFile     :: FilePath
    , status404     :: FilePath
    , blackList     :: [Mime]
    , domain        :: [(String,FilePath)]
    }

defaultConfig :: Config
defaultConfig = Config
    { port          = 80
    , maxListen     = 2
    , saveLog       = True
    , fileLog       = "asdf"
    , status404     = "error.html"
    , indexFile     = "/index.html"
    , blackList     = []
    , domain        = [("127.0.0.1", ".")]
    }
