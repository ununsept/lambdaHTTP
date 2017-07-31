module Configuration
    ( Config (..)
    , Header (..)
    , Mime
    , defaultConfig
    ) where

import System.IO                ( FilePath )

type Mime = String

data Header = Header
    { contentLanguage :: Maybe String
    , server          :: Maybe String
    , pragma          :: Maybe String
    , date            :: Bool
    }

data Config = Config
    { port          :: Int
    , maxListen     :: Int
    , fileLog       :: Maybe FilePath
    , indexFile     :: FilePath
    , status400     :: FilePath
    , status404     :: FilePath
    , unknownDomain :: FilePath
    , header        :: Header
    , blackList     :: [Mime]
    , domain        :: [(String,FilePath)]
    }

defaultConfig :: Config
defaultConfig = Config
    { port          = 80
    , maxListen     = 2
    , fileLog       = Just "asdf"
    , status400     = "error.html"
    , status404     = "error.html"
    , indexFile     = "index.html"
    , unknownDomain = "./"
    , header        = Header
        { contentLanguage   = Nothing
        , server            = Nothing
        , pragma            = Nothing
        , date              = False
        }
    , blackList     = []
    , domain        = [ ("127.0.0.1", "./")
                      ]
    }
