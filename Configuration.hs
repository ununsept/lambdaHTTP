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
    , date            :: Bool
    }

data Config = Config
    { port          :: Int
    , maxListen     :: Int
    , fileLog       :: Maybe FilePath
    , indexFile     :: FilePath
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
    , status404     = "error.html"
    , indexFile     = "index.html"
    , unknownDomain = "./"
    , header        = Header
        { contentLanguage   = Nothing
        , server            = Nothing
        , date              = False
        }
    , blackList     = []
    , domain        = [ ("127.0.0.1", "./")
                      ]
    }
