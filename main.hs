module Main where

import System.IO                ( FilePath, appendFile )
import Network.Socket           ( Socket     (..)
                                , SockAddr   (..)
                                , SocketType (..)
                                , Family     (..)
                                , send   , recv   , bind
                                , socket , listen , close
                                , accept , iNADDR_ANY
                                )
import Text.Printf              ( printf               )
import Control.Concurrent       ( forkIO               )
import System.Time              ( getClockTime         )

import Engine                   ( parsHTTP             )
import Configuration

loged :: Config -> SockAddr -> String -> IO ()
loged conf addr request = do
    time <- getClockTime
    if (saveLog conf)
        then appendFile (fileLog conf) $
            (show addr) ++ " = " ++ (show time) ++ "\n" ++ request
        else putStrLn $
            (show addr) ++ " = " ++ (show time) ++ "\n" ++ request

connection :: (Socket, SockAddr) -> Config  -> IO ()
connection (sock, addr) conf = do
    request <- recv sock 100000
    loged conf addr request
    --putStrLn (parsHTTP conf request)
    send sock (parsHTTP conf request)
    close sock

mainLoop :: Socket -> Config -> IO ()
mainLoop sock conf = do
    connect <- accept sock
    connection connect conf
    mainLoop sock conf

startServer :: Config -> IO ()
startServer conf = do
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet (fromIntegral (port conf)) iNADDR_ANY)
    listen sock (maxListen conf)
    mainLoop sock conf

main :: IO ()
main = startServer defaultConfig
