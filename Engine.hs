module Engine
    ( startServer
    ) where

import           System.FilePath.Windows       (takeExtension)
import           Data.List.Split               (splitOneOf)
import           Data.Text.Unsafe              (inlinePerformIO)
import           System.Directory              (doesFileExist)
import           System.IO                     (FilePath, appendFile)
import           Network.Socket
import           Control.Concurrent            (forkIO)
import           Control.Monad                 (forever)
import           System.Time                   (getClockTime)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B (ByteString)
import qualified Data.ByteString.Char8    as Bp(pack)
import           Network.Socket.ByteString     (sendAll, send)
import           Configuration                 (Config (..))

data StatusCodes =
    OK
  | NotFound
  deriving (Show)

getMimeType :: FilePath -> String
getMimeType file = case (takeExtension file) of
    ".bmp"    -> "image/bmp"
    ".css"    -> "text/css"
    ".gif"    -> "image/gif"
    ".htm"    -> "text/html"
    ".html"   -> "text/html"
    ".jpg"    -> "image/jpeg"
    ".jpeg"   -> "image/jpeg"
    ".js"     -> "application/javascript"
    ".json"   -> "application/json"
    ".otf"    -> "font/opentype"
    ".png"    -> "image/png"
    ".text"   -> "text/plain"
    ".aac"    -> "audio/aac"
    ".abw"    -> "application/x-abiword"
    ".arc"    -> "application/octet-stream"
    ".avi"    -> "video/x-msvideo"
    ".azw"    -> "application/vnd.amazon.ebook"
    ".bz"     -> "application/x-bzip"
    ".bz2"    -> "application/x-bzip2"
    ".csh"    -> "application/x-csh"
    ".csv"    -> "text/csv"
    ".doc"    -> "application/msword"
    ".epub"   -> "application/epub+zip"
    ".ico"    -> "image/x-icon"
    ".ics"    -> "text/calendar"
    ".jar"    -> "application/java-archive"
    ".mid"    -> "audio/midi"
    ".midi"   -> "audio/midi"
    ".mpeg"   -> "video/mpeg"
    ".mpkg"   -> "application/vnd.apple.installer+xml"
    ".odp"    -> "application/vnd.oasis.opendocument.presentation"
    ".ods"    -> "application/vnd.oasis.opendocument.spreadsheet"
    ".odt"    -> "application/vnd.oasis.opendocument.text"
    ".oga"    -> "audio/ogg"
    ".ogv"    -> "video/ogg"
    ".ogx"    -> "application/ogg"
    ".pdf"    -> "application/pdf"
    ".ppt"    -> "application/vnd.ms-powerpoint"
    ".rar"    -> "application/x-rar-compressed"
    ".rtf"    -> "application/rtf"
    ".sh"     -> "application/x-sh"
    ".svg"    -> "image/svg+xml"
    ".swf"    -> "application/x-shockwave-flash"
    ".tar"    -> "application/x-tar"
    ".tif"    -> "image/tiff"
    ".tiff"   -> "image/tiff"
    ".ttf"    -> "font/ttf"
    ".vsd"    -> "application/vnd.visio"
    ".wav"    -> "audio/x-wav"
    ".weba"   -> "audio/webm"
    ".webm"   -> "video/webm"
    ".webp"   -> "image/webp"
    ".woff"   -> "font/woff"
    ".woff2"  -> "font/woff2"
    ".xhtml"  -> "application/xhtml+xml"
    ".xls"    -> "application/vnd.ms-excel"
    ".xml"    -> "application/xml"
    ".xul"    -> "application/vnd.mozilla.xul+xml"
    ".zip"    -> "application/zip"
    ".3gp"    -> "video/3gpp"
    ".3g2"    -> "video/3gpp2"
    ".7z"     -> "application/x-7z-compressed"
    otherwise -> "application/octet-stream"

readFileUnsafe :: FilePath -> String
readFileUnsafe fileNane = inlinePerformIO $ readFile fileNane

getStatus :: StatusCodes -> String
getStatus status = case status of
    OK       -> "HTTP/1.1 200 OK\n\r"
    NotFound -> "HTTP/1.1 404 Not Found\n\r"

requestHeader :: Config -> StatusCodes -> String
requestHeader conf status =
    (getStatus status)

request :: Config -> StatusCodes ->  FilePath -> IO B.ByteString
request conf status path = do
    file <- B.readFile path
    return $ B.append
          (Bp.pack((requestHeader conf status) ++ (contentHeader file))) file
    where contentHeader :: B.ByteString -> String
          contentHeader file =
            "Content-Length: " ++ (show $ B.length file) ++ "\n\r"
            ++ "Content-Type: "   ++ (getMimeType path) ++ "\n\r\n\r"

requestGet :: Config -> [String] -> IO B.ByteString
requestGet conf (path:_:_:host) =
    if (inlinePerformIO $ doesFileExist pathFile)
        then request conf OK        pathFile
        else request conf NotFound  (status404 conf)
    where pathFile :: String
          pathFile = if head (splitOneOf "?" path) == "/"
              then (rootDirectory conf) ++ (indexFile conf)
              else (rootDirectory conf) ++ head (splitOneOf "?" path)

parsHTTP :: Config -> [String] -> IO B.ByteString
parsHTTP conf (verb:x) = case verb of
    "GET"     -> requestGet conf (take 4 x)
    "POST"    -> requestGet conf (take 4 x)
    --otherwise -> expression

processHTTP :: Config -> String -> IO B.ByteString
processHTTP conf query = parsHTTP conf $
          removeEmpytElement(splitOneOf " \n\r" query)
    where removeEmpytElement :: [String] -> [String]
          removeEmpytElement []     = []
          removeEmpytElement (x:xs) = if x == ""
            then removeEmpytElement xs
            else x : removeEmpytElement xs

loger :: Config -> SockAddr -> String -> IO ()
loger conf addr request = do
    time <- getClockTime
    if (saveLog conf)
        then appendFile (fileLog conf) $
            (show addr) ++ " = " ++ (show time) ++ "\n" ++ request
        else putStrLn $
            (show addr) ++ " = " ++ (show time) ++ "\n" ++ request

connection :: (Socket, SockAddr) -> Config  -> IO ()
connection (sock, addr) conf = do
    request <- recv sock 100000
    loger conf addr request
    content <- (processHTTP conf request)
    print content
    sendAll sock content
    close sock


startServer :: Config -> IO ()
startServer conf = do
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet (fromIntegral (port conf)) iNADDR_ANY)
    listen sock (maxListen conf)
    forever $ do
        connect <- accept sock
        connection connect conf
