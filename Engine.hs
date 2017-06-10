module Engine
    ( startServer
    ) where

import           System.FilePath.Windows       (takeExtension)
import           Data.List                     (elemIndex, findIndex)
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
import           Network.Socket.ByteString     (sendAll)
import           Configuration

data StatusCodes =
    OK
  | NotFound
  deriving (Show)

getMimeType :: FilePath -> Mime
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

lengthFileUnsafe :: FilePath -> Int
lengthFileUnsafe file = (B.length $ inlinePerformIO $ B.readFile file)

getStatus :: StatusCodes -> String
getStatus status = case status of
    OK       -> "HTTP/1.1 200 OK\r\n"
    NotFound -> "HTTP/1.1 404 Not Found\r\n"

requestHeader :: Config -> StatusCodes -> FilePath -> String
requestHeader conf status path =
    (getStatus status) ++ (contentHeader)
    where
          contentHeader :: String
          contentHeader =
            "Content-Length: "  ++ (show $ lengthFileUnsafe path) ++ "\r\n"
            ++ "Content-Type: " ++ (getMimeType path) ++ "\r\n\r\n"

requestGet :: Config -> [String] -> (String, FilePath)
requestGet conf (path:_:_:host:_) = case searchHost of
    Just a  -> requestMake conf a path
    Nothing -> (requestHeader conf NotFound (status404 conf), status404 conf)
    where
          searchHost :: Maybe Int
          searchHost = elemIndex True $ map ((host ==) . fst) (domain conf)

requestMake :: Config -> Int -> FilePath -> (String, FilePath)
requestMake  conf hostNum path =
    if (inlinePerformIO $ doesFileExist pathFile) && blackListVerification
        then (requestHeader conf OK       pathFile        , pathFile)
        else (requestHeader conf NotFound (status404 conf), status404 conf)
    where
          pathFile :: FilePath
          pathFile = if head (splitOneOf "?" path) == "/"
              then (snd domainConf) ++ (indexFile conf)
              else (snd domainConf) ++ head (splitOneOf "?" path)
          blackListVerification :: Bool
          blackListVerification =
              filter (getMimeType pathFile ==) (blackList conf) == []
          domainConf :: (String,FilePath)
          domainConf = (domain conf) !! hostNum
          status404File :: FilePath
          status404File = status404 conf

parsHTTP :: Config -> [String] -> (String, FilePath)
parsHTTP conf (verb:x) = case verb of
    "GET"     -> requestGet conf (take 4 x)
    "POST"    -> requestGet conf (take 4 x)
    --otherwise -> expression

processHTTP :: Config -> String -> (String, FilePath)
processHTTP conf query = parsHTTP conf $
          removeEmpytElement(splitOneOf " \n\r" query)
    where
          removeEmpytElement :: [String] -> [String]
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

requestSend :: Socket -> (String, FilePath) -> IO ()
requestSend sock (header, fileName) = do
    print header
    send sock header
    asdf <- B.readFile fileName
    sendAll sock asdf
    close sock

startServer :: Config -> IO ()
startServer conf = do
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet (fromIntegral (port conf)) iNADDR_ANY)
    listen sock (maxListen conf)
    forever $ do
        (sock, addr) <- accept sock
        request      <- recv sock 100000
        requestSend sock $ processHTTP conf request
        loger conf addr request
