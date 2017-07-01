module Engine
    ( startServer
    ) where

import           Control.Concurrent            (forkIO)
import           Control.Monad                 (forever)
import           Data.List                     (elemIndex, findIndex, concat)
import           Data.List.Split               (splitOneOf)
import           Data.Time.Clock               (getCurrentTime, utctDay)
import           Data.Time.Calendar            (toGregorian)
import           Data.Text.Unsafe              (inlinePerformIO)
import           Network.Socket
import           Network.Socket.ByteString     (sendAll)
import           System.Directory              (doesFileExist)
import           System.FilePath.Windows       (takeExtension)
import           System.IO                     (FilePath, appendFile)
import           System.Time                   (getClockTime)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B (ByteString)

import           Configuration

data StatusCodes =
      OK
    | NotFound
    deriving (Show)

data Fields = Fields
    { hostField :: Maybe String
    } deriving (Show)

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


 -- Sun, 06 Nov 1994 08:49:37 GMT
date :: String
date = undefined
    where
          dateLocal :: IO (Integer,Int,Int)
          dateLocal = getCurrentTime >>= return . toGregorian . utctDay

getStatus :: StatusCodes -> String
getStatus status = case status of
    OK       -> "HTTP/1.1 200 OK\r\n"
    NotFound -> "HTTP/1.1 404 Not Found\r\n"

headers :: Config -> String
headers conf = concat
    [ contentLanguageHeder
    , serverHeder
    ]
    where
          hederConf :: Header
          hederConf = header conf
          contentLanguageHeder :: String
          contentLanguageHeder = case contentLanguage hederConf of
              Just a  -> "Content-Language: " ++ a ++  "\r\n"
              Nothing -> ""
          serverHeder :: String
          serverHeder = case contentLanguage hederConf of
              Just a  -> "Host: " ++ a ++  "\r\n"
              Nothing -> ""

requestHeader :: Config -> StatusCodes -> FilePath -> String
requestHeader conf status path =
    (getStatus status) ++ (localHeader)
    where
          localHeader :: String
          localHeader = (headers conf) ++
            "Content-Length: "  ++ (show $ lengthFileUnsafe path) ++ "\r\n"
            ++ "Content-Type: " ++ (getMimeType path) ++ "\r\n\r\n"

parsFields :: [String] -> Fields
parsFields = foundField Fields { hostField = Nothing
                               }
    where
          foundField :: Fields -> [String] -> Fields
          foundField f []              = f
          foundField f ("Host:":x:xs) = foundField (f {hostField = Just x}) xs
          foundField f (_:x)          = foundField f x

requestGet :: Config -> [String] -> (String, FilePath)
requestGet conf (path:field) = case searchHost of
    Just a  -> requestMake conf a path
    Nothing -> (requestHeader conf NotFound (status404 conf), status404 conf)
    where
          searchHost :: Maybe Int
          searchHost = case (hostField $ parsFields field) of
              Just a   -> elemIndex True $ map ((a ==) . fst) (domain conf)
              Nothing  -> Nothing
          status404File :: FilePath
          status404File = unknownDomain conf ++ status404 conf

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
    "GET"     -> requestGet conf x
    "POST"    -> requestGet conf x
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
    case fileLog conf of
        Just file -> appendFile file $
            (show addr) ++ " = " ++ (show time) ++ "\n" ++ request
        Nothing -> putStrLn $
            (show addr) ++ " = " ++ (show time) ++ "\n" ++ request

requestSend :: Socket -> (String, FilePath) -> IO ()
requestSend sock (header, fileName) = do
    print header
    send sock header
    asdf <- B.readFile fileName
    sendAll sock asdf
    close sock

conect :: Config -> Socket -> SockAddr -> IO ()
conect conf sock addr = do
    request         <- recv sock 100000
    requestSend sock $ processHTTP conf request
    loger conf addr request

startServer :: Config -> IO ()
startServer conf = do
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet (fromIntegral (port conf)) iNADDR_ANY)
    listen sock (maxListen conf)
    forever $ do
        (sock, addr) <- accept sock
        forkIO $ conect conf sock addr
