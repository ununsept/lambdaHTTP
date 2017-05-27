module Engine
    ( parsHTTP
    ) where

import System.FilePath.Windows  ( takeExtension   )
import Data.List.Split          ( splitOneOf      )
import Data.Text.Unsafe         ( inlinePerformIO )
import System.Directory         ( doesFileExist   )


import Configuration            ( Config (..)     )

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

data StatusCodes =
      OK
    | NotFound
    deriving (Show)

readFileUnsafe :: FilePath -> String
readFileUnsafe fileNane = inlinePerformIO $ readFile fileNane

getStatus :: StatusCodes -> String
getStatus status = case status of
    OK       -> "HTTP/1.1 200 OK\n\r"
    NotFound -> "HTTP/1.1 404 Not Found\n\r"

request :: Config -> StatusCodes ->  FilePath -> String
request conf status path =
       getStatus status
    ++ "Content-Length: " ++ (show $ length $ readFileUnsafe path) ++ "\n\r"
    ++ "Content-Type: "   ++ (getMimeType path)
    ++ "\n\r\n\r"         ++ (readFileUnsafe path)

requestGet :: Config -> [String] -> String
requestGet conf (path:_:_:host) =
    if (inlinePerformIO $ doesFileExist pathFile)
        then request conf OK        pathFile
        else request conf NotFound  (status404 conf)
    where pathFile :: String
          pathFile = if head (splitOneOf "?" path) == "/"
              then (rootDirectory conf) ++ (indexFile conf)
              else (rootDirectory conf) ++ head (splitOneOf "?" path)

processHTTP :: Config -> [String] -> String
processHTTP conf (verb:x) = case verb of
    "GET"     -> requestGet conf (take 4 x)
    "POST"    -> requestGet conf (take 4 x)
    --otherwise -> expression

parsHTTP :: Config -> String -> String
parsHTTP conf query = processHTTP conf $
          removeEmpytElement(splitOneOf " \n\r" query)
    where removeEmpytElement :: [String] -> [String]
          removeEmpytElement []     = []
          removeEmpytElement (x:xs) = if x == ""
            then removeEmpytElement xs
            else x : removeEmpytElement xs
