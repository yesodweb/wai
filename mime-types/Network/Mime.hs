{-# LANGUAGE OverloadedStrings #-}
module Network.Mime
    ( -- * Lookups
      mimeByExt
    , defaultMimeLookup
      -- * Defaults
    , defaultMimeType
    , defaultMimeMap
      -- * Utilities
    , fileNameExtensions
      -- * Types
    , FileName
    , MimeType
    , MimeMap
    , Extension
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.Map as Map

-- | Maps extensions to mime types.
type MimeMap = Map.Map Extension MimeType

-- | The filename component of a filepath, leaving off the directory but
-- keeping all extensions.
type FileName = Text

-- | Individual mime type for be served over the wire.
type MimeType = ByteString

-- | The default fallback mime type \"application/octet-stream\".
defaultMimeType :: MimeType
defaultMimeType = "application/octet-stream"

-- | A default mapping from filename extension to mime type.
--
-- taken from snap-core Snap.Util.FileServer
defaultMimeMap :: MimeMap
defaultMimeMap = Map.fromList [
  ( "apk"     , "application/vnd.android.package-archive" ),
  ( "asc"     , "text/plain"                        ),
  ( "asf"     , "video/x-ms-asf"                    ),
  ( "asx"     , "video/x-ms-asf"                    ),
  ( "avi"     , "video/x-msvideo"                   ),
  ( "bz2"     , "application/x-bzip"                ),
  ( "c"       , "text/plain"                        ),
  ( "class"   , "application/octet-stream"          ),
  ( "conf"    , "text/plain"                        ),
  ( "cpp"     , "text/plain"                        ),
  ( "css"     , "text/css"                          ),
  ( "cxx"     , "text/plain"                        ),
  ( "dtd"     , "text/xml"                          ),
  ( "dvi"     , "application/x-dvi"                 ),
  ( "epub"    , "application/epub+zip"              ),
  ( "gif"     , "image/gif"                         ),
  ( "gz"      , "application/x-gzip"                ),
  ( "hs"      , "text/plain"                        ),
  ( "htm"     , "text/html"                         ),
  ( "html"    , "text/html"                         ),
  ( "ico"     , "image/vnd.microsoft.icon"          ),
  ( "jar"     , "application/x-java-archive"        ),
  ( "jpeg"    , "image/jpeg"                        ),
  ( "jpe"    , "image/jpeg"                         ),
  ( "jpg"     , "image/jpeg"                        ),
  ( "js"      , "text/javascript"                   ),
  ( "json"    , "application/json"                  ),
  ( "log"     , "text/plain"                        ),
  ( "manifest", "text/cache-manifest"               ),
  ( "m3u"     , "audio/x-mpegurl"                   ),
  ( "mov"     , "video/quicktime"                   ),
  ( "mp3"     , "audio/mpeg"                        ),
  ( "mpeg"    , "video/mpeg"                        ),
  ( "mpg"     , "video/mpeg"                        ),
  ( "ogg"     , "application/ogg"                   ),
  ( "pac"     , "application/x-ns-proxy-autoconfig" ),
  ( "pdf"     , "application/pdf"                   ),
  ( "png"     , "image/png"                         ),
  ( "bmp"     , "image/bmp"                         ),
  ( "ps"      , "application/postscript"            ),
  ( "qt"      , "video/quicktime"                   ),
  ( "sig"     , "application/pgp-signature"         ),
  ( "spl"     , "application/futuresplash"          ),
  ( "svg"     , "image/svg+xml"                     ),
  ( "swf"     , "application/x-shockwave-flash"     ),
  ( "tar"     , "application/x-tar"                 ),
  ( "tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( "tar.gz"  , "application/x-tgz"                 ),
  ( "tbz"     , "application/x-bzip-compressed-tar" ),
  ( "text"    , "text/plain"                        ),
  ( "tgz"     , "application/x-tgz"                 ),
  ( "torrent" , "application/x-bittorrent"          ),
  ( "ttf"     , "application/x-font-truetype"       ),
  ( "txt"     , "text/plain"                        ),
  ( "wav"     , "audio/x-wav"                       ),
  ( "wax"     , "audio/x-ms-wax"                    ),
  ( "wma"     , "audio/x-ms-wma"                    ),
  ( "wmv"     , "video/x-ms-wmv"                    ),
  ( "woff"    , "application/font-woff"             ),
  ( "xbm"     , "image/x-xbitmap"                   ),
  ( "xhtml"   , "application/xhtml+xml"             ),
  ( "xml"     , "text/xml"                          ),
  ( "xpm"     , "image/x-xpixmap"                   ),
  ( "xwd"     , "image/x-xwindowdump"               ),
  ( "zip"     , "application/zip"                   )]

-- | Look up a mime type from the given mime map and default mime type.
mimeByExt :: MimeMap
          -> MimeType -- ^ default mime type
          -> FileName
          -> MimeType
mimeByExt mm def =
    go . fileNameExtensions
  where
    go [] = def
    go (e:es) =
        case Map.lookup e mm of
            Nothing -> go es
            Just mt -> mt

-- | @mimeByExt@ applied to @defaultMimeType@ and @defaultMimeMap@.
defaultMimeLookup :: FileName -> MimeType
defaultMimeLookup = mimeByExt defaultMimeMap defaultMimeType

-- | Get a list of all of the file name extensions from a piece.
--
-- > pieceExtensions "foo.tar.gz" == ["tar.gz", "gz"]
fileNameExtensions :: FileName -> [Extension]
fileNameExtensions =
    go
  where
    go t
        | T.null e = []
        | otherwise = e : go e
      where
        e = T.drop 1 $ T.dropWhile (/= '.') t

-- | Path extension. May include multiple components, e.g. tar.gz
type Extension = Text
