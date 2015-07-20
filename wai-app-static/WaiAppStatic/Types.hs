{-# LANGUAGE OverloadedStrings #-}
module WaiAppStatic.Types
    ( -- * Pieces
      Piece
    , toPiece
    , fromPiece
    , unsafeToPiece
    , Pieces
    , toPieces
      -- * Caching
    , MaxAge (..)
      -- * File\/folder serving
    , FolderName
    , Folder (..)
    , File (..)
    , LookupResult (..)
    , Listing
      -- * Settings
    , StaticSettings (..)
    ) where

import Data.Text (Text)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import Data.ByteString (ByteString)
import System.Posix.Types (EpochTime)
import qualified Data.Text as T
import Blaze.ByteString.Builder (Builder)
import Network.Mime (MimeType)

-- | An individual component of a path, or of a filepath.
--
-- This is the core type used by wai-app-static for doing lookups. It provides
-- a smart constructor to avoid the possibility of constructing unsafe path
-- segments (though @unsafeToPiece@ can get around that as necessary).
--
-- Individual file lookup backends must know how to convert from a @Piece@ to
-- their storage system.
newtype Piece = Piece { fromPiece :: Text }
    deriving (Show, Eq, Ord)

-- | Smart constructor for a @Piece@. Won\'t allow unsafe components, such as
-- pieces beginning with a period or containing a slash. This /will/, however,
-- allow null pieces.
toPiece :: Text -> Maybe Piece
toPiece t
    | T.null t = Just $ Piece t
    | T.head t == '.' = Nothing
    | T.any (== '/') t = Nothing
    | otherwise = Just $ Piece t

-- | Construct a @Piece@ without input validation.
unsafeToPiece :: Text -> Piece
unsafeToPiece = Piece

-- | Call @toPiece@ on a list.
--
-- > toPieces = mapM toPiece
toPieces :: [Text] -> Maybe Pieces
toPieces = mapM toPiece

-- | Request coming from a user. Corresponds to @pathInfo@.
type Pieces = [Piece]

-- | Values for the max-age component of the cache-control response header.
data MaxAge = NoMaxAge -- ^ no cache-control set
            | MaxAgeSeconds Int -- ^ set to the given number of seconds
            | MaxAgeForever -- ^ essentially infinite caching; in reality, probably one year

-- | Just the name of a folder.
type FolderName = Piece

-- | Represent contents of a single folder, which can be itself either a file
-- or a folder.
data Folder = Folder
    { folderContents :: [Either FolderName File]
    }

-- | Information on an individual file.
data File = File
    { -- | Size of file in bytes
      fileGetSize :: Int
      -- | How to construct a WAI response for this file. Some files are stored
      -- on the filesystem and can use @ResponseFile@, while others are stored
      -- in memory and should use @ResponseBuilder@.
    , fileToResponse :: H.Status -> H.ResponseHeaders -> W.Response
      -- | Last component of the filename.
    , fileName :: Piece
      -- | Calculate a hash of the contents of this file, such as for etag.
    , fileGetHash :: IO (Maybe ByteString)
      -- | Last modified time, used for both display in listings and if-modified-since.
    , fileGetModified :: Maybe EpochTime
    }

-- | Result of looking up a file in some storage backend.
--
-- The lookup is either a file or folder, or does not exist.
data LookupResult = LRFile File
                  | LRFolder Folder
                  | LRNotFound

-- | How to construct a directory listing page for the given request path and
-- the resulting folder.
type Listing = Pieces -> Folder -> IO Builder

-- | All of the settings available to users for tweaking wai-app-static.
--
-- Note that you should use the settings type approach for modifying values.
-- See <http://www.yesodweb.com/book/settings-types> for more information.
data StaticSettings = StaticSettings
    {
      -- | Lookup a single file or folder. This is how you can control storage
      -- backend (filesystem, embedded, etc) and where to lookup.
      ssLookupFile :: Pieces -> IO LookupResult

      -- | Determine the mime type of the given file. Note that this function
      -- lives in @IO@ in case you want to perform more complicated mimetype
      -- analysis, such as via the @file@ utility.
    , ssGetMimeType :: File -> IO MimeType

      -- | Ordered list of filenames to be used for indices. If the user
      -- requests a folder, and a file with the given name is found in that
      -- folder, that file is served. This supercedes any directory listing.
    , ssIndices :: [Piece]

      -- | How to perform a directory listing. Optional. Will be used when the
      -- user requested a folder.
    , ssListing :: Maybe Listing

      -- | Value to provide for max age in the cache-control.
    , ssMaxAge :: MaxAge

      -- | Given a requested path and a new destination, construct a string
      -- that will go there. Default implementation will use relative paths.
    , ssMkRedirect :: Pieces -> ByteString -> ByteString

      -- | If @True@, send a redirect to the user when a folder is requested
      -- and an index page should be displayed. When @False@, display the
      -- content immediately.
    , ssRedirectToIndex :: Bool

      -- | Prefer usage of etag caching to last-modified caching.
    , ssUseHash :: Bool

      -- | Force a trailing slash at the end of directories
    , ssAddTrailingSlash :: Bool
    }
