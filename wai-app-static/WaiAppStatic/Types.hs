{-# LANGUAGE OverloadedStrings #-}
module WaiAppStatic.Types
    ( Pieces
    , toPiece
    , unsafeToPiece
    , toPieces
    , fromPiece
    , pieceExtensions
    , MaxAge (..)
    , Folder (..)
    , File (..)
    , Piece
    , emptyParentFolder
    , MimeType
    , Extension
    , MimeMap
    , LookupResult (..)
    , StaticSettings (..)
    , Listing
    ) where

import Data.Text (Text)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import Data.ByteString (ByteString)
import System.Posix.Types (EpochTime)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as L

-- | An individual component of a path, or of a filepath.
newtype Piece = Piece { fromPiece :: Text }
    deriving (Show, Eq, Ord)

-- | Smart constructor for a @Piece@. Won\'t allow unsafe components.
toPiece :: Text -> Maybe Piece
toPiece t
    | T.null t = Just $ Piece t
    | T.head t == '.' = Nothing
    | T.any (== '/') t = Nothing
    | otherwise = Just $ Piece t

unsafeToPiece :: Text -> Piece
unsafeToPiece = Piece

toPieces :: [Text] -> Maybe Pieces
toPieces = mapM toPiece

-- | Request coming from a user. Corresponds to @pathInfo@.
type Pieces = [Piece]

pieceExtensions :: Piece -> [Extension]
pieceExtensions =
    go . fromPiece
  where
    go t
        | T.null e = []
        | otherwise = e : go e
      where
        e = T.drop 1 $ T.dropWhile (/= '.') t

-- | Values for the max-age component of the cache-control response header.
data MaxAge = NoMaxAge -- ^ no cache-control set
            | MaxAgeSeconds Int -- ^ set to the given number of seconds
            | MaxAgeForever -- ^ essentially infinite caching; in reality, probably one year

data Folder = Folder -- FIXME revisit this
    { folderName :: Piece
    , folderContents :: [Either Folder File]
    }

data File = File
    { fileGetSize :: Int
    , fileToResponse :: H.Status -> H.ResponseHeaders -> W.Response
    , fileName :: Piece
    , fileGetHash :: IO (Maybe ByteString)
    , fileGetModified :: Maybe EpochTime
    }

emptyParentFolder :: Folder
emptyParentFolder = Folder (Piece "") []

type MimeType = ByteString
type Extension = Text
type MimeMap = Map.Map Extension MimeType

data LookupResult = LRFile File
                  | LRFolder Folder
                  | LRNotFound

type Listing = Pieces -> Folder -> IO L.ByteString

data StaticSettings = StaticSettings
    { -- | Lookup a single file or folder.
      ssLookupFile :: Pieces -> IO LookupResult

      -- | Determine the mime type of the given file.
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

      -- FIXME Need clarity on what exactly is going on here.
    , ssUseHash :: Bool
    }
