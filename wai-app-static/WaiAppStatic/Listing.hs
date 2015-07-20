{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module WaiAppStatic.Listing
    ( defaultListing
    ) where

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5            as H
import           Text.Blaze                  ((!))
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import WaiAppStatic.Types
#if !MIN_VERSION_time(1,5,0)
import System.Locale (defaultTimeLocale)
#endif
import Data.List (sortBy)
import Util

import qualified Text.Blaze.Html.Renderer.Utf8 as HU

-- | Provides a default directory listing, suitable for most apps.
--
-- Code below taken from Happstack: <http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty/happstack-server/src/Happstack/Server/FileServe/BuildingBlocks.hs>
defaultListing :: Listing
defaultListing pieces (Folder contents) = do
    let isTop = null pieces || map Just pieces == [toPiece ""]
    let fps'' :: [Either FolderName File]
        fps'' = (if isTop then id else (Left (unsafeToPiece "") :)) contents -- FIXME emptyParentFolder feels like a bit of a hack
    return $ HU.renderHtmlBuilder
           $ H.html $ do
             H.head $ do
                 let title = T.intercalate "/" $ map fromPiece pieces
                 let title' = if T.null title then "root folder" else title
                 H.title $ H.toHtml title'
                 H.style $ H.toHtml $ unlines [ "table { margin: 0 auto; width: 760px; border-collapse: collapse; font-family: 'sans-serif'; }"
                                              , "table, th, td { border: 1px solid #353948; }"
                                              , "td.size { text-align: right; font-size: 0.7em; width: 50px }"
                                              , "td.date { text-align: right; font-size: 0.7em; width: 130px }"
                                              , "td { padding-right: 1em; padding-left: 1em; }"
                                              , "th.first { background-color: white; width: 24px }"
                                              , "td.first { padding-right: 0; padding-left: 0; text-align: center }"
                                              , "tr { background-color: white; }"
                                              , "tr.alt { background-color: #A3B5BA}"
                                              , "th { background-color: #3C4569; color: white; font-size: 1.125em; }"
                                              , "h1 { width: 760px; margin: 1em auto; font-size: 1em; font-family: sans-serif }"
                                              , "img { width: 20px }"
                                              , "a { text-decoration: none }"
                                              ]
             H.body $ do
                 let hasTrailingSlash =
                        case map fromPiece $ reverse $ pieces of
                            "":_ -> True
                            _ -> False
                 H.h1 $ showFolder' hasTrailingSlash $ filter (not . T.null . fromPiece) pieces
                 renderDirectoryContentsTable (map fromPiece pieces) haskellSrc folderSrc fps''
  where
    image x = T.unpack $ T.concat [(relativeDirFromPieces pieces), ".hidden/", x, ".png"]
    folderSrc = image "folder"
    haskellSrc = image "haskell"
    showName "" = "root"
    showName x = x

    -- Add a link to the root of the tree
    showFolder' :: Bool -> Pieces -> H.Html
    showFolder' hasTrailingSlash pieces  = showFolder hasTrailingSlash (unsafeToPiece "root" : pieces)

    showFolder :: Bool -> Pieces -> H.Html
    showFolder _ [] = "/" -- won't happen
    showFolder _ [x] = H.toHtml $ showName $ fromPiece x
    showFolder hasTrailingSlash (x:xs) = do
        let len = length xs - (if hasTrailingSlash then 0 else 1)
            href
                | len == 0 = "."
                | otherwise = concat $ replicate len "../" :: String
        H.a ! A.href (H.toValue href) $ H.toHtml $ showName $ fromPiece x
        " / " :: H.Html
        showFolder hasTrailingSlash xs

-- | a function to generate an HTML table showing the contents of a directory on the disk
--
-- This function generates most of the content of the
-- 'renderDirectoryContents' page. If you want to style the page
-- differently, or add google analytics code, etc, you can just create
-- a new page template to wrap around this HTML.
--
-- see also: 'getMetaData', 'renderDirectoryContents'
renderDirectoryContentsTable :: [T.Text] -- ^ requested path info
                             -> String
                             -> String
                             -> [Either FolderName File]
                             -> H.Html
renderDirectoryContentsTable pathInfo' haskellSrc folderSrc fps =
           H.table $ do H.thead $ do H.th ! (A.class_ "first") $ H.img ! (A.src $ H.toValue haskellSrc)
                                     H.th "Name"
                                     H.th "Modified"
                                     H.th "Size"
                        H.tbody $ mapM_ mkRow (zip (sortBy sortMD fps) $ cycle [False, True])
    where
      sortMD :: Either FolderName File -> Either FolderName File -> Ordering
      sortMD Left{} Right{} = LT
      sortMD Right{} Left{} = GT
      sortMD (Left a) (Left b) = compare a b
      sortMD (Right a) (Right b) = compare (fileName a) (fileName b)

      mkRow :: (Either FolderName File, Bool) -> H.Html
      mkRow (md, alt) =
          (if alt then (! A.class_ "alt") else id) $
          H.tr $ do
                   H.td ! A.class_ "first"
                        $ case md of
                            Left{} -> H.img ! A.src (H.toValue folderSrc)
                                            ! A.alt "Folder"
                            Right{} -> return ()
                   let name =
                           case either id fileName md of
                               (fromPiece -> "") -> unsafeToPiece ".."
                               x -> x
                   let isFile = either (const False) (const True) md
                       href = addCurrentDir $ fromPiece name
                       addCurrentDir x =
                           case reverse pathInfo' of
                               "":_ -> x -- has a trailing slash
                               [] -> x -- at the root
                               currentDir:_ -> T.concat [currentDir, "/", x]
                   H.td (H.a ! A.href (H.toValue href) $ H.toHtml $ fromPiece name)
                   H.td ! A.class_ "date" $ H.toHtml $
                       case md of
                           Right File { fileGetModified = Just t } ->
                                   formatCalendarTime defaultTimeLocale "%d-%b-%Y %X" t
                           _ -> ""
                   H.td ! A.class_ "size" $ H.toHtml $
                       case md of
                           Right File { fileGetSize = s } -> prettyShow s
                           Left{} -> ""
      formatCalendarTime a b c =  formatTime a b $ posixSecondsToUTCTime (realToFrac c :: POSIXTime)
      prettyShow x
        | x > 1024 = prettyShowK $ x `div` 1024
        | otherwise = addCommas "B" x
      prettyShowK x
        | x > 1024 = prettyShowM $ x `div` 1024
        | otherwise = addCommas "KB" x
      prettyShowM x
        | x > 1024 = prettyShowG $ x `div` 1024
        | otherwise = addCommas "MB" x
      prettyShowG x = addCommas "GB" x
      addCommas s = (++ (' ' : s)) . reverse . addCommas' . reverse . show
      addCommas' (a:b:c:d:e) = a : b : c : ',' : addCommas' (d : e)
      addCommas' x = x
