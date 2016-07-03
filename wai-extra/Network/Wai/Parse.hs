{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
-- | Some helpers for parsing data out of a raw WAI 'Request'.

module Network.Wai.Parse
    ( parseHttpAccept
    , parseRequestBody
    , RequestBodyType (..)
    , getRequestBodyType
    , sinkRequestBody
    , BackEnd
    , lbsBackEnd
    , tempFileBackEnd
    , tempFileBackEndOpts
    , Param
    , File
    , FileInfo (..)
    , parseContentType
    , ParseRequestBodyOptions (..)
    , parseRequestBodyEx
#if TEST
    , Bound (..)
    , findBound
    , sinkTillBound
    , killCR
    , killCRLF
    , takeLine
#endif
    ) where

import qualified Data.ByteString.Search as Search
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Data.Default (Default, def)
import Data.Word (Word8)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sortBy)
import Data.Function (on, fix)
import System.Directory (removeFile, getTemporaryDirectory)
import System.IO (hClose, openBinaryTempFile)
import Network.Wai
import qualified Network.HTTP.Types as H
import Control.Applicative ((<$>))
import Control.Monad (when, unless)
import Control.Monad.Trans.Resource (allocate, release, register, InternalState, runInternalState)
import Data.IORef
import Network.HTTP.Types (hContentType)
import Data.CaseInsensitive (mk)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: S.ByteString -> [S.ByteString]
parseHttpAccept = map fst
                . sortBy (rcompare `on` snd)
                . map (addSpecificity . grabQ)
                . S.split 44 -- comma
  where
    rcompare :: (Double,Int) -> (Double,Int) -> Ordering
    rcompare = flip compare
    addSpecificity (s, q) =
        -- Prefer higher-specificity types
        let semicolons = S.count 0x3B s
            stars = S.count 0x2A s
        in (s, (q, semicolons - stars))
    grabQ s =
        -- Stripping all spaces may be too harsh.
        -- Maybe just strip either side of semicolon?
        let (s', q) = S.breakSubstring ";q=" (S.filter (/=0x20) s) -- 0x20 is space
            q' = S.takeWhile (/=0x3B) (S.drop 3 q) -- 0x3B is semicolon
         in (s', readQ q')
    readQ s = case reads $ S8.unpack s of
                (x, _):_ -> x
                _ -> 1.0

-- | Store uploaded files in memory
lbsBackEnd :: Monad m => ignored1 -> ignored2 -> m S.ByteString -> m L.ByteString
lbsBackEnd _ _ popper =
    loop id
  where
    loop front = do
        bs <- popper
        if S.null bs
            then return $ L.fromChunks $ front []
            else loop $ front . (bs:)

-- | Save uploaded files on disk as temporary files
--
-- Note: starting with version 2.0, removal of temp files is registered with
-- the provided @InternalState@. It is the responsibility of the caller to
-- ensure that this @InternalState@ gets cleaned up.
tempFileBackEnd :: InternalState -> ignored1 -> ignored2 -> IO S.ByteString -> IO FilePath
tempFileBackEnd = tempFileBackEndOpts getTemporaryDirectory "webenc.buf"

-- | Same as 'tempFileBackEnd', but use configurable temp folders and patterns.
tempFileBackEndOpts :: IO FilePath -- ^ get temporary directory
                    -> String -- ^ filename pattern
                    -> InternalState
                    -> ignored1
                    -> ignored2
                    -> IO S.ByteString
                    -> IO FilePath
tempFileBackEndOpts getTmpDir pattrn internalState _ _ popper = do
    (key, (fp, h)) <- flip runInternalState internalState $ allocate it (hClose . snd)
    _ <- runInternalState (register $ removeFile fp) internalState
    fix $ \loop -> do
        bs <- popper
        unless (S.null bs) $ do
            S.hPut h bs
            loop
    release key
    return fp
    where
        it = do
            tempDir <- getTmpDir
            openBinaryTempFile tempDir pattrn

-- | A data structure that describes the behavior of
-- the parseRequestBodyEx function.
data ParseRequestBodyOptions = ParseRequestBodyOptions
    { -- | The maximum length of a filename
      prboKeyLength             :: Int
    , -- | The maximum number of files.
      prboMaxNumFiles           :: Int
    , -- | The maximum filesize per file.
      prboMaxFileSize           :: Maybe Int64
    , -- | The maximum total filesize.
      prboMaxFilesSize          :: Maybe Int64
    , -- | The maximum size of the sum of all parameters values
      prboMaxParmsValueSize     :: Int
    , -- | The maximum header lines per entry
      prboMaxHeaderLines        :: Int }

instance Default ParseRequestBodyOptions where
    def = ParseRequestBodyOptions
        { prboKeyLength=32
        , prboMaxNumFiles=10
        , prboMaxFileSize=Nothing
        , prboMaxFilesSize=Nothing
        , prboMaxParmsValueSize=65336
        , prboMaxHeaderLines=32 }

-- | Information on an uploaded file.
data FileInfo c = FileInfo
    { fileName :: S.ByteString
    , fileContentType :: S.ByteString
    , fileContent :: c
    }
    deriving (Eq, Show)

-- | Post parameter name and value.
type Param = (S.ByteString, S.ByteString)

-- | Post parameter name and associated file information.
type File y = (S.ByteString, FileInfo y)

-- | A file uploading backend. Takes the parameter name, file name, and a
-- stream of data.
type BackEnd a = S.ByteString -- ^ parameter name
              -> FileInfo ()
              -> IO S.ByteString
              -> IO a

data RequestBodyType = UrlEncoded | Multipart S.ByteString

getRequestBodyType :: Request -> Maybe RequestBodyType
getRequestBodyType req = do
    ctype' <- lookup hContentType $ requestHeaders req
    let (ctype, attrs) = parseContentType ctype'
    case ctype of
        "application/x-www-form-urlencoded" -> return UrlEncoded
        "multipart/form-data" | Just bound <- lookup "boundary" attrs -> return $ Multipart bound
        _ -> Nothing

-- | Parse a content type value, turning a single @ByteString@ into the actual
-- content type and a list of pairs of attributes.
--
-- Since 1.3.2
parseContentType :: S.ByteString -> (S.ByteString, [(S.ByteString, S.ByteString)])
parseContentType a = do
    let (ctype, b) = S.break (== semicolon) a
        attrs = goAttrs id $ S.drop 1 b
     in (ctype, attrs)
  where
    semicolon = 59
    equals = 61
    space = 32
    goAttrs front bs
        | S.null bs = front []
        | otherwise =
            let (x, rest) = S.break (== semicolon) bs
             in goAttrs (front . (goAttr x:)) $ S.drop 1 rest
    goAttr bs =
        let (k, v') = S.break (== equals) bs
            v = S.drop 1 v'
         in (strip k, strip v)
    strip = S.dropWhile (== space) . fst . S.breakEnd (/= space)

parseRequestBody :: BackEnd y
                 -> Request
                 -> IO ([Param], [File y])
parseRequestBody s r =
    case getRequestBodyType r of
        Nothing -> return ([], [])
        Just rbt -> sinkRequestBody s rbt (requestBody r)

parseRequestBodyEx :: ParseRequestBodyOptions
                   -> BackEnd y
                   -> Request
                   -> IO ([Param], [File y])
parseRequestBodyEx o s r =
    case getRequestBodyType r of
        Nothing -> return ([], [])
        Just rbt -> sinkRequestBodyEx o s rbt (requestBody r)

sinkRequestBody :: BackEnd y
                -> RequestBodyType
                -> IO S.ByteString
                -> IO ([Param], [File y])
sinkRequestBody s r body = do
    ref <- newIORef (id, id)
    let add x = atomicModifyIORef ref $ \(y, z) ->
            case x of
                Left y' -> ((y . (y':), z), ())
                Right z' -> ((y, z . (z':)), ())
    conduitRequestBody s r body add
    (x, y) <- readIORef ref
    return (x [], y [])

sinkRequestBodyEx :: ParseRequestBodyOptions
                  -> BackEnd y
                  -> RequestBodyType
                  -> IO S.ByteString
                  -> IO ([Param], [File y])
sinkRequestBodyEx o s r body = do
    ref <- newIORef ([], [])
    let add x = atomicModifyIORef ref $ \(y, z) ->
            case x of
                Left y'  -> ((y':y, z), ())
                Right z' -> ((y, z':z), ())
    conduitRequestBodyEx o s r body add
    readIORef ref

conduitRequestBody :: BackEnd y
                   -> RequestBodyType
                   -> IO S.ByteString
                   -> (Either Param (File y) -> IO ())
                   -> IO ()
conduitRequestBody _ UrlEncoded rbody add = do
    -- NOTE: in general, url-encoded data will be in a single chunk.
    -- Therefore, I'm optimizing for the usual case by sticking with
    -- strict byte strings here.
    let loop front = do
            bs <- rbody
            if S.null bs
                then return $ S.concat $ front []
                else loop $ front . (bs:)
    bs <- loop id
    mapM_ (add . Left) $ H.parseSimpleQuery bs
conduitRequestBody backend (Multipart bound) rbody add =
    parsePieces backend (S8.pack "--" `S.append` bound) rbody add

conduitRequestBodyEx :: ParseRequestBodyOptions
                     -> BackEnd y
                     -> RequestBodyType
                     -> IO S.ByteString
                     -> (Either Param (File y) -> IO ())
                     -> IO ()
conduitRequestBodyEx o _ UrlEncoded rbody add = do
    -- NOTE: in general, url-encoded data will be in a single chunk.
    -- Therefore, I'm optimizing for the usual case by sticking with
    -- strict byte strings here.
    size <- newIORef 0
    let loop front = do
            bs <- rbody
            if S.null bs
                then return $ S.concat $ front []
                else do
                    newsize <- atomicModifyIORef size $
                        \cursize -> let newsize = cursize + S.length bs in (newsize, newsize)
                    when (newsize > prboMaxParmsValueSize o) $
                        error "Maximum size of parameters exceeded"
                    loop $ front . (bs:)
    bs <- loop id
    mapM_ (add . Left) $ H.parseSimpleQuery bs
conduitRequestBodyEx o backend (Multipart bound) rbody add =
    parsePiecesEx o backend (S8.pack "--" `S.append` bound) rbody add


-- | Take one header or subheader line.
-- It makes sense to limit the maximum line length.
-- Apache's default is 8190 (http://httpd.apache.org/docs/2.2/mod/core.html#limitrequestline)
-- so we're using that here as well.
takeLine :: Source -> IO (Maybe S.ByteString)
takeLine src =
    go ""
  where
    go front = do
        bs <- readSource src
        when (S.length front + S.length bs > 8190) $ error "Header line length exceeds allowed maximum."
        if S.null bs
            then close front
            else push front bs

    close front = leftover src front >> return Nothing
    push front bs = do
        let (x, y) = S.break (== 10) bs -- LF
         in if S.null y
                then go $ front `S.append` x
                else do
                    when (S.length y > 1) $ leftover src $ S.drop 1 y
                    return $ Just $ killCR $ front `S.append` x

takeLines :: Source -> IO [S.ByteString]
takeLines src = do
    res <- takeLine src
    case res of
        Nothing -> return []
        Just l
            | S.null l -> return []
            | otherwise -> do
                ls <- takeLines src
                return $ l : ls

takeLines' :: Int -> Source -> IO [S.ByteString]
takeLines' a b = reverse <$> takeLines'' [] a b

takeLines'' :: [S.ByteString] -> Int -> Source -> IO [S.ByteString]
takeLines'' lines maxLines src
    | length lines > maxLines = error "Too many lines in mime/multipart header"
    | otherwise = do
        res <- takeLine src
        case res of
            Nothing -> return lines
            Just l
                | S.null l -> return lines
                | otherwise -> takeLines'' (l:lines) maxLines src

data Source = Source (IO S.ByteString) (IORef S.ByteString)

mkSource :: IO S.ByteString -> IO Source
mkSource f = do
    ref <- newIORef S.empty
    return $ Source f ref

readSource :: Source -> IO S.ByteString
readSource (Source f ref) = do
    bs <- atomicModifyIORef ref $ \bs -> (S.empty, bs)
    if S.null bs
        then f
        else return bs

leftover :: Source -> S.ByteString -> IO ()
leftover (Source _ ref) bs = writeIORef ref bs

parsePieces :: BackEnd y
            -> S.ByteString
            -> IO S.ByteString
            -> (Either Param (File y) -> IO ())
            -> IO ()
parsePieces sink bound rbody add =
    mkSource rbody >>= loop
  where
    loop src = do
        _boundLine <- takeLine src
        res' <- takeLines src
        unless (null res') $ do
            let ls' = map parsePair res'
            let x = do
                    cd <- lookup contDisp ls'
                    let ct = lookup contType ls'
                    let attrs = parseAttrs cd
                    name <- lookup "name" attrs
                    return (ct, name, lookup "filename" attrs)
            case x of
                Just (mct, name, Just filename) -> do
                    let ct = fromMaybe "application/octet-stream" mct
                        fi0 = FileInfo filename ct ()
                    ((wasFound, _fileSize), y) <- sinkTillBound' bound name fi0 sink src Nothing
                    add $ Right (name, fi0 { fileContent = y })
                    when wasFound (loop src)
                Just (_ct, name, Nothing) -> do
                    let seed = id
                    let iter front bs = return $ front . (:) bs
                    ((wasFound, _fileSize), front) <- sinkTillBound bound iter seed src Nothing
                    let bs = S.concat $ front []
                    let x' = (name, bs)
                    add $ Left x'
                    when wasFound (loop src)
                _ -> do
                    -- ignore this part
                    let seed = ()
                        iter () _ = return ()
                    ((wasFound, _fileSize), ()) <- sinkTillBound bound iter seed src Nothing
                    when wasFound (loop src)
      where
        contDisp = mk $ S8.pack "Content-Disposition"
        contType = mk $ S8.pack "Content-Type"
        parsePair s =
            let (x, y) = breakDiscard 58 s -- colon
             in (mk $ x, S.dropWhile (== 32) y) -- space

parsePiecesEx :: ParseRequestBodyOptions
              -> BackEnd y
              -> S.ByteString
              -> IO S.ByteString
              -> (Either Param (File y) -> IO ())
              -> IO ()
parsePiecesEx o sink bound rbody add =
    mkSource rbody >>= loop 0 0 0 0
  where
    loop :: Int -> Int -> Int -> Int64 -> Source -> IO ()
    loop numParms numFiles parmSize filesSize src = do
        _boundLine <- takeLine src
        res' <- takeLines' (prboMaxHeaderLines o) src
        unless (null res') $ do
            let ls' = map parsePair res'
            let x = do
                    cd <- lookup contDisp ls'
                    let ct = lookup contType ls'
                    let attrs = parseAttrs cd
                    name <- lookup "name" attrs
                    return (ct, name, lookup "filename" attrs)
            case x of
                Just (mct, name, Just filename) -> do
                    when (S.length name > prboKeyLength o) $
                        error "Filename is too long"
                    when (numFiles >= prboMaxNumFiles o) $
                        error "Maximum number of files exceeded"
                    let ct = fromMaybe "application/octet-stream" mct
                        fi0 = FileInfo filename ct ()
                        fs = catMaybes [ prboMaxFileSize o
                                       , subtract filesSize <$> prboMaxFilesSize o ]
                        mfs = if fs == [] then Nothing else Just $ minimum fs
                    ((wasFound, fileSize), y) <- sinkTillBound' bound name fi0 sink src mfs
                    let newFilesSize = filesSize + fileSize
                    add $ Right (name, fi0 { fileContent = y })
                    when wasFound $ loop numParms (numFiles + 1) parmSize newFilesSize src
                Just (_ct, name, Nothing) -> do
                    when (S.length name > prboKeyLength o) $
                        error "Parameter name is too long"
                    let seed = id
                    let iter front bs = return $ front . (:) bs
                    ((wasFound, _fileSize), front) <- sinkTillBound bound iter seed src $
                        Just . fromIntegral . prboMaxParmsValueSize $ o
                    let bs = S.concat $ front []
                    let x' = (name, bs)
                    let newParmSize = parmSize + S.length name + S.length bs
                    when (newParmSize > prboMaxParmsValueSize o) $
                        error "Maximum size of parameters exceeded"
                    add $ Left x'
                    when wasFound $ loop (numParms + 1) numFiles
                        newParmSize filesSize src
                _ -> do
                    -- ignore this part
                    let seed = ()
                        iter () _ = return ()
                    ((wasFound, _fileSize), ()) <- sinkTillBound bound iter seed src Nothing
                    when wasFound $ loop numParms numFiles parmSize filesSize src
      where
        contDisp = mk $ S8.pack "Content-Disposition"
        contType = mk $ S8.pack "Content-Type"
        parsePair s =
            let (x, y) = breakDiscard 58 s -- colon
             in (mk $ x, S.dropWhile (== 32) y) -- space


data Bound = FoundBound S.ByteString S.ByteString
           | NoBound
           | PartialBound
    deriving (Eq, Show)

findBound :: S.ByteString -> S.ByteString -> Bound
findBound b bs = handleBreak $ Search.breakOn b bs
  where
    handleBreak (h, t)
        | S.null t = go [lowBound..S.length bs - 1]
        | otherwise = FoundBound h $ S.drop (S.length b) t

    lowBound = max 0 $ S.length bs - S.length b

    go [] = NoBound
    go (i:is)
        | mismatch [0..S.length b - 1] [i..S.length bs - 1] = go is
        | otherwise =
            let endI = i + S.length b
             in if endI > S.length bs
                    then PartialBound
                    else FoundBound (S.take i bs) (S.drop endI bs)
    mismatch [] _ = False
    mismatch _ [] = False
    mismatch (x:xs) (y:ys)
        | S.index b x == S.index bs y = mismatch xs ys
        | otherwise = True

sinkTillBound' :: S.ByteString
               -> S.ByteString
               -> FileInfo ()
               -> BackEnd y
               -> Source
               -> Maybe Int64
               -> IO ((Bool, Int64), y)
sinkTillBound' bound name fi sink src max = do
    (next, final) <- wrapTillBound bound src max
    y <- sink name fi next
    b <- final
    return (b, y)

data WTB = WTBWorking (S.ByteString -> S.ByteString)
         | WTBDone Bool
wrapTillBound :: S.ByteString -- ^ bound
              -> Source
              -> Maybe Int64
              -> IO (IO S.ByteString, IO (Bool, Int64)) -- ^ Bool indicates if the bound was found
wrapTillBound bound src max = do
    ref <- newIORef $ WTBWorking id
    sref <- newIORef (0 :: Int64)
    return (go ref sref, final ref sref)
  where
    final ref sref = do
        x <- readIORef ref
        case x of
            WTBWorking _ -> error "wrapTillBound did not finish"
            WTBDone y -> do
                siz <- readIORef sref
                return (y, siz)

    go ref sref = do
        state <- readIORef ref
        case state of
            WTBDone _ -> return S.empty
            WTBWorking front -> do
                bs <- readSource src
                cur <- atomicModifyIORef sref $ \ cur ->
                    let new = cur + fromIntegral (S.length bs) in (new, new)
                case max of
                    Just max' | cur > max' -> error "Maximum size exceeded"
                    _ -> return ()
                if S.null bs
                    then do
                        writeIORef ref $ WTBDone False
                        return $ front bs
                    else push $ front bs
      where
        push bs =
            case findBound bound bs of
                FoundBound before after -> do
                    let before' = killCRLF before
                    leftover src after
                    writeIORef ref $ WTBDone True
                    return before'
                NoBound -> do
                    -- don't emit newlines, in case it's part of a bound
                    let (toEmit, front') =
                            if not (S8.null bs) && S8.last bs `elem` ['\r','\n']
                                then let (x, y) = S.splitAt (S.length bs - 2) bs
                                      in (x, S.append y)
                                else (bs, id)
                    writeIORef ref $ WTBWorking front'
                    if S.null toEmit
                        then go ref sref
                        else return toEmit
                PartialBound -> do
                    writeIORef ref $ WTBWorking $ S.append bs
                    go ref sref

sinkTillBound :: S.ByteString
              -> (x -> S.ByteString -> IO x)
              -> x
              -> Source
              -> Maybe Int64
              -> IO ((Bool, Int64), x)
sinkTillBound bound iter seed0 src max = do
    (next, final) <- wrapTillBound bound src max
    let loop seed = do
            bs <- next
            if S.null bs
                then return seed
                else iter seed bs >>= loop
    seed <- loop seed0
    b <- final
    return (b, seed)

parseAttrs :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseAttrs = map go . S.split 59 -- semicolon
  where
    tw = S.dropWhile (== 32) -- space
    dq s = if S.length s > 2 && S.head s == 34 && S.last s == 34 -- quote
                then S.tail $ S.init s
                else s
    go s =
        let (x, y) = breakDiscard 61 s -- equals sign
         in (tw x, dq $ tw y)

killCRLF :: S.ByteString -> S.ByteString
killCRLF bs
    | S.null bs || S.last bs /= 10 = bs -- line feed
    | otherwise = killCR $ S.init bs

killCR :: S.ByteString -> S.ByteString
killCR bs
    | S.null bs || S.last bs /= 13 = bs -- carriage return
    | otherwise = S.init bs
