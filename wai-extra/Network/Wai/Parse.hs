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
import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Function (on, fix)
import System.Directory (removeFile, getTemporaryDirectory)
import System.IO (hClose, openBinaryTempFile)
import Network.Wai
import qualified Network.HTTP.Types as H
import Control.Monad (when, unless)
import Control.Monad.Trans.Resource (allocate, release, register, InternalState, runInternalState)
import Data.IORef
import Network.HTTP.Types (hContentType)

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

-- | Same as 'tempFileSink', but use configurable temp folders and patterns.
tempFileBackEndOpts :: IO FilePath -- ^ get temporary directory
                    -> String -- ^ filename pattern
                    -> InternalState
                    -> ignored1
                    -> ignored2
                    -> IO S.ByteString
                    -> IO FilePath
tempFileBackEndOpts getTmpDir pattern internalState _ _ popper = do
    (key, (fp, h)) <- flip runInternalState internalState $ allocate (do
        tempDir <- getTmpDir
        openBinaryTempFile tempDir pattern) (\(_, h) -> hClose h)
    _ <- runInternalState (register $ removeFile fp) internalState
    fix $ \loop -> do
        bs <- popper
        unless (S.null bs) $ do
            S.hPut h bs
            loop
    release key
    return fp

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

takeLine :: Source -> IO (Maybe S.ByteString)
takeLine src =
    go id
  where
    go front = do
        bs <- readSource src
        if S.null bs
            then close front
            else push front bs

    close front = leftover src (front S.empty) >> return Nothing
    push front bs = do
        let (x, y) = S.break (== 10) $ front bs -- LF
         in if S.null y
                then go $ S.append x
                else do
                    when (S.length y > 1) $ leftover src $ S.drop 1 y
                    return $ Just $ killCR x

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
                    (wasFound, y) <- sinkTillBound' bound name fi0 sink src
                    add $ Right (name, fi0 { fileContent = y })
                    when wasFound (loop src)
                Just (_ct, name, Nothing) -> do
                    let seed = id
                    let iter front bs = return $ front . (:) bs
                    (wasFound, front) <- sinkTillBound bound iter seed src
                    let bs = S.concat $ front []
                    let x' = (name, bs)
                    add $ Left x'
                    when wasFound (loop src)
                _ -> do
                    -- ignore this part
                    let seed = ()
                        iter () _ = return ()
                    (wasFound, ()) <- sinkTillBound bound iter seed src
                    when wasFound (loop src)
      where
        contDisp = S8.pack "Content-Disposition"
        contType = S8.pack "Content-Type"
        parsePair s =
            let (x, y) = breakDiscard 58 s -- colon
             in (x, S.dropWhile (== 32) y) -- space

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
               -> IO (Bool, y)
sinkTillBound' bound name fi sink src = do
    (next, final) <- wrapTillBound bound src
    y <- sink name fi next
    b <- final
    return (b, y)

data WTB = WTBWorking (S.ByteString -> S.ByteString)
         | WTBDone Bool
wrapTillBound :: S.ByteString -- ^ bound
              -> Source
              -> IO (IO S.ByteString, IO Bool) -- ^ Bool indicates if the bound was found
wrapTillBound bound src = do
    ref <- newIORef $ WTBWorking id
    return (go ref, final ref)
  where
    final ref = do
        x <- readIORef ref
        case x of
            WTBWorking _ -> error "wrapTillBound did not finish"
            WTBDone y -> return y

    go ref = do
        state <- readIORef ref
        case state of
            WTBDone _ -> return S.empty
            WTBWorking front -> do
                bs <- readSource src
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
                        then go ref
                        else return toEmit
                PartialBound -> do
                    writeIORef ref $ WTBWorking $ S.append bs
                    go ref

sinkTillBound :: S.ByteString
              -> (x -> S.ByteString -> IO x)
              -> x
              -> Source
              -> IO (Bool, x)
sinkTillBound bound iter seed0 src = do
    (next, final) <- wrapTillBound bound src
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
