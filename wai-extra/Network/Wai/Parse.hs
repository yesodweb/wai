{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , conduitRequestBody
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
import Data.Function (on)
import System.Directory (removeFile, getTemporaryDirectory)
import System.IO (hClose, openBinaryTempFile)
import Network.Wai
import Data.Conduit
import Data.Conduit.Internal ()
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types as H
import Data.Either (partitionEithers)
import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (allocate, release, register)
#if MIN_VERSION_conduit(1, 0, 0)
import Data.Conduit.Internal (Pipe (NeedInput, HaveOutput), (>+>), withUpstream, injectLeftovers, ConduitM (..))
import Data.Void (Void)
#else
import Data.Conduit.Internal (sinkToPipe)
#endif

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
lbsBackEnd :: Monad m => ignored1 -> ignored2 -> Sink S.ByteString m L.ByteString
lbsBackEnd _ _ = fmap L.fromChunks CL.consume

-- | Save uploaded files on disk as temporary files
tempFileBackEnd :: MonadResource m => ignored1 -> ignored2 -> Sink S.ByteString m FilePath
tempFileBackEnd = tempFileBackEndOpts getTemporaryDirectory "webenc.buf"

-- | Same as 'tempFileSink', but use configurable temp folders and patterns.
tempFileBackEndOpts :: MonadResource m
                    => IO FilePath -- ^ get temporary directory
                    -> String -- ^ filename pattern
                    -> ignored1
                    -> ignored2
                    -> Sink S.ByteString m FilePath
tempFileBackEndOpts getTmpDir pattern _ _ = do
    (key, (fp, h)) <- lift $ allocate (do
        tempDir <- getTmpDir
        openBinaryTempFile tempDir pattern) (\(_, h) -> hClose h)
    _ <- lift $ register $ removeFile fp
    CB.sinkHandle h
    lift $ release key
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

-- | A file uploading backend. Takes the parameter name, file name, and content
-- type, and returns a `Sink` for storing the contents.
type BackEnd a = S.ByteString -- ^ parameter name
              -> FileInfo ()
              -> Sink S.ByteString (ResourceT IO) a

data RequestBodyType = UrlEncoded | Multipart S.ByteString

getRequestBodyType :: Request -> Maybe RequestBodyType
getRequestBodyType req = do
    ctype' <- lookup "Content-Type" $ requestHeaders req
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
                 -> ResourceT IO ([Param], [File y])
parseRequestBody s r =
    case getRequestBodyType r of
        Nothing -> return ([], [])
        Just rbt -> fmap partitionEithers $ requestBody r $$ conduitRequestBody s rbt =$ CL.consume

sinkRequestBody :: BackEnd y
                -> RequestBodyType
                -> Sink S.ByteString (ResourceT IO) ([Param], [File y])
sinkRequestBody s r = fmap partitionEithers $ conduitRequestBody s r =$ CL.consume

conduitRequestBody :: BackEnd y
                   -> RequestBodyType
                   -> Conduit S.ByteString (ResourceT IO) (Either Param (File y))
conduitRequestBody _ UrlEncoded = do
    -- NOTE: in general, url-encoded data will be in a single chunk.
    -- Therefore, I'm optimizing for the usual case by sticking with
    -- strict byte strings here.
    bs <- CL.consume
    mapM_ yield $ map Left $ H.parseSimpleQuery $ S.concat bs
conduitRequestBody backend (Multipart bound) =
    parsePieces backend $ S8.pack "--" `S.append` bound

#if MIN_VERSION_conduit(1, 0, 0)
takeLine :: Monad m => Consumer S.ByteString m (Maybe S.ByteString)
#else
takeLine :: Monad m => Pipe S.ByteString S.ByteString o u m (Maybe S.ByteString)
#endif
takeLine =
    go id
  where
    go front = await >>= maybe (close front) (push front)

    close front = leftover (front S.empty) >> return Nothing
    push front bs = do
        let (x, y) = S.break (== 10) $ front bs -- LF
         in if S.null y
                then go $ S.append x
                else do
                    when (S.length y > 1) $ leftover $ S.drop 1 y
                    return $ Just $ killCR x

#if MIN_VERSION_conduit(1, 0, 0)
takeLines :: Consumer S.ByteString (ResourceT IO) [S.ByteString]
#else
takeLines :: Pipe S.ByteString S.ByteString o u (ResourceT IO) [S.ByteString]
#endif
takeLines = do
    res <- takeLine
    case res of
        Nothing -> return []
        Just l
            | S.null l -> return []
            | otherwise -> do
                ls <- takeLines
                return $ l : ls

parsePieces :: BackEnd y
            -> S.ByteString
#if MIN_VERSION_conduit(1, 0, 0)
            -> ConduitM S.ByteString (Either Param (File y)) (ResourceT IO) ()
#else
            -> Pipe S.ByteString S.ByteString (Either Param (File y)) u (ResourceT IO) ()
#endif
parsePieces sink bound =
    loop
  where
    loop = do
        _boundLine <- takeLine
        res' <- takeLines
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
                    (wasFound, y) <- sinkTillBound' bound name fi0 sink
                    yield $ Right (name, fi0 { fileContent = y })
                    when wasFound loop
                Just (_ct, name, Nothing) -> do
                    let seed = id
                    let iter front bs = return $ front . (:) bs
                    (wasFound, front) <- sinkTillBound bound iter seed
                    let bs = S.concat $ front []
                    let x' = (name, bs)
                    yield $ Left x'
                    when wasFound loop
                _ -> do
                    -- ignore this part
                    let seed = ()
                        iter () _ = return ()
                    (wasFound, ()) <- sinkTillBound bound iter seed
                    when wasFound loop
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
#if MIN_VERSION_conduit(1, 0, 0)
               -> ConduitM S.ByteString o (ResourceT IO) (Bool, y)
#else
               -> Pipe S.ByteString S.ByteString o u (ResourceT IO) (Bool, y)
#endif
sinkTillBound' bound name fi sink =
#if MIN_VERSION_conduit(1, 0, 0)
    ConduitM $ anyOutput $
#endif
    conduitTillBound bound >+> withUpstream (fix $ sink name fi)
  where
#if MIN_VERSION_conduit(1, 0, 0)
    fix :: Sink S8.ByteString (ResourceT IO) y -> Pipe Void S8.ByteString Void Bool (ResourceT IO) y
    fix (ConduitM p) = ignoreTerm >+> injectLeftovers p
    ignoreTerm = await' >>= maybe (return ()) (\x -> yield' x >> ignoreTerm)
    await' = NeedInput (return . Just) (const $ return Nothing)
    yield' = HaveOutput (return ()) (return ())

    anyOutput p = p >+> dropInput
    dropInput = NeedInput (const dropInput) return
#else
    fix = sinkToPipe
#endif

conduitTillBound :: Monad m
                 => S.ByteString -- bound
#if MIN_VERSION_conduit(1, 0, 0)
                 -> Pipe S.ByteString S.ByteString S.ByteString () m Bool
#else
                 -> Pipe S.ByteString S.ByteString S.ByteString u m Bool
#endif
conduitTillBound bound =
#if MIN_VERSION_conduit(1, 0, 0)
    unConduitM $
#endif
    go id
  where
    go front = await >>= maybe (close front) (push front)
    close front = do
        let bs = front S.empty
        unless (S.null bs) $ yield bs
        return False
    push front bs' = do
        let bs = front bs'
        case findBound bound bs of
            FoundBound before after -> do
                let before' = killCRLF before
                yield before'
                leftover after
                return True
            NoBound -> do
                -- don't emit newlines, in case it's part of a bound
                let (toEmit, front') =
                        if not (S8.null bs) && S8.last bs `elem` "\r\n"
                            then let (x, y) = S.splitAt (S.length bs - 2) bs
                                  in (x, S.append y)
                            else (bs, id)
                yield toEmit
                go front'
            PartialBound -> go $ S.append bs

sinkTillBound :: S.ByteString
              -> (x -> S.ByteString -> IO x)
              -> x
#if MIN_VERSION_conduit(1, 0, 0)
              -> Consumer S.ByteString (ResourceT IO) (Bool, x)
#else
              -> Pipe S.ByteString S.ByteString o u (ResourceT IO) (Bool, x)
#endif
sinkTillBound bound iter seed0 =
#if MIN_VERSION_conduit(1, 0, 0)
    ConduitM $
#endif
    (conduitTillBound bound >+> (withUpstream $ ij $ CL.foldM iter' seed0))
  where
    iter' a b = liftIO $ iter a b
#if MIN_VERSION_conduit(1, 0, 0)
    ij (ConduitM p) = ignoreTerm >+> injectLeftovers p
    ignoreTerm = await' >>= maybe (return ()) (\x -> yield' x >> ignoreTerm)
    await' = NeedInput (return . Just) (const $ return Nothing)
    yield' = HaveOutput (return ()) (return ())
#else
    ij = id
#endif

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
