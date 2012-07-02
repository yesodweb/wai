{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Some helpers for parsing data out of a raw WAI 'Request'.

module Network.Wai.Parse
    ( parseHttpAccept
    , parseRequestBody
    , RequestBodyType (..)
    , getRequestBodyType
    , sinkRequestBody
    , conduitRequestBody
    , lbsSink
    , tempFileSink
    , tempFileSinkOpts
    , Param
    , File
    , FileInfo (..)
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
import Data.Conduit.Internal (sinkToPipe)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types as H
import Data.Either (partitionEithers)
import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (allocate, release, register)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: S.ByteString -> [S.ByteString]
parseHttpAccept = map fst
                . sortBy (rcompare `on` snd)
                . map grabQ
                . S.split 44 -- comma
  where
    rcompare :: Double -> Double -> Ordering
    rcompare = flip compare
    grabQ s =
        let (s', q) = breakDiscard 59 s -- semicolon
            (_, q') = breakDiscard 61 q -- equals sign
         in (trimWhite s', readQ $ trimWhite q')
    readQ s = case reads $ S8.unpack s of
                (x, _):_ -> x
                _ -> 1.0
    trimWhite = S.dropWhile (== 32) -- space

-- | Store uploaded files in memory
lbsSink :: Monad m => Sink S.ByteString m L.ByteString
lbsSink = fmap L.fromChunks CL.consume

-- | Save uploaded files on disk as temporary files
tempFileSink :: MonadResource m => Sink S.ByteString m FilePath
tempFileSink = tempFileSinkOpts getTemporaryDirectory "webenc.buf"

-- | Same as 'tempFileSink', but use configurable temp folders and patterns.
tempFileSinkOpts :: MonadResource m
                 => IO FilePath -- ^ get temporary directory
                 -> String -- ^ filename pattern
                 -> Sink S.ByteString m FilePath
tempFileSinkOpts getTmpDir pattern = do
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

type Param = (S.ByteString, S.ByteString)
type File y = (S.ByteString, FileInfo y)

data RequestBodyType = UrlEncoded | Multipart S.ByteString

getRequestBodyType :: Request -> Maybe RequestBodyType
getRequestBodyType req = do
    ctype <- lookup "Content-Type" $ requestHeaders req
    if urlenc `S.isPrefixOf` ctype
        then Just UrlEncoded
        else case boundary ctype of
                Just x -> Just $ Multipart x
                Nothing -> Nothing
  where
    urlenc = S8.pack "application/x-www-form-urlencoded"
    formBound = S8.pack "multipart/form-data;"
    bound' = "boundary="
    boundary s =
        if "multipart/form-data;" `S.isPrefixOf` s
            then
                let s' = S.dropWhile (== 32) $ S.drop (S.length formBound) s
                 in if bound' `S.isPrefixOf` s'
                        then Just $ S.drop (S.length bound') s'
                        else Nothing
            else Nothing

parseRequestBody :: Sink S.ByteString (ResourceT IO) y
                 -> Request
                 -> ResourceT IO ([Param], [File y])
parseRequestBody s r =
    case getRequestBodyType r of
        Nothing -> return ([], [])
        Just rbt -> fmap partitionEithers $ requestBody r $$ conduitRequestBody s rbt =$ CL.consume

sinkRequestBody :: Sink S.ByteString (ResourceT IO) y
                -> RequestBodyType
                -> Sink S.ByteString (ResourceT IO) ([Param], [File y])
sinkRequestBody s r = fmap partitionEithers $ conduitRequestBody s r =$ CL.consume

conduitRequestBody :: Sink S.ByteString (ResourceT IO) y
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

takeLine :: Monad m => Pipe S.ByteString S.ByteString o u m (Maybe S.ByteString)
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

takeLines :: Pipe S.ByteString S.ByteString o u (ResourceT IO) [S.ByteString]
takeLines = do
    res <- takeLine
    case res of
        Nothing -> return []
        Just l
            | S.null l -> return []
            | otherwise -> do
                ls <- takeLines
                return $ l : ls

parsePieces :: Sink S.ByteString (ResourceT IO) y
            -> S.ByteString
            -> Pipe S.ByteString S.ByteString (Either Param (File y)) u (ResourceT IO) ()
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
                    (wasFound, y) <- sinkTillBound' bound sink
                    let fi = FileInfo filename ct y
                    let y' = (name, fi)
                    yield $ Right y'
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
               -> Sink S.ByteString (ResourceT IO) y
               -> Pipe S.ByteString S.ByteString o u (ResourceT IO) (Bool, y)
sinkTillBound' bound sink = conduitTillBound bound >+> withUpstream (sinkToPipe sink)

conduitTillBound :: Monad m
                 => S.ByteString -- bound
                 -> Pipe S.ByteString S.ByteString S.ByteString u m Bool
conduitTillBound bound =
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
              -> Pipe S.ByteString S.ByteString o u (ResourceT IO) (Bool, x)
sinkTillBound bound iter seed0 =
    conduitTillBound bound >+> withUpstream (CL.foldM iter' seed0)
  where
    iter' a b = liftIO $ iter a b

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
