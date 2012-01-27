{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Some helpers for parsing data out of a raw WAI 'Request'.

module Network.Wai.Parse
    ( parseHttpAccept
    , parseRequestBody
    , conduitRequestBody
    , lbsBackEnd
    , tempFileBackEnd
    , BackEnd (..)
    , Param
    , File
    , FileInfo (..)
    -- ** Deprecated
    , lbsSink
    , tempFileSink
#if TEST
    , Bound (..)
    , findBound
    , sinkTillBound
    , killCR
    , killCRLF
    , takeLine
#endif
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Data.Word (Word8)
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Function (on)
import System.Directory (removeFile, getTemporaryDirectory)
import System.IO (hClose, openBinaryTempFile)
import Network.Wai
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types as H
import Data.Either (partitionEithers)

uncons :: S.ByteString -> Maybe (Word8, S.ByteString)
uncons s
    | S.null s = Nothing
    | otherwise = Just (S.head s, S.tail s)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

qsDecode :: S.ByteString -> S.ByteString
qsDecode z = fst $ S.unfoldrN (S.length z) go z
  where
    go bs =
        case uncons bs of
            Nothing -> Nothing
            Just (43, ws) -> Just (32, ws) -- plus to space
            Just (37, ws) -> Just $ fromMaybe (37, ws) $ do -- percent
                (x, xs) <- uncons ws
                x' <- hexVal x
                (y, ys) <- uncons xs
                y' <- hexVal y
                Just $ (combine x' y', ys)
            Just (w, ws) -> Just (w, ws)
    hexVal w
        | 48 <= w && w <= 57  = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70  = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b

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

-- | A destination for file data, with concrete implemtations
-- provided by 'lbsBackEnd' and 'tempFileBackEnd'
data BackEnd y = forall x . BackEnd
    { initialize :: IO x
    , append :: x -> S.ByteString -> IO x
    , close :: x -> IO y
    , finalize :: y -> IO ()
    }

-- | Store uploaded files in memory
lbsBackEnd :: BackEnd L.ByteString
lbsBackEnd = BackEnd
    { initialize = return id
    , append = \front bs -> return $ front . (:) bs
    , close = \front -> return $ L.fromChunks $ front []
    , finalize = \_ -> return ()
    }

-- | Save uploaded files on disk as temporary files
tempFileBackEnd :: BackEnd FilePath
tempFileBackEnd = BackEnd
    { initialize = do
        tempDir <- getTemporaryDirectory
        openBinaryTempFile tempDir "webenc.buf"
    , append = \(fp, h) bs -> S.hPut h bs >> return (fp, h)
    , close = \(fp, h) -> do
        hClose h
        return fp
    , finalize = \fp -> removeFile fp
    }

-- | This function has been renamed to 'lbsBackEnd'
lbsSink :: BackEnd L.ByteString
lbsSink = lbsBackEnd

-- | This function has been renamed to  'tempFileBackEnd'
tempFileSink :: BackEnd FilePath
tempFileSink = tempFileBackEnd

{-# DEPRECATED lbsSink "Please use 'lbsBackEnd'" #-}
{-# DEPRECATED tempFileSink "Please use 'tempFileBackEnd'" #-}

-- | Information on an uploaded file.
data FileInfo c = FileInfo
    { fileName :: S.ByteString
    , fileContentType :: S.ByteString
    , fileContent :: c
    }
    deriving (Eq, Show)

type Param = (S.ByteString, S.ByteString)
type File y = (S.ByteString, FileInfo y)

parseRequestBody :: BackEnd y
                 -> Request
                 -> C.Sink S.ByteString IO ([Param], [File y])
parseRequestBody s r = fmap partitionEithers $ conduitRequestBody s r C.=$ CL.consume

conduitRequestBody :: BackEnd y
                   -> Request
                   -> C.Conduit S.ByteString IO (Either Param (File y))
conduitRequestBody sink req = do
    case ctype of
        Nothing -> C.Conduit $ return $ C.PreparedConduit
            { C.conduitPush = \bs -> return $ C.Finished (Just bs) []
            , C.conduitClose = return []
            }
        Just Nothing -> C.sequenceSink () $ \() -> do -- url-encoded
            -- NOTE: in general, url-encoded data will be in a single chunk.
            -- Therefore, I'm optimizing for the usual case by sticking with
            -- strict byte strings here.
            bs <- CL.consume
            return $ C.Emit () $ map Left $ H.parseSimpleQuery $ S.concat bs
        Just (Just bound) -> -- multi-part
            let bound'' = S8.pack "--" `S.append` bound
             in parsePieces sink bound''
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
    ctype = do
      ctype' <- lookup "Content-Type" $ requestHeaders req
      if urlenc `S.isPrefixOf` ctype'
          then Just Nothing
          else case boundary ctype' of
                Just x -> Just $ Just x
                Nothing -> Nothing

takeLine :: C.Sink S.ByteString IO (Maybe S.ByteString)
takeLine =
    C.sinkState id push close'
  where
    close' _ = return Nothing
    push front bs = do
        let (x, y) = S.break (== 10) $ front bs -- LF
         in if S.null y
                then return $ C.StateProcessing $ S.append x
                else do
                    let lo = if S.length y > 1 then Just (S.drop 1 y) else Nothing
                    return $ C.StateDone lo $ Just $ killCR x

takeLines :: C.Sink S.ByteString IO [S.ByteString]
takeLines = do
    res <- takeLine
    case res of
        Nothing -> return []
        Just l
            | S.null l -> return []
            | otherwise -> do
                ls <- takeLines
                return $ l : ls

parsePieces :: BackEnd y -> S.ByteString
            -> C.Conduit S.ByteString IO (Either Param (File y))
parsePieces sink bound = C.sequenceSink True (parsePiecesSink sink bound)

parsePiecesSink :: BackEnd y
                -> S.ByteString
                -> C.SequencedSink Bool S.ByteString IO (Either Param (File y))
parsePiecesSink _ _ False = return C.Stop
parsePiecesSink BackEnd{initialize=initialize',append=append',close=close'}
                bound True = do
    _boundLine <- takeLine
    res' <- takeLines
    case res' of
        [] -> return C.Stop
        _ -> do
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
                    seed <- liftIO initialize'
                    (seed', wasFound) <-
                        sinkTillBound bound append' seed
                    y <- liftIO $ close' seed'
                    let fi = FileInfo filename ct y
                    let y' = (name, fi)
                    return $ C.Emit wasFound [Right y']
                Just (_ct, name, Nothing) -> do
                    let seed = id
                    let iter front bs = return $ front . (:) bs
                    (front, wasFound) <-
                        sinkTillBound bound iter seed
                    let bs = S.concat $ front []
                    let x' = (name, qsDecode bs)
                    return $ C.Emit wasFound [Left x']
                _ -> do
                    -- ignore this part
                    let seed = ()
                        iter () _ = return ()
                    ((), wasFound) <- sinkTillBound bound iter seed
                    return $ C.Emit wasFound []
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
findBound b bs = go [0..S.length bs - 1]
  where
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

sinkTillBound :: S.ByteString
              -> (x -> S.ByteString -> IO x)
              -> x
              -> C.Sink S.ByteString IO (x, Bool)
sinkTillBound bound iter seed0 = C.sinkState
    (id, seed0)
    push
    close'
  where
    close' (front, seed) = do
        seed' <- liftIO $ iter seed $ front S.empty
        return (seed', False)
    push (front, seed) bs' = do
        let bs = front bs'
        case findBound bound bs of
            FoundBound before after -> do
                let before' = killCRLF before
                seed' <- liftIO $ iter seed before'
                return $ C.StateDone (Just after) (seed', True)
            NoBound -> do
                -- don't emit newlines, in case it's part of a bound
                let (toEmit, front') =
                        if not (S8.null bs) && S8.last bs `elem` "\r\n"
                            then let (x, y) = S.splitAt (S.length bs - 2) bs
                                  in (x, S.append y)
                            else (bs, id)
                seed' <- liftIO $ iter seed toEmit
                return $ C.StateProcessing (front', seed')
            PartialBound -> return $ C.StateProcessing (S.append bs, seed)

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
