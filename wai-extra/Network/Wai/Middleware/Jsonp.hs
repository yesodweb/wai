{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.Jsonp
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic wrapping of JSON responses to convert into JSONP.
--
---------------------------------------------------------
module Network.Wai.Middleware.Jsonp (jsonp) where

import Network.Wai
import Network.Wai.Internal
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Blaze.ByteString.Builder (Builder, copyByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.Monoid (mappend)
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as S
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.CaseInsensitive (CI)
import Network.HTTP.Types (Status)

-- | Wrap json responses in a jsonp callback.
--
-- Basically, if the user requested a \"text\/javascript\" and supplied a
-- \"callback\" GET parameter, ask the application for an
-- \"application/json\" response, then convert that into a JSONP response,
-- having a content type of \"text\/javascript\" and calling the specified
-- callback function.
jsonp :: Middleware
jsonp app env = do
    let accept = fromMaybe B8.empty $ lookup "Accept" $ requestHeaders env
    let callback :: Maybe B8.ByteString
        callback =
            if B8.pack "text/javascript" `B8.isInfixOf` accept
                then join $ lookup "callback" $ queryString env
                else Nothing
    let env' =
            case callback of
                Nothing -> env
                Just _ -> env
                        { requestHeaders = changeVal "Accept"
                                           "application/json"
                                           $ requestHeaders env
                        }
    res <- app env'
    return $ case callback of
        Nothing -> res
        Just c -> go c res
  where
    go c r@(ResponseBuilder s hs b) =
        case checkJSON hs of
            Nothing -> r
            Just hs' -> ResponseBuilder s hs' $
                copyByteString c
                `mappend` fromChar '('
                `mappend` b
                `mappend` fromChar ')'
    go c r =
        case checkJSON hs of
            Just hs' -> addCallback c s hs' wb
            Nothing -> r
      where
        (s, hs, wb) = responseToSource r

    checkJSON hs =
        case lookup "Content-Type" hs of
            Just x
                | B8.pack "application/json" `S.isPrefixOf` x ->
                    Just $ fixHeaders hs
            _ -> Nothing
    fixHeaders = changeVal "Content-Type" "text/javascript"

    addCallback :: ByteString
                -> Status
                -> [(CI ByteString, ByteString)]
                -> (forall b. WithSource IO (C.Flush Builder) b)
                -> Response
    addCallback cb s hs wb =
        ResponseSource s hs $ \f -> wb $ \b -> f $
            C.yield (C.Chunk $ copyByteString cb `mappend` fromChar '(')
            `mappend` b
            `mappend` C.yield (C.Chunk $ fromChar ')')

changeVal :: Eq a
          => a
          -> ByteString
          -> [(a, ByteString)]
          -> [(a, ByteString)]
changeVal key val old = (key, val)
                      : filter (\(k, _) -> k /= key) old
