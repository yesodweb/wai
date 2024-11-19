module Network.Wai.Handler.Warp.Imports (
    ByteString (..),
    NonEmpty (..),
    module Control.Applicative,
    module Control.Monad,
    module Data.Bits,
    module Data.Int,
    module Data.Monoid,
    module Data.Ord,
    module Data.Word,
    module Data.Maybe,
    module Numeric,
    throughAsync,
    isAsyncException,
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import Data.ByteString.Internal (ByteString (..))
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Word
import Numeric

isAsyncException :: Exception e => e -> Bool
isAsyncException e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> True
        Nothing -> False

throughAsync :: IO a -> SomeException -> IO a
throughAsync action (SomeException e)
    | isAsyncException e = throwIO e
    | otherwise = action
