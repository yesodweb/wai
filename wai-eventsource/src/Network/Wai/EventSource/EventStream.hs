{-# LANGUAGE OverloadedStrings #-}
{- code adapted by Mathias Billman originaly from Chris Smith https://github.com/cdsmith/gloss-web -}

{-|
    A WAI adapter to the HTML5 Server-Sent Events API.  Push-mode and
    pull-mode interfaces are both available.
-}
module Network.Wai.EventSource.EventStream (
    ServerEvent(..),
    eventToBuilder
    ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Data.Monoid

{-|
    Type representing a communication over an event stream.  This can be an
    actual event, a comment, a modification to the retry timer, or a special
    "close" event indicating the server should close the connection.
-}
data ServerEvent
    = ServerEvent {
        eventName :: Maybe Builder,
        eventId   :: Maybe Builder,
        eventData :: [Builder]
        }
    | CommentEvent {
        eventComment :: Builder
        }
    | RetryEvent {
        eventRetry :: Int
        }
    | CloseEvent


{-|
    Newline as a Builder.
-}
nl :: Builder
nl = fromChar '\n'


{-|
    Field names as Builder
-}
nameField, idField, dataField, retryField, commentField :: Builder
nameField = fromString "event:"
idField = fromString "id:"
dataField = fromString "data:"
retryField = fromString "retry:"
commentField = fromChar ':'


{-|
    Wraps the text as a labeled field of an event stream.
-}
field :: Builder -> Builder -> Builder
field l b = l `mappend` b `mappend` nl


{-|
    Appends a buffer flush to the end of a Builder.
-}
flushAfter :: Builder -> Builder
flushAfter b = b `mappend` flush


{-|
    Converts a 'ServerEvent' to its wire representation as specified by the
    @text/event-stream@ content type.
-}
eventToBuilder :: ServerEvent -> Maybe Builder
eventToBuilder (CommentEvent txt) = Just $ flushAfter $ field commentField txt
eventToBuilder (RetryEvent   n)   = Just $ flushAfter $ field retryField (fromShow n)
eventToBuilder (CloseEvent)       = Nothing
eventToBuilder (ServerEvent n i d)= Just $ flushAfter $
    (name n $ evid i $ mconcat (map (field dataField) d)) `mappend` nl
  where
    name Nothing  = id
    name (Just n') = mappend (field nameField n')
    evid Nothing  = id
    evid (Just i') = mappend (field idField   i')
