module WaiAppStatic.Storage.Embedded(
    -- * Basic
      embeddedSettings

    -- * Template Haskell
    , Etag
    , EmbeddableEntry(..)
    , mkSettings
    ) where

import WaiAppStatic.Storage.Embedded.Runtime
import WaiAppStatic.Storage.Embedded.TH
