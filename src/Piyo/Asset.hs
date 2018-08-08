-- | Asset for draw and sound.
module Piyo.Asset
    ( defaultAsset
    )
where

import           Piyo.Internal.Types

-- | Empty asset.
defaultAsset :: Asset
defaultAsset = Asset {sounds = [], image = Nothing, imap = []}
