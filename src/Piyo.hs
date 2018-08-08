{-|
Module      : Piyo
Description : Haskell game engine like fantasy console.
Copyright   : (c) peus, 2018
License     : MIT
Maintainer  : opyapeus@gmail.com
Stability   : experimental
Portability : POSIX

Please see the README on GitHub at <https://github.com/opyapeus/piyo#readme>

Minimal working code is below. 

@
import Piyo

instance Game () where
    draw _ =
        [ cls Yellow
        , txt Black "Hello, World！" 48 48
        ]

main :: IO ()
main = piyo () Nothing
@

-}
module Piyo
  ( piyo
  , module Piyo.Game
  , module Piyo.Asset
  , module Piyo.Types
  , module Piyo.Draw
  , module Piyo.Sound
  )
where

import           Control.Monad.Extra            ( loopM )
import qualified SDL
import           Data.IORef
import           Data.Foldable                  ( toList )
import           Piyo.Internal.Common
import           Piyo.Internal.InputState
import           Piyo.Internal.Types
import           Piyo.Game
import           Piyo.Asset
import           Piyo.Types
import           Piyo.Draw
import           Piyo.Sound

-- | Start up game function.
-- First argument is state that user can flexibly define.
-- Second argument is asset (sprite, map, sound).
piyo :: Game s => s -> Maybe Asset -> IO ()
piyo initialState mAsset =
  withSDL
    $ withWindow
    $ \w -> withRenderer w $ \r ->
        withSound snds $ \chnks -> withImage img r $ \mTxtr ->
          withFont $ \font -> do
            let ra = RenderAsset r font mTxtr mapData

            -- clear screen
            cls Black ra
            -- init inputsRef
            inputsRef <- newIORef []

            loopM
              (\s -> do
                mEvent <- SDL.pollEvent
                let act = mkAction mEvent

                case act of
                  KeyInput km -> modifyIORef inputsRef (updateInputs km) -- update inputsRef
                  _           -> return ()
                -- read inputsRef
                inputs <- readIORef inputsRef

                case act of
                  Quit -> return $ Right ()
                  _    -> do
                    let ns       = update inputs s
                        drawOps  = draw ns
                        soundOps = sound ns
                    sequence_ $ drawOps <*> [ra]
                    sequence_ $ soundOps <*> [chnks]
                    SDL.present r
                    return $ Left ns
              )
              initialState
 where
  img     = mAsset >>= image
  snds    = toList mAsset >>= sounds
  mapData = toList mAsset >>= imap
