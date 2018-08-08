-- | Game class.
-- Functions are called in order of update, draw, sound every frame.
module Piyo.Game
    ( Game(..)
    )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           Piyo.Internal.Types

-- | Game class.
-- s is a state that user can flexibly define.
class Game s where
    -- | Update state.
    -- The state next frame is determined by keyboard input and previous state. 
    -- There is no side effect in this function.
    update :: [Input] -> s -> s
    update _ = id

    -- | Draw.
    -- Oprations are determined by state.
    draw :: MonadIO m => s -> [DrawOp m]
    draw _ = []

    -- | Sound.
    -- Oprations are determined by state.
    sound :: MonadIO m => s -> [SoundOp m]
    sound _ = []
