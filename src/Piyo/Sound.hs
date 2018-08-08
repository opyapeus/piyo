-- | Sound functions.
-- It may be more development.
module Piyo.Sound
    ( ply
    , hlt
    , hltall
    )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad
import qualified SDL.Mixer
import           Piyo.Internal.Types
import           Piyo.Internal.Config


-- | Play sound on given channel.
-- Specify the sound by given index.
-- If the channel is already taken, halt previous then play.
ply :: MonadIO m => SDL.Mixer.Channel -> Int -> SoundOp m
ply ch n []    = return ()
ply ch n chnks = do
    hlt ch chnks
    forM_ chnk (SDL.Mixer.playOn ch SDL.Mixer.Once)
    where chnk = sliceOne n chnks

-- | Halt sound on given channel.
-- If the channel is already halted, do nothing.
hlt :: MonadIO m => SDL.Mixer.Channel -> SoundOp m
hlt ch [] = return ()
hlt ch _  = do
    isPlaying <- SDL.Mixer.playing ch
    when isPlaying $ SDL.Mixer.halt ch

-- | Halt sounds on all channels.
hltall :: MonadIO m => SoundOp m
hltall = return $ SDL.Mixer.halt SDL.Mixer.AllChannels

sliceOne i xs = take 1 $ drop i xs

