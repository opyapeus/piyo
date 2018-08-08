module Piyo.Internal.Config
    ( windowConfig
    , rendererConfig
    , audioConfig
    , audioFlugs
    )
where

import qualified SDL
import qualified SDL.Mixer
import Piyo.Internal.Constants


windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow
    { SDL.windowInitialSize = makeV2 windowSize
    , SDL.windowResizable   = True
    }
    where
        makeV2 (x, y) = SDL.V2 (fromIntegral x) (fromIntegral y)

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
    { SDL.rendererType          = SDL.AcceleratedVSyncRenderer
    , SDL.rendererTargetTexture = False
    }

audioConfig :: SDL.Mixer.Audio
audioConfig = SDL.Mixer.Audio
    { SDL.Mixer.audioFrequency = sampleFrequency
    , SDL.Mixer.audioFormat    = SDL.Mixer.FormatS16_Sys -- NOTE: output sample format
    , SDL.Mixer.audioOutput    = SDL.Mixer.Stereo
    }

audioFlugs :: [SDL.Mixer.InitFlag]
audioFlugs =
    [ SDL.Mixer.InitMOD
    , SDL.Mixer.InitOGG
    -- , SDL.Mixer.InitMP3
    -- , SDL.Mixer.InitFLAC
    ]

