module Piyo.Internal.Common
    ( withSDL
    , withWindow
    , withRenderer
    , withSound
    , withImage
    , withFont
    )
where

import           Paths_piyo
import qualified SDL
import           SDL                            ( ($=) )
import qualified SDL.Font
import qualified SDL.Mixer
import qualified SDL.Image
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Traversable               ( forM )
import           Data.Foldable                  ( forM_ )
import           Piyo.Internal.Types
import           Piyo.Internal.Constants
import           Piyo.Internal.Config
import           Piyo.Game


withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
    SDL.initialize []
    void op
    SDL.quit

withWindow :: (MonadIO m) => (SDL.Window -> m a) -> m ()
withWindow op = do
    w <- SDL.createWindow title windowConfig
    SDL.windowMinimumSize w $= SDL.windowInitialSize windowConfig
    SDL.showWindow w
    void $ op w
    SDL.destroyWindow w

withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
    r  <- SDL.createRenderer w (-1) rendererConfig
    ws <- SDL.get $ SDL.windowSize w
    SDL.rendererLogicalSize r $= Just ws
    void $ op r
    SDL.destroyRenderer r

withSound :: [FilePath] -> SoundOp IO -> IO ()
withSound []     op = void $ op []
withSound sounds op = SDL.Mixer.withAudio audioConfig chunkSize $ do
    SDL.Mixer.initialize audioFlugs
    chnks <- forM sounds SDL.Mixer.load
    void $ op chnks
    forM_ chnks SDL.Mixer.free
    SDL.Mixer.quit

withImage
    :: MonadIO m
    => Maybe FilePath
    -> SDL.Renderer
    -> (Maybe SDL.Texture -> m ())
    -> m ()
withImage (Just path) r op = do
    txtr <- SDL.Image.loadTexture r path
    void $ op (Just txtr)
    SDL.destroyTexture txtr
withImage Nothing r op = void $ op Nothing

withFont :: (Font -> IO ()) -> IO ()
withFont op = do
    SDL.Font.initialize
    fontPath'  <- Paths_piyo.getDataFileName fontPath
    fontBPath' <- Paths_piyo.getDataFileName fontBPath
    fontPlain  <- SDL.Font.load fontPath' fontSize
    fontBold   <- SDL.Font.load fontBPath' fontSize
    void $ op $ Font {plain = fontPlain, bold = fontBold}
    SDL.Font.free fontPlain
    SDL.Font.free fontBold
    SDL.Font.quit
