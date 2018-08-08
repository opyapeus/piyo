module Piyo.Internal.Types where

import qualified SDL
import qualified SDL.Font
import qualified SDL.Mixer


data Action = KeyInput KeyMotion | Idle | Quit
  deriving Show

data KeyMotion
  = Pressed Input
  | Released Input
  deriving Show

-- | Keyboard input patterns.
data Input = BtnUp | BtnDown | BtnLeft | BtnRight | BtnA | BtnB
  deriving (Show, Eq)

-- | Color patterns.
-- Same as PICO-8 color palette.
data Color
  = Black | DarkBlue | DarkPurple | DarkGreen
  | Brown | DarkGray | LightGray | White
  | Red | Orange | Yellow | Green
  | Blue | Indigo | Pink | Peach
  deriving (Show, Eq)

-- | Asset data for draw and sound.
data Asset = Asset
  { sounds :: [FilePath]
  , image :: Maybe FilePath
  , imap :: ImageMap
  }
  deriving Show

-- | 2d nested list.
-- The value means sprite indices.
type ImageMap = [[(IdX, IdY)]]

data RenderAsset = RenderAsset
  { renderer :: SDL.Renderer
  , font :: Font
  , mImage :: Maybe SDL.Texture
  , imageMap :: ImageMap
  }

data Font = Font
  { plain :: SDL.Font.Font
  , bold :: SDL.Font.Font
  }
  deriving Show

type DrawOp m = RenderAsset -> m ()

type SoundOp m = [SDL.Mixer.Chunk] -> m ()

-- | Posision value.
type X = Int
type Y = Int
-- | Length.
type Width = Int
type Height = Int
type Radius = Int
-- | Index number.
type IdX = Int
type IdY = Int
-- | Count number.
type CntX = Int
type CntY = Int
