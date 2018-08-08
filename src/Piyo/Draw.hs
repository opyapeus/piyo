-- | Draw functions.
-- Last two arguments -> X -> Y is the drawing origin.
module Piyo.Draw
    ( cls
    , dot
    , line
    , rect
    , rectfill
    , circ
    , circfill
    , tri
    , trifill
    , spr
    , sprn
    , sprc
    , txt
    , txtb
    , mp
    )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad
import           Foreign.C.Types                ( CInt )
import           SDL                            ( ($=) )
import qualified SDL
import qualified SDL.Primitive
import qualified SDL.Image
import qualified SDL.Font
import           Piyo.Internal.Types
import           Piyo.Internal.Constants
import           Data.Word                      ( Word8 )
import           Data.Text                      ( pack )


-- | Clear the screen with given color.
cls :: MonadIO m => Color -> DrawOp m
cls c ra = setColor c ra >> SDL.clear (renderer ra)

-- | Draw rectangle with given color.
-- Origin is left-top.
rect :: MonadIO m => Color -> Width -> Height -> X -> Y -> DrawOp m
rect = rectF SDL.Primitive.rectangle

rectfill :: MonadIO m => Color -> Width -> Height -> X -> Y -> DrawOp m
rectfill = rectF SDL.Primitive.fillRectangle

rectF f c w h x y = \ra -> f (renderer ra) pa pb col
  where
    pa  = makePos x y
    pb  = makePos (x + w) (y + h)
    col = colorCode c

dot :: MonadIO m => Color -> X -> Y -> DrawOp m
dot c x y = \ra -> SDL.Primitive.pixel (renderer ra) p col
  where
    p   = makePos x y
    col = colorCode c

line :: MonadIO m => Color -> X -> Y -> X -> Y -> DrawOp m
line c xa ya xb yb = \ra -> SDL.Primitive.line (renderer ra) pa pb col
  where
    pa  = makePos xa ya
    pb  = makePos xb yb
    col = colorCode c

-- | Draw circle with given color.
-- Origin is center.
circ :: MonadIO m => Color -> Radius -> X -> Y -> DrawOp m
circ = circF SDL.Primitive.circle

circfill :: MonadIO m => Color -> Radius -> X -> Y -> DrawOp m
circfill = circF SDL.Primitive.fillCircle

circF f c r x y = \ra -> f (renderer ra) p r' col
  where
    p   = makePos x y
    r'  = fromIntegral r
    col = colorCode c

tri :: MonadIO m => Color -> X -> Y -> X -> Y -> X -> Y -> DrawOp m
tri = triF SDL.Primitive.triangle

trifill :: MonadIO m => Color -> X -> Y -> X -> Y -> X -> Y -> DrawOp m
trifill = triF SDL.Primitive.fillTriangle

triF f c xa ya xb yb xc yc = \ra -> f (renderer ra) pa pb pc col
  where
    pa  = makePos xa ya
    pb  = makePos xb yb
    pc  = makePos xc yc
    col = colorCode c

-- | Draw sprite.
-- Origin is left-top.
spr :: MonadIO m => IdX -> IdY -> X -> Y -> DrawOp m
spr xId yId = sprn xId yId 1 1

-- | Draw sprite for large size.
sprn :: MonadIO m => IdX -> IdY -> CntX -> CntY -> X -> Y -> DrawOp m
sprn xId yId bw bh = sprc x' y' w h
  where
    x' = boxSize * xId
    y' = boxSize * yId
    w  = boxSize * bw
    h  = boxSize * bh

-- | Draw sprite for any size.
sprc :: MonadIO m => X -> Y -> Width -> Height -> X -> Y -> DrawOp m
sprc xs ys w h xt yt ra = case mTxtr of
    Just txtr -> SDL.copy rdr txtr (Just rs) (Just rt)
    Nothing   -> return ()
  where
    mTxtr = mImage ra
    rdr   = renderer ra
    rs    = makeRect xs ys w h
    rt    = makeRect xt yt w h

-- | Draw text with given color.
-- Font is JF-Dot-K12.
-- Origin is left-top.
txt :: MonadIO m => Color -> String -> X -> Y -> DrawOp m
txt = txtF plain

-- | Draw bold text with given color.
-- Font is JF-Dot-K12B.
-- Origin is left-top.
txtb :: MonadIO m => Color -> String -> X -> Y -> DrawOp m
txtb = txtF bold

txtF f c t x y ra = do
    sfc    <- SDL.Font.solid font' col t'
    txtr   <- SDL.createTextureFromSurface rdr sfc
    (w, h) <- SDL.Font.size font' t'
    let rt = makeRect x y w h in SDL.copy rdr txtr Nothing (Just rt)
    SDL.freeSurface sfc
    SDL.destroyTexture txtr
  where
    t'    = pack t
    font' = f $ font ra
    rdr   = renderer ra
    col   = colorCode c

-- | Draw map (a set of sprites).
-- Origin is left-top.
mp :: MonadIO m => IdX -> IdY -> CntX -> CntY -> X -> Y -> DrawOp m
mp mx my mw mh x y ra = forM_
    im
    (\(vrt, il) -> forM_
        il
        (\(hr, (nx, ny)) -> do
            let w  = boxSize
                h  = boxSize
                xs = w * nx
                ys = h * ny
                xt = x + w * (mx + hr)
                yt = y + h * (my + vrt)
            unless (hr > mw || vrt > mh) $ sprc xs ys w h xt yt ra
        )
    )
  where
    m  = imageMap ra
    l  = zip [0 ..]
    im = l $ (map l) m


setColor :: MonadIO m => Color -> DrawOp m
setColor c ra = SDL.rendererDrawColor (renderer ra) $= colorCode c

colorCode :: Color -> SDL.V4 Word8
colorCode Black      = SDL.V4 0 0 0 maxBound
colorCode DarkBlue   = SDL.V4 29 43 83 maxBound
colorCode DarkPurple = SDL.V4 126 37 83 maxBound
colorCode DarkGreen  = SDL.V4 0 135 81 maxBound
colorCode Brown      = SDL.V4 171 82 54 maxBound
colorCode DarkGray   = SDL.V4 95 87 79 maxBound
colorCode LightGray  = SDL.V4 194 195 199 maxBound
colorCode White      = SDL.V4 maxBound 241 232 maxBound
colorCode Red        = SDL.V4 maxBound 0 77 maxBound
colorCode Orange     = SDL.V4 maxBound 163 0 maxBound
colorCode Yellow     = SDL.V4 maxBound 236 39 maxBound
colorCode Green      = SDL.V4 0 228 54 maxBound
colorCode Blue       = SDL.V4 41 173 maxBound maxBound
colorCode Indigo     = SDL.V4 131 118 156 maxBound
colorCode Pink       = SDL.V4 maxBound 119 168 maxBound
colorCode Peach      = SDL.V4 maxBound 204 170 maxBound

makePos :: Int -> Int -> SDL.V2 CInt
makePos x y = SDL.V2 x' y' where (x', y') = (fromIntegral x, fromIntegral y)

makeRect :: Int -> Int -> Int -> Int -> SDL.Rectangle CInt
makeRect x y w h = SDL.Rectangle o z
  where
    (x', y', w', h') =
        (fromIntegral x, fromIntegral y, fromIntegral w, fromIntegral h)
    o = SDL.P (SDL.V2 x' y')
    z = SDL.V2 w' h'
