import           Piyo

data State = State
    { x :: X
    , y :: Y
    , isCollige :: Bool
    }

instance Game State where
    update inputs state =
        state { x = nnx, y = nny, isCollige = isXWall || isYWall }
        where
            nx = foldr (\i acc -> case i of BtnLeft -> acc - 1; BtnRight -> acc + 1; _ -> acc) (x state) inputs
            ny = foldr (\i acc -> case i of BtnUp -> acc - 1; BtnDown -> acc + 1; _ -> acc) (y state) inputs
            isXWall = isWall nx (y state)
            isYWall = isWall (x state) ny
            nnx = if isXWall then (x state) else nx
            nny = if isYWall then (y state) else ny

    draw state =
        [ cls Black
        , mp 0 0 16 16 0 0
        , spr 1 0 (x state) (y state)
        ]

    sound state =
        if isCollige state
            then [ply 0 0]
            else []

boxSize = 12
block = (0, 0)

isWall :: X -> Y -> Bool
isWall x y = foldr (\p acc -> acc || check p) False points
  where
    m      = imap asset
    lx     = dot2box x
    rx     = dot2box (x + boxSize - 1)
    ty     = dot2box y
    by     = dot2box (y + boxSize - 1)
    points = [(lx, ty), (lx, by), (rx, ty), (rx, by)]
    check (px, py) = case findMap m px py of
        Nothing -> False
        Just pnt@(idx, idy) | pnt == block -> True
                            | otherwise    -> False

dot2box :: Int -> Int
dot2box dot = div dot boxSize

findMap :: ImageMap -> IdX -> IdY -> Maybe (IdX, IdY)
findMap m idx idy = case safeIndex idy m >>= safeIndex idx of
    [(idx', idy')] -> Just (idx', idy')
    _              -> Nothing

safeIndex :: Int -> [a] -> [a]
safeIndex i = take 1 . drop i

asset :: Asset
asset = Asset
    { image  = Just "assets/sprite.png"
    , imap   = field
    , sounds = ["assets/beep.mid"]
    }
    -- make fenced field (16x16)
  where
    empty = (0, 1)
    line  = replicate 16 block
    edge  = [block] ++ replicate 14 empty ++ [block]
    field = [line] ++ replicate 14 edge ++ [line]

initialState = State {x = 96, y = 96, isCollige = False}

main :: IO ()
main = piyo initialState (Just asset)
