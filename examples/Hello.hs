import           Piyo

instance Game () where
    draw _ =
        [ cls Yellow
        , txt Black "Hello, World！" 48 48
        , txt Orange "こんにちは、世界！" 48 96
        ]

main :: IO ()
main = piyo () Nothing
