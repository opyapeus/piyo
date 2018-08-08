module Main where

import           Piyo
import           System.Environment             ( getArgs )
import           Data.Maybe                     ( listToMaybe )


newtype Arg = Arg { unTag :: Maybe String }

instance Game Arg where
    draw s =
        [ cls Yellow
        , txt Black str 48 48
        ]
        where
            str = case unTag s of
                    Just arg -> arg
                    Nothing -> "Hello, World!"

-- print first argument in the game window 
main :: IO ()
main = do
    args <- getArgs
    let s = Arg $ listToMaybe args
    piyo s Nothing
