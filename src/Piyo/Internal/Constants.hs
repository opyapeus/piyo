{-# LANGUAGE OverloadedStrings #-}

module Piyo.Internal.Constants
    ( windowSize
    , title
    , fontPath
    , fontBPath
    , fontSize
    , boxSize
    , chunkSize
    , sampleFrequency
    )
where

import           Data.Text


windowSize :: (Int, Int)
windowSize = (192, 192)

title :: Text
title = "PIYO"

fontPath :: FilePath
fontPath = "fonts/JF-Dot-K12.ttf"

fontBPath :: FilePath
fontBPath = "fonts/JF-Dot-K12B.ttf"

fontSize :: Int
fontSize = 12

boxSize :: Int
boxSize = 12

-- TODO: adjust
chunkSize :: Int
chunkSize = 16

sampleFrequency :: Int
sampleFrequency = 44100
