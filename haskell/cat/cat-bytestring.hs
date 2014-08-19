module Main where

import qualified Data.ByteString.Lazy as B

main = do
    contents <- B.getContents
    B.putStr contents
