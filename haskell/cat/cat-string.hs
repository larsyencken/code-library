--
-- cat.hs
--
-- A Haskell implementation of the cat command.
--

module Main where

import System.IO

main = do
    contents <- getContents
    putStr contents
