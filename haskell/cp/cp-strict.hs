--
--  cp-strict.hs
--
--  Do a strict copy of one file to another, pulling the entire contents into
--  memory.
--

import Control.Exception
import System.Directory
import System.Environment
import System.Exit
import System.IO

main :: IO()
main = do
    args <- getArgs
    case args of [source, dest] -> copy source dest
                 _              -> usage

usage :: IO()
usage = do
    putStrLn "Usage: cp-strict <source> <dest>"
    exitFailure

copy :: String -> String -> IO()
copy source dest = do
    contents <- readFile source
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle contents
            hClose tempHandle
            renameFile tempName dest)
