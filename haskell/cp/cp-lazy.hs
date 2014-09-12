--
--  cp-lazy.hs
--  code
--
--  Do a lazy copy of one file to another, without holding the contents in
--  memory.
--

import System.Environment
import System.Directory
import System.IO
import System.Exit
import Control.Exception
import qualified Data.ByteString.Lazy as B

main :: IO()
main = do
    args <- getArgs
    case args of [source, dest] -> copy source dest
                 _              -> usage

usage :: IO()
usage = do
    putStrLn "Usage: cp-lazy <source> <dest>"
    exitFailure

copy :: String -> String -> IO()
copy source dest = do
    contents <- B.readFile source
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            B.hPutStr tempHandle contents
            hClose tempHandle
            renameFile tempName dest)
