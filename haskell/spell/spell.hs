--
-- spell.hs
--
-- Basic spellchecker. 
--

import System.Environment
import System.IO
import System.Posix.Temp
import Control.Monad
import Data.Set
import Data.Maybe
import Text.Printf

main = do
    (f, g) <- readFiles
    let dict = fromList (lines f)
        errs = mapMaybe (spell dict) (words g)
    write errs

readFiles = do
    [s] <- getArgs
    f <- readFile "/usr/share/dict/words"
    g <- readFile s
    return (f, g)

write errs = do
    (t, h) <- mkstemp "/tmp/spell.XXXXXX"
    mapM_ (hPutStrLn h) errs
    hClose h
    printf "%d spelling errors written to '%s'\n" (length errs) t

spell d w
    | w `notMember` d   = Just w
    | otherwise         = Nothing
