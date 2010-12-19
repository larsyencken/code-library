--
--  file_sum.hs
--
--  Prints the sum of all integers in the given file.
--

import Text.Printf
import System.IO
import System.Environment

main :: IO()
main = do 
    [inputFile] <- getArgs
    x <- readFile inputFile
    printf "%d\n" (sum (map readInt (lines x)))
    where
        readInt = read::(String -> Int)
