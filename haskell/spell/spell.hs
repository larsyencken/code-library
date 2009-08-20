--
-- spell.hs
--
-- Basic spellchecker. 
--

import Data.Set hiding (map)
import Data.Maybe
import Data.Char
import Text.Printf
import System.IO
import System.Environment
import Control.Monad
import Control.Concurrent

main = do
    (f, g, n) <- readFiles
    let dict = fromList (lines f)
        work = chunk n (words g)
    run n dict work

chunk n list = case list of { [] -> [] ; (y:ys) -> ch' ys (n-1) (y:) }
    where
    ch' [] _ k = k [] : []
    ch' (y:ys) 0 k = k [] : ch' ys (n-1) (y:)
    ch' (y:ys) (c+1) k = ch' ys c (k . (y:))

readFiles = do
    [s, n] <- getArgs
    f <- readFile "/usr/share/dict/words"
    g <- readFile s
    return (f, g, read n)

--write errs = do
--    (t, h) <- mkstemp "/tmp/spell.XXXXXX"
--    mapM_ (hPutStrLn h) errs
--    hClose h
--    printf "%d spelling errors written to '%s'\n" (length errs) t

run n dict work = do
    chan <- newChan
    errs <- getChanContents chan
    mapM_ (forkIO . thread chan dict) (zip [1..n] work)
    wait n errs 0

wait n xs i = when (i < n) $ case xs of
    Nothing : ys -> wait n ys $! i+1
    Just s  : ys -> putStrLn s >> wait n ys i

thread chan dict (me, xs) = do
    mapM_ spellit xs
    writeChan chan Nothing

    where
        spellit w = when (w `notMember` dict) $
            writeChan chan . Just $ printf "Thread %d: %-25s" (me::Int) w

