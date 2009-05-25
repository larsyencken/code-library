--
-- hello.hs
--
-- A simple hello world program.
--

main = do putStrLn "What's your name?"
          name <- getLine
          putStr ("Hello, " ++ name ++ "!\n")
