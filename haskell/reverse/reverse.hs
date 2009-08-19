--
-- reverse.hs
--

main = interact (unlines . map reverse . lines)
