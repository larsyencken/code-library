--
-- wc.hs
--

main = interact (count . lines)

count s = show (length s) ++ "\n"
