-- 
-- fact.hs
--
-- Simple factorial functions.
--

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n > 0 = n * factorial (n - 1)

factorial2 :: Integer -> Integer
factorial2 n = product (enumFromTo 1 n)

factorial3 :: Integer -> Integer
factorial3 = product . enumFromTo 1
