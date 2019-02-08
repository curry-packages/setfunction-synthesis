anyOf :: [Int] -> Int
anyOf (x : xs) = x ? anyOf xs

-- Synthesize set function `anyOfS` with:
-- > synsetfun Examples -f anyOf


ndconst :: Int -> Int -> Int
ndconst x y = x ? 1

-- Synthesize set function `ndconstS` with:
-- > synsetfun Examples -f ndconst
