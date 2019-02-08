-- Example where translation with sharing is necessary to
-- implement call-time choice

xor :: Bool -> Bool -> Bool
xor False x     = x
xor True  x = xor' x

xor' :: Bool -> Bool
xor' False = True
xor' True  = False

xorSelf :: Bool -> Bool
xorSelf x = xor x x

main :: Bool
main = xorSelf (False ? True)

-- Synthesize set function `mainS` with:
-- > synsetfun Xor -f main
