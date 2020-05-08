module TupleFunctions where

addEmUp2 :: Num a => (a,a) -> a
addEmUp2 (x, y) = x + y

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))