module enumAdd where

(+*) :: Enum a => a -> Int -> a
(+*) x 0 = x
(+*) x y = (succ x) +* (y - 1) 