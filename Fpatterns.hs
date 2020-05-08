bindExp :: Integer -> String
-- Shadowing of parameters
bindExp x = let x = 10 in "x: " ++ (show x)

-- Anonymous funcs exercises

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

funcZ x = case x + 1 == 1 of
  True -> "AWESOME"
  False -> "wut"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"


pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Guards

myAbs x
  | x < 0     = (-x)
  | otherwise =   x