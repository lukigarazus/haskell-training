import Data.List (intersperse)

fact x =
  case x of
    0 -> 1
    _ -> x * fact (x - 1)

fibo n =
  case n of
    0 -> 0
    1 -> 1
    _ -> fibo (n - 1) + fibo (n - 2)

divide :: Integer -> Integer -> Integer
divide x y
  | x < y = 0
  | x == y = 1
  | x > y = 1 + divide (x - y) y

weird a b = go a b 0
  where go a b c
          | a < b = c
          | otherwise = a

multi x y
  | y == 0    = 0
  | y == 1    = x
  | otherwise = x + multi x (y - 1)

summy n
  | n == 0    = 0
  | otherwise = n + summy (n - 1)

mc x
  | x > 100   = x - 10
  | otherwise = mc $ mc (x + 11)

digitToStr d =
  case d of
    0 -> "Zero"
    1 -> "One"
    2 -> "Two"
    3 -> "Three"
    4 -> "Four"
    5 -> "Five"
    6 -> "Six"
    7 -> "Seven"
    8 -> "Eight"
    9 -> "Nine"
    _ -> "Dupa"

digits :: Integer -> [Integer]
digits x = go x 10
  where go x s
          | x < 10    = x :[]
          | otherwise = (go divided s) ++ (mr :[])
          where mr      = mod x s
                diff    = x - mr
                divided = floor $ realToFrac diff / 10

intToStr x = concat . intersperse "-" . map digitToStr . digits $ x