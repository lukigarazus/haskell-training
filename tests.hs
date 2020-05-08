import Data.Char

alphaOrd l =
  case l of
    'a' -> 1
    l  -> 1 + (alphaOrd (pred l))

intToAlpha x =
  case (mod x 27) + 1 of
    0 -> ' '
    1 -> 'a'
    x -> succ . intToAlpha $ x - 1

sumAlpha x = intToAlpha . sum . map alphaOrd $ x 