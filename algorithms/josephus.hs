module Josephus where

removeItem [] _ = []
removeItem items i = (take i items) ++ (drop (1 + i) items)

josephusSurvivor :: Int -> Int -> Int
josephusSurvivor n k = go (take n . enumFrom $ 1) k 0
  where go arr k pos =
          case length arr of
            1 -> head arr
            _ -> go nArr k rpos
                 where rpos = mod (pos + k - 1) (length arr)
                       nArr = removeItem arr rpos

