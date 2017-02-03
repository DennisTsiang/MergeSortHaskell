import Data.List

mergeSort:: [Int] -> [Int]
mergeSort []
  = []
mergeSort [x]
  = [x]
mergeSort list@(x:xs)
  = merge (mergeSort (take half list)) (mergeSort (drop half list))
    where
      half = (length list) `div` 2


merge xs []
  = xs
merge [] ys
  = ys
merge (x:xs) (y:ys)
  | x <= y = x: (merge xs (y:ys))
  | y < x = y: (merge (x:xs) ys)
  