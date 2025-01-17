multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z

compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred = compare 100

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--   where
--     g x y = f y x
flip' f x y = f y x

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let smallerSorted = quickSort (filter (<= x) xs)
      largerSorted = quickSort (filter (> x) xs)
   in smallerSorted ++ [x] ++ largerSorted

largestDivisible :: Int
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = (x `mod` 3829) == 0

sumOddSquares :: Int
sumOddSquares = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | odd n = n : chain ((n * 3) + 1)
  | otherwise = n : chain (div n 2)

numLongChains :: Int
numLongChains = length (filter (\x -> length x > 15) (map chain [1 .. 100]))

-- map' :: (a -> b) -> [a] -> [b]
-- map' f xs = foldr (\x acc -> f x : acc) [] xs
