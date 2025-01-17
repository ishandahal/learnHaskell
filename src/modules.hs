import Data.List (nub)
import Data.Map qualified as M

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub
