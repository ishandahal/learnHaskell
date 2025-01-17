doubleUs x y = doubleMe x + doubleMe y

doubleMe x = x + x

doubleSmallNumber x =
  if x < 100
    then x + x
    else x

doubleSmallNumber' :: Int -> Int
doubleSmallNumber' x =
  ( if x < 100
      then x + x
      else x
  )
    + 1

-- ====================== --
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number 7!"
lucky x = "Sorry, you are out of luck Pal!"

-- =======================
factorial :: (Integral a) => a -> a
factorial n = case n of
  0 -> 1
  n -> n * (factorial n - 1)

-- factorial :: Integer -> Integer
-- factorial n = case n of
--   0 -> 1
--   _ -> n * factorial (n - 1)

-- factorial 0 = 1
-- factorial n = n * factorial (n - 1)

-- ========================
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"

-- ========================
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors x y = (fst x + fst y, snd x + snd y)
addVectors (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

-- ========================
head' :: [a] -> a
head' [] = error "Error! Can't call head on empty list."
head' (a : _) = a

-- ========================
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- ========================
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You are underweight!"
  | bmi <= 25.0 = "You are normal."
  | bmi <= 30.0 = "You are overweight."
  | otherwise = "You are a whale."

-- =========================
max' :: (Ord a) => a -> a -> a
max' x y
  | x < y = y
  | otherwise = x

-- ==========================
myCompare :: (Ord a) => a -> a -> Ordering
x `myCompare` y
  | x < y = LT
  | x > y = GT
  | otherwise = EQ

-- ==========================
capital :: String -> String
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- ========================
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' height weight
  | bmi <= 18.5 = "You are underweight!"
  | bmi <= 25.0 = "You are normal."
  | bmi <= 30.0 = "You are overweight."
  | otherwise = "You are a whale."
  where
    bmi = weight / height ^ 2

-- ========================
initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstName
    (l : _) = lastName

-- ========================
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- ========================
calcBmi :: (RealFloat a) => [(a, a)] -> [a]
calcBmi xs = [bmi | (h, w) <- xs, let bmi = w / h ^ 2, bmi >= 35]

-- =======================
describeList :: [a] -> String
describeList xs = case xs of
  [] -> "Empty list"
  [x] -> "Singleton list."
  _ -> "Long list"

-- ========================
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Can't call maximum' on empty list"
maximum' [x] = x
-- maximum' (x : xs)
--   | x > maxRest = x
--   | otherwise = maxRest
--   where
--     maxRest = maximum' xs
maximum' (x : xs) = max x (maximum' xs)

-- ========================
replicate' :: (Ord i, Num i) => i -> a -> [a]
-- replicate' 0 _ = []
replicate' n elem
  | n <= 0 = []
  | otherwise = elem : replicate' (n - 1) elem

-- ========================
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- =======================
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

-- =======================
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- =======================
multiply' :: (Num a) => a -> a -> a -> a
multiply' x y z = x * y * z

-- takes a number and returns a function that takes a number and returns a number (total product)
multiplyWith10 :: (Num a) => a -> a -> a
multiplyWith10 y = multiply' y 10
