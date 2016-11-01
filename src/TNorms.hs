module TNorms
    ( luk
    , prod
    , drast
    , nilpotent
    , hamacher
    ) where

luk :: (Num a, Ord a) => a -> a -> a
luk a b = max 0 (a + b - 1)

prod :: (Num a) => a -> a -> a
prod = (*)

drast :: (Num a, Eq a) => a -> a -> a
drast a b | a == 1 = b
          | b == 1 = a
          | otherwise = 0

nilpotent :: (Num a, Ord a) => a -> a -> a
nilpotent a b | a + b > 1 = min a b
              | otherwise = 0

hamacher :: (Fractional a, Ord a) => a -> a -> a
hamacher a b | a == 0 && b == 0 = 0
             | otherwise = a * b / (a + b - a * b)
