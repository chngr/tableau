module Tableau where

import Data.List
import Data.Maybe (fromJust)

type Box = (Int,Int)
type Tableau = [[Int]]

-- | Compute the size of a tableau
size :: Tableau -> Int
size = sum . shape

-- | Compute the shape of a tableau
shape :: Tableau -> [Int]
shape = map length

-- | Compute the index of the smallest box containing a given number.
(!?) :: Tableau -> Int -> Maybe Box
t !? n = case findIndices (elem n) t of
           [] -> Nothing
           rs -> let bs = map (\i -> let Just j = elemIndex n (t !! i) in (i,j)) rs
                     b  = minimumBy (\a a' -> compare (snd a) (snd a')) bs
                  in Just b

(.!) :: Tableau -> Box -> Maybe Int
t .! (i,j) = if 0 <= i && i < length t
             && 0 <= j && j < length (t !! i)
               then Just ((t !! i) !! j)
               else Nothing

-- | Given box (i,j) in tableau, compute the next position to slide to
-- based on the given comparison function.
chooseSlideWith :: (Int -> Int -> Ordering) -> Tableau -> Box -> Maybe Box
chooseSlideWith f t (i,j) =
    case (t .! (i,j+1), t .! (i+1,j)) of
      (Nothing, Nothing) -> Nothing         -- Corner
      (Nothing, Just _)  -> Just (i+1,j)    -- None to right
      (Just _,  Nothing) -> Just (i,j+1)    -- None below
      (Just r,  Just b)  -> Just (if f r b == LT then (i,j+1) else (i+1,j))

-- | Slide to the smaller one.
chooseSlide :: Tableau -> Box -> Maybe Box
chooseSlide = chooseSlideWith compare

-- | Given box (i,j) in tableau, compute the next position to reverse slide
-- to based on the given comparision function.
chooseRSlideWith :: (Int -> Int -> Ordering) -> Tableau -> Box -> Maybe Box
chooseRSlideWith f t (i,j) =
    case (t .! (i,j-1), t .! (i-1,j)) of
      (Nothing, Nothing) -> Nothing         -- Corner
      (Nothing, Just _)  -> Just (i-1,j)    -- None left
      (Just _, Nothing)  -> Just (i,j-1)    -- None above
      (Just l, Just a)   -> Just (if f l a == GT then (i,j-1) else (i-1,j))

-- | Reverse slide to the larger one.
chooseRSlide :: Tableau -> Box -> Maybe Box
chooseRSlide = chooseRSlideWith compare

-- | Swap the entry in (i,j) with the entry in (k,l)
swap :: Tableau -> Box -> Box -> Tableau
swap t (i,j) (k,l) = t''
  where t'  = changeEntry t  (i,j) $ fromJust (t .! (k,l))
        t'' = changeEntry t' (k,l) $ fromJust (t .! (i,j))

-- | Change the entry at position i to n
changeRowEntry :: [Int] -> Int -> Int -> [Int]
changeRowEntry r i n =
    foldr (\(e,ix) acc -> if i == ix then n:acc else e:acc) [] (zip r [0..])

-- | Change the entry at (i,j) to n
changeEntry :: Tableau ->  Box -> Int -> Tableau
changeEntry t (i,j) n = map (\(r,ix) -> if ix == i
                                          then changeRowEntry r j n
                                          else r) (zip t [0..])

-- | Extract subtableau with entries at least lo and entries at most hi
subtableau :: (Int,Int) -> Tableau -> Tableau
subtableau (lo, hi) = map (map (\e -> if e < lo then -1 else e) . filter (<= hi))

descents :: Tableau -> [Bool]
descents t = descents' $ map (\k -> (snd . fromJust) (t !? k)) [1..(size t)]
    where descents' :: [Int] -> [Bool]
          descents' []               = []
          descents' [_]              = [False]
          descents' (j:k:xs) = (k <= j):descents' (k:xs)
