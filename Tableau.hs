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

-- | Compute the promotion of a given tableau
promotion :: Tableau -> Tableau
promotion t = fst $ promotion' (0,0) [] t

-- | Compute the promotion path of a given tableau
promotionPath :: Tableau -> [Box]
promotionPath t = (0,0):(reverse $ snd $ promotion' (0,0) [] t)

-- | Compute the orbit of promotion
promotionOrbit :: Tableau -> [Tableau]
promotionOrbit t = nub $ take (n * m)  $ iterate promotion t
  where n = length t
        m = length (t !! 0)

-- | Recursive procedure to compute each step of promtion; also compute the
-- promotion path
promotion' :: Box -> [Box] -> Tableau -> (Tableau, [Box])
promotion' (i,j) path t =
    case chooseSlide t (i,j) of
      Nothing -> (changeEntry (decrAll t) (i,j) (size t), path)
      Just s  -> promotion' s (s:path) $ swap t (i,j) s
  where decrAll = map (map (\n -> n - 1))

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

minOrbit :: Int -> [Int] -> Tableau
minOrbit m ws =
    map (\(ls,rs) -> ls ++ (tail . dropWhile (== -1)) rs) (zip left right)
  where n = length ws
        left = mkLeft
             $ foldr (\(w,k) acc ->
                       (replicate k 0 ++ [w]):acc)
                     []
                     (zip (reverse ws) [n - 1, n - 2..])
        right = mkRight
              $ foldr (\(w,k) acc ->
                        (replicate k (-1) ++ (w + n * (m - 1)):replicate (m - k - 1) 0):acc)
                      []
                      (zip (reverse ws) [n - 1, n - 2..])
        mkRight t =
          case findCorner' t of
            Nothing -> t
            Just b  ->
              let (t',b') = slideWith chooseRSlide t b
                  t''     = changeEntry t' b' $ fromJust (t .! b') - n
              in mkRight t''
        mkLeft t = case findCorner t of
                     Nothing -> t
                     Just b  ->
                       let (t',b') = slideWith chooseSlide t b
                           t''     = changeEntry t' b' $ fromJust (t .! b') + n
                        in mkLeft t''

minOrbitTri :: [Int] -> Tableau
minOrbitTri ws = standardize
               $ mkLeft
               $ foldr (\(w,k) acc ->
                        (replicate k 0 ++ [w]):acc)
                       [] (zip (reverse ws) [n - 1, n - 2..])
  where n = length ws
        mkLeft t = case findCorner t of
                     Nothing -> t
                     Just b  ->
                       let (t',b') = slideWith chooseSlide t b
                           t''     = changeEntry t' b' $ fromJust (t .! b') + n
                        in mkLeft t''

-- | Given a semistandard tableau, compute the standard tableau
-- corresponding to the filling.
standardize :: Tableau -> Tableau
standardize t = fst $ foldl (\(t',n) b -> (changeEntry t' b n, n + 1)) (t,1) bs
    where bs = sortBoxes t

-- | Given tableau, return list of box coordinates in increasing order
-- based on their entry, with column coordinate being the tie breaker.
sortBoxes :: Tableau -> [Box]
sortBoxes t = map snd $
    sortBy (\(e,(i,j)) (e',(i',j')) ->
            if e == e'
              then if j == j'
                     then compare i i'
                     else compare j j'
              else compare e e') bs
  where bs = concat $
             map (\(r,i) -> map (\(e,j) -> (e,(i,j))) (zip r [0..])) (zip t [0..])

-- | Slide through tableau with some function to choose next box.
slideWith :: (Tableau -> Box -> Maybe Box) -> Tableau -> Box -> (Tableau, Box)
slideWith choice t b = case choice t b of
                    Nothing -> (t,b)
                    Just b' -> case t .! b' of
                                 Just (-1) -> (t,b)
                                 Just _    -> slideWith choice (swap t b b') b'

-- | Find inner corner of tableau.
findCorner' :: Tableau -> Maybe Box
findCorner' t =
    let rs = dropWhile (notElem 0 . fst) (zip t [0..])
     in if null rs
          then Nothing
          else let (r,i) = head rs
                   Just j = elemIndex 0 r
                in Just (i,j)

-- | Find the index of a corner in a Tableau
findCorner :: Tableau -> Maybe Box
findCorner t =
  let rs = filter (elem 0 . fst) (zip t [0..])
   in if null rs
        then Nothing
        else let (r,i) = last rs
                 (_,j) = last $ takeWhile ((== 0) . fst) (zip r [0..])
              in Just (i,j)
