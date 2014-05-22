module Tableau.Promotion where

import Tableau

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

-- | Construct a tableau with minimal promotion orbit length
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
