module Tableau.Staircase where

import Tableau

isStaircase :: Tableau -> Bool
isStaircase t = shape t == [n, (n - 1)..1]
  where n = length t

withDescents :: [Bool] -> [Tableau]
withDescents des = withDescents' k (head des) des' t0
  where n    = length des
        k    = floor . sqrt . fromIntegral $ 2 * n
        t0   = [1]:replicate (k-1) []
        des' = zip (tail des) [2..n]

withDescents' :: Int -> Bool -> [(Bool,Int)] -> Tableau -> [Tableau]
withDescents' _ _ []          t = [t]
withDescents' k d ((d',n):ds) t =
    let Just (i,_)   = t !? (n - 1)
        possibleRows = goodRows . filter shortEnough $ (if d then drop else take) (i+1) $ ti
     in foldl (\acc l -> acc
                      ++ withDescents' k d' ds (tweakRow l)) [] possibleRows
  where ti                  = zip t [0 .. (k - 1)]
        tweakRow l          = map (\(r,i) -> if i == l then r ++ [n] else r) ti
        shortEnough (r,l)   = (length r + l) < k
        goodRows :: [([Int],Int)] -> [Int]
        goodRows []         = []
        goodRows [rl]       = [snd rl]
        goodRows (rl:sk:ts) = let l    = snd rl
                                  lenr = length (fst rl)
                                  lens = length (fst sk)
                               in if lenr == lens
                                    then [l]
                                    else l:goodRows (sk:ts)
