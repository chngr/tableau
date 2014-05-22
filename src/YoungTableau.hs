{-# LANGUAGE NoMonomorphismRestriction #-}
module YoungTableau where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Tableau
import Tableau.Promotion
import Data.List (transpose,permutations)

-- | Create Ferrer's Diagram from list of integers
ferrer :: [Int] -> Diagram B R2
ferrer ns = cat unitY $ reverse $ map row ns

-- | Create a row of n unit squares
row :: Int -> Diagram B R2
row n = cat unitX $ take n (repeat unitSquare)

-- | Create a row of unit squares filled with an entry
tRow :: Show a => [a] -> Diagram B R2
tRow t = cat unitX $ map (\e -> text (show e) # fontSize 0.8
                                              # font "inconsolata" <> unitSquare) t

tRow' :: Show a => [(Int,Int)] -> ([a],Int) -> Diagram B R2
tRow' p (r,i) = let p' = filter ((== i) . fst) p
                    r' = zip r [0..]
                in cat unitX
                 $ map (\(e,l) -> let sq = if any ((== l) . snd) p'
                                             then unitSquare # fc purple
                                             else unitSquare
                                  in text (show e) # fontSize 0.8
                                                   # font "inconsolata"
                                                   <> sq) r'

-- | Draw a tableau
tableau :: Tableau -> Diagram B R2
tableau t = cat unitY $ reverse $ map tRow t

-- | Draw a tableau with promotion path coloured
tableauWithPromotionPath :: Tableau -> Diagram B R2
tableauWithPromotionPath t =
    let p = promotionPath t
    in cat unitY $ reverse $ map (tRow' p) (zip t [0..])

-- | Create a Ferrer's diagram with the largest box coloured
growthFerrer :: Tableau -> Diagram B R2
growthFerrer t = cat unitY $ reverse $ map (row' k) (zip (shape t) [0..])
    where Just (k,_) = t !? (size t)
          row' m (n,l) = if m /= l
                           then row n
                           else row (n - 1) ||| unitSquare # fc purple


-- | Creates a sequence of diagrams representing the growth diagram of
-- a standard tableau.
growth :: Tableau -> [Diagram B R2]
growth t = map (\n -> growthFerrer $ subtableau (1,n) t) [1..(size t)]

divideBy _ [] = []
divideBy n xs = let (y,ys) = splitAt n xs in y:(divideBy n ys)

t1 :: Tableau
t1 = [ [1, 2, 3, 4, 5, 6]
    , [7, 8, 9, 10, 11, 12]
    , [13, 14, 15, 16, 17, 19]
    , [18, 20, 21, 22, 23, 24]
    ]

t2 :: Tableau
t2 = [[1,4,7,10],[2,5,8,11],[3,6,9,12]]

t3 :: Tableau
t3 = transpose [ [1, 2, 6, 10, 14, 18]
     , [3, 4, 8, 12, 16, 20]
     , [5, 9, 13, 17, 21, 22]
     , [7, 11, 15, 19, 23, 24]]

-- w = 15243
t4 :: Tableau
t4 = [ [  1,  2,  3,  8, 13, 18, 23, 28]
     , [  4,  7,  9, 14, 19, 24, 29, 33]
     , [  5, 10, 12, 17, 22, 27, 34, 38]
     , [  6, 15, 20, 25, 30, 32, 35, 39]
     , [ 11, 16, 21, 26, 31, 36, 37, 40]
     ]

-- w = 513642
t5 :: Tableau
t5 = [ [  1,  2,  4,  8, 14, 20, 26, 32, 38, 44]
     , [  3,  6,  9, 10, 16, 22, 28, 34, 40, 50]
     , [  5, 12, 15, 18, 24, 30, 36, 42, 46, 52]
     , [  7, 13, 21, 27, 33, 39, 45, 48, 54, 56]
     , [ 11, 19, 25, 31, 37, 43, 49, 51, 57, 58]
     , [ 17, 23, 29, 35, 41, 47, 53, 55, 59, 60]
     ]

t6 :: Tableau
t6 = [ [  1,  4,  6,  8, 10]
     , [  2,  5,  7,  9, 18]
     , [  3, 12, 14, 16, 19]
     , [ 11, 13, 15, 17, 20]
     ]

ex :: Diagram B R2
ex = pad 1.1 . center $ cat' unitX (with & sep .~ 1) $ growth t1

ex2 :: Diagram B R2
ex2 = pad 1.1 . center $
      cat' (negateV unitY) (with & sep .~ 1) $
      map (\w ->
      cat' (negateV unitY) (with & sep .~ 1) $
      reverse $ map drawRow $ divideBy 25 $ map tableauWithPromotionPath $
      promotionOrbit (minOrbit 3 w)) (map (1:) (permutations [2,3]))
  where drawRow r = cat' unitX (with & sep .~ 1) $ r

ex3 :: Diagram B R2
ex3 = pad 1.1 . center $ drawRow $ map tableauWithPromotionPath $ promotionOrbit (divideBy 5 [1..15])
  where drawRow r = cat' unitX (with & sep .~ 1) $ r


ws = [[1,2,3,4,5,6], [1,2,3,4,6,5], [1,2,3,6,4,5], [1,2,6,3,4,5], [1,6,2,3,4,5], [1,2,3,4,5,6]
      ,[1,2,3,5,4,6], [1,2,5,3,4,6], [1,5,2,3,4,6], [1,2,3,4,6,5], [1,2,4,3,6,5], [1,4,2,3,6,5]
      ,[1,2,3,6,5,4], [1,3,2,6,5,4], [1,2,6,5,4,3], [1,6,5,4,3,2]]

main = mainWith $ ex2
