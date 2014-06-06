module Tableau where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M

type Box = (Int,Int)
type Diagram = [Int]
type Filling = Map Box Int
data Tableau = SemiStandard { shape :: Diagram, filling :: Filling }
             | Skew { shape :: (Diagram, Diagram), filling :: Filling }
             | Shifted { shape :: Diagram, filling :: Filling }
             deriving (Show, Eq)

-- | Compute the size of a tableau.
size :: Tableau -> Int
size (Skew { shape = (lambda,mu) }) = (sum lambda) - (sum mu)
size = sum . shape

-- | Compute the number of rows of a tableau.
nrows :: Tableau -> Int
nrows (Skew { shape = (lambda,_) }) = length lambda
nrows = length . shape

-- | Compute the number of columns of a tableau---the length of first part.
ncols :: Tableau -> Int
ncols (Skew { shape = (lambda,mu) }) = (head lambda) - (head mu)
ncols = length . head . shape

-- | Compute the transpose of a Young Diagram
transposeDiagram :: Diagram -> Diagram
transposeDiagram sh = takeWhile (/= 0)
                    $ map (\n -> length $ filter (>= n) sh) [1..]

-- | Compute the transpose of a Filling
transposeFilling :: Filling -> Filling
transposeFilling f = M.fromList $ map (\((i,j),n) -> ((j,i),n)) $ M.toList f

-- | Compute the transpose of a Young tableau
transpose :: Tableau -> Tableau
transpose (Skew { shape = (lambda,mu), filling = f}) =
    Skew { shape   = (transposeDiagram lambda, transposeDiagram mu')
         , filling = transposeFilling f }
transpose (Semistandard { shape = lambda, filling = f }) =
    SemiStandard { shape   = transposeDiagram lambda
                 , filling = transposeFilling f }
transpose t = t

-- | Given a box `b' and tableau `t', return the entry contained in box
-- `b'; if `b' is not in the diagram, then `Nothing' is returned.
(.!) :: Box -> Tableau -> Maybe Int
b .! t = M.lookup b $ filling b

-- | Given box (i,j) in tableau, compute the next position to slide to
-- based on the given comparison function.
chooseSlideWith :: (Int -> Int -> Ordering) -> Tableau -> Box -> Maybe Box
chooseSlideWith f t (i,j) =
    case ((i,j+1) .! t, (i+1,j) .! t) of
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
    case ((i,j-1) .! t, (i-1,j) .! t) of
      (Nothing, Nothing) -> Nothing         -- Corner
      (Nothing, Just _)  -> Just (i-1,j)    -- None left
      (Just _, Nothing)  -> Just (i,j-1)    -- None above
      (Just l, Just a)   -> Just (if f l a == GT then (i,j-1) else (i-1,j))

-- | Reverse slide to the larger one.
chooseRSlide :: Tableau -> Box -> Maybe Box
chooseRSlide = chooseRSlideWith compare

-- | Swap the entry in (i,j) with the entry in (k,l)
swap :: Tableau -> Box -> Box -> Tableau
swap t a b =
    case (a .! t, b .! t) of
      (Just n, Just m) -> let f  = filling t
                             f' = M.adjust (const n) b $ M.adjust (const m) a f
                          in t { filling = f' }
      (_,_)            -> t

-- | Change the entry at (i,j) to n
changeEntry :: Tableau ->  Box -> Int -> Tableau
changeEntry t b n = t { filling = M.adjust (const n) b (filling t) }

-- | Extract subtableau with entries at least lo and entries at most hi
subtableau :: (Int,Int) -> Tableau -> Tableau
subtableau (lo, hi) t =
    t { filling = (M.filter (>= lo) . M.filter (<= hi) . filling) t }

-- | Compute the entries which are descents
descents :: Tableau -> [Int]
descents t = M.filterWithKey f t
  where f (_,j) n = not . M.null
                  $ M.filterWithKey (\(_,l) m -> m == n + 1 && l <= j)))

-- | Map a given function over all the entries of the given tableau.
mapTableau :: (Box -> Int -> Int) -> Tableau -> Tableau
mapTableau f t = t { filling = mapWithKey f (filling t) }

-- | Return the position of an inner corner of a tableau.
innerCorner :: Tableau -> Maybe Box
innerCorner (Skew t)         = let (_,mu) = shape t in (length mu, last mu)
innerCorner (SemiStandard t) = Nothing
innerCorner (Shifted t)      = Nothing
