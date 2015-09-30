module Data.IntervalSet
where

-- ----------------------------------------

-- an pair of Ints can represent closed Intervals
-- (i, j) <=> [i..j]
-- Intervalls with i > j represent the empty set

type Interval = (Int, Int)

overlap :: Interval -> Interval -> Bool
overlap (x1, y1) (x2, y2)
  = (x1 <= (y2+1)) && (x2 <= (y1+1))


less :: Interval -> Interval -> Bool
less (_x1, y1) (x2, _y2)
  = y1 < x2

                           
emptyInterval :: Interval -> Bool
emptyInterval (x, y)
  = x > y


-- merge 2 (overlapping) intervals
merge :: Interval -> Interval -> Interval
merge (x1, y1) (x2, y2) = (x1 `min` x2, y1 `max` y2)


-- ----------------------------------------

-- a set of integers can be represented by an
-- ordered list of none empty intervals, which
-- do not overlap

type IntervalSet = [Interval]

inv :: IntervalSet -> Bool
inv [] = True
inv (x: []) = not (emptyInterval x)
inv (x0: (x1:xs)) = not(emptyInterval x0) && not(emptyInterval x1) && x0 `less` x1 && not (x0 `overlap` x1) && inv (x1:xs)



-- ----------------------------------------
-- internal interval set ops

singleInterval :: Int -> Int -> IntervalSet
singleInterval x y
    | x <= y    = [(x, y)]
    | otherwise = []

insertInterval :: Interval -> IntervalSet -> IntervalSet
insertInterval i []  
	| emptyInterval i = []
	| otherwise = [i]
insertInterval i (x:xs) 
	| emptyInterval i = (x:xs)
	| overlap i x =  insertInterval (merge i x) xs
	| not (less i x) = x : (insertInterval i xs)
	| otherwise = (i : (x : xs))

fromIntervalList :: [(Int, Int)] -> IntervalSet
fromIntervalList = undefined


-- ----------------------------------------
--
-- exported ops, names similar to Data.Set

empty :: IntervalSet
empty = []

singleton :: Int -> IntervalSet
singleton i = singleInterval i i

insert :: Int -> IntervalSet -> IntervalSet
insert i = insertInterval (i, i)

union :: IntervalSet -> IntervalSet -> IntervalSet
union = undefined


member :: Int -> IntervalSet -> Bool
member = undefined

         
fromList :: [Int] -> IntervalSet
fromList = undefined


toList :: IntervalSet -> [Int]
toList = undefined


-- ----------------------------------------
