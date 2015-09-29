module InfiniteLists where

-- | construct a name generator
--
-- names = ["a".."z", "a1".."z1", "a2".."z2", ...]

names :: [String]
names = [[x]++(numToStr y) | y <- [0..], x <- ['a'..'z']]
  where
    numToStr n
      | n == 0 = ""
      | otherwise = show n


-- | constructs the infinite sequence
-- of fibonacci numbers in linear time
--
-- fibs = [0, 1, 1, 2, 3, 5, 8, ...]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) (tail fibs) fibs

-- ----------------------------------------
--
-- | naive prime number generator with
-- sieve of Eratosthenes and a recursive
-- sieve operation

primes :: [Integer]
primes = sieve [2..]
  where
    sieve   :: [Integer] -> [Integer]
    sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- ----------------------------------------
--
-- | the hamiltonian sequence is the ordered sequence
-- of natural number which are multiples of 2 or 3 or 5
--
-- Implementation: the 3 lists of multiples of 2, 3 and 5
-- are merged together with a @merges@ function.
--
-- The direct solution is

hamilton' :: [Integer]
hamilton'
  = filter
    (\ i ->    i `mod` 2 == 0
            || i `mod` 3 == 0
            || i `mod` 5 == 0
    ) [0..]

-- | @hamilton@ by merging sequences

hamilton :: [Integer]
hamilton
  = merges' [is2, is3, is5]
    where
      is2 = [x |  x <- [0..], x `mod` 2 == 0] -- shitty
      is3 = [x*3 |  x <- [0..]] -- not shitty
      is5 = map (*5) [0..] -- equal not shitty to is3

merge :: [Integer] -> [Integer] -> [Integer]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x == y = [x] ++ merge xs (ys)
  | x < y = [x] ++ merge xs (y:ys)
  | otherwise = [y] ++ merge ys (x:xs)

-- | @merges@ takes a list of lists of ascending integers
-- and merges these lists into a single sorted list without any duplicates
-- direct impl

merges :: [[Integer]] -> [Integer]
merges [] = []
merges ([]:xss) = merges(xss)
merges (xs:[]) = xs
merges (xs:xss) = merges ((merge xs (head xss)) : tail xss)

-- | @merges@ with a fold

merges' :: [[Integer]] -> [Integer]
merges' = foldr merge []    -- after chapter about folds

-- ----------------------------------------
