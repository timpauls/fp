module Data.FunctionalList
where

import           Prelude (Bool(..), (.), (++), undefined)
import qualified Prelude as P

type List a = [a] -> [a]

-- ----------------------------------------

fromList        :: [a] -> List a
fromList l      = \xs -> l ++ xs

toList          :: List a -> [a]
toList l        = l []

empty           :: List a
empty           = P.id

singleton       :: a -> List a
singleton     	= (:)

-- (:) for functional lists
cons            :: a -> List a -> List a
--					a -> ([a] -> [a]) -> ([a] -> [a])
cons e l        = \xs -> e : l xs

-- dual to cons
snoc            :: List a -> a -> List a
--				([a] -> [a]) -> a -> ([a] -> [a])
snoc l e        = append l (singleton e)

-- (++) for functional lists
append          :: List a -> List a -> List a
append l1 l2    = \xs -> l1 (l2 xs)
-- append l1 l2 = l1 . l2
-- append = (.)

-- like concat for normal lists: foldr (++) []
concat          :: [List a] -> List a
concat          = P.foldr append empty 

-- like map for normal lists: foldr ((:) . f) []
map             :: (a -> b) -> List a -> List b
map f           = foldr (cons.f) empty

-- foldr with foldr for normal lists
-- P.foldr mit toList
foldr           :: (a -> b -> b) -> b -> List a -> b
foldr op n      = P.foldr op n . toList

-- head, tail, null
head            :: List a -> a
head     		= P.head . toList

tail            :: List a -> List a
tail l 	        = P.tail . l

null            :: List a -> Bool
null            = P.null . toList

reverse         :: List a -> List a
reverse l      	= P.reverse . l

-- ----------------------------------------
