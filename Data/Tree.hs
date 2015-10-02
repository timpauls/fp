{-# LANGUAGE DeriveDataTypeable #-}

-- ----------------------------------------

-- | binary tree with values at the leafs (Tip),
-- the branches (Bin) don't contain any further information,
-- the empty tree is represented by a special value Null

module Data.Tree
where

import           Prelude             hiding (foldl, foldr, head, tail, init, last)

import           Control.Applicative
import           Control.Monad

import           Data.Data
import           Data.Foldable
import           Data.Monoid

-- ----------------------------------------

data Tree a
    = Null
    | Tip a
    | Bin (Tree a) (Tree a)
      deriving (Show, Data, Typeable)

-- | smart constructor
-- garantiert einen baum ohne Null
bin :: Tree a -> Tree a -> Tree a
bin Null r = r
bin l Null = l
bin l r = Bin l r

instance Functor Tree where
  fmap f Null = Null
  fmap f (Tip a) = Tip(f a)
  fmap f (Bin a b) = Bin (fmap f a) (fmap f b)

instance Applicative Tree where
  pure  = undefined
  (<*>) = undefined
  
instance Monad Tree where
  return     = undefined
  _    >>= _ = undefined

instance Alternative Tree where
  empty = mzero   -- or Null
  (<|>) = mplus

instance MonadPlus Tree where
  mzero = undefined
  mplus = undefined

instance Monoid (Tree a) where
  mempty  = undefined
  mappend = undefined

-- fold elements like in a list from right to left
instance Foldable Tree where
  foldr _ e t = undefined

-- ----------------------------------------
-- classical visitor

visitTree :: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
visitTree e tf bf = visit'
  where
    visit' Null = e
    visit' (Tip x) = tf x
    visit' (Bin l r) = bf (visit' l) (visit' r)

-- special visitors

sizeTree :: Tree a -> Int
sizeTree = visitTree 0 (const 1) (+)

minDepth, maxDepth :: Tree a -> Int
minDepth = visitTree 0 (const 1) (\x y -> (x `min` y) +1)
maxDepth = visitTree 0 (const 1) (\x y -> (x `max` y) +1)

-- ----------------------------------------
-- access functions

-- zersägt den baum so, dass er (am weitesten linke element, der Baum ohne das linkeste Element)
-- erwartet nur valide baeume.
viewL :: Tree a -> Maybe (a, Tree a)
viewL Null = Nothing
viewL (Tip a) = Just (a, Null)
viewL (Bin a b) = 
  Just (l, bin b rt)
  where
    (Just (l, rt)) = viewL a


-- zersägt den baum so, dass er (am weitesten rechte element, der Baum ohne das rechteste Element)
viewR :: Tree a -> Maybe (Tree a, a)
viewR Null = Nothing
viewR (Tip a) = Just (Null, a)
viewR (Bin a b) = 
  Just (bin a rt, r)
  where
    (Just (rt, r)) = viewR b

head :: Tree a -> a
head = maybe (error "head: empty tree") fst . viewL

tail :: Tree a -> Tree a
tail = maybe (error "tail: empty tree") snd . viewL

last :: Tree a -> a
last = maybe (error "last: empty tree") snd . viewR

init :: Tree a -> Tree a
init = maybe (error "init: empty tree") fst . viewR

-- ----------------------------------------
-- conversions to/from lists

-- | runs in O(n) due to the use of (:)
toList :: Tree a -> [a]
toList = foldr undefined undefined

-- | runs in O(n^2) due to the use of (++)
toListSlow :: Tree a -> [a]
toListSlow = visitTree undefined undefined undefined

-- | build a balanced tree
--
-- doesn't work for infinite lists

-- weak balancing criterion
fromList :: [a] -> Tree a
fromList = fromList' -- cheat

-- strong balancing criterion
fromList' :: [a] -> Tree a
fromList' [] = Null
fromList' (x:[]) = Tip x
fromList' l = 
  bin (fromList' (lh l)) (fromList' (rh l))
  where
    lh l = take (mid l) l
    rh l = drop (mid l) l
    mid l = (length l) `div` 2


-- list to the right
fromList'' :: [a] -> Tree a
fromList'' = foldr (\ x t -> Tip x `bin` t) Null

-- list to the left
fromList''' :: [a] -> Tree a
fromList''' = foldl (\ t x -> t `bin` Tip x) Null

-- runtime differences between fromList, fromList', fromList'', fromList'''?
-- differences in balancing quality?

-- ----------------------------------------
