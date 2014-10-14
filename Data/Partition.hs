{-# LANGUAGE PatternGuards        #-}

--------------------------------------------------------------------------
-- |
-- Module               : Data.Parition
-- Copyright            : (c) Luke Palmer, 2013
-- License              : BSD3
--
-- Maintainer           : Luke Palmer <lrpalmer@gmail.com>
-- Stability            : experimental
-- Portability          : portable
--
-- Disjoint set data structure -- @Partition a@ maintains a collection of
-- disjoint sets of type @a@, with the ability to find which set a particular
-- item belongs to and the ability to merge any two such sets into one.
---------------------------------------------------------------------------

module Data.Partition 
    ( Partition, discrete, empty, fromSets, fromDisjointSets, nontrivialSets, joinElems, find, rep )
 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

-- | A Partition of @a@: represents a collection of disjoint sets of @a@ whose
-- union includes every element of @a@.  Semantics: @[[Partition a]] = P(P(a))@
-- where @P@ is the power set operation.
data Partition a 
    = Partition { forwardMap :: Map.Map a a, backwardMap :: Map.Map a (Set.Set a) }
    deriving (Eq, Ord)  -- Since the representative is always the least element,
                        -- we have a canonical representation and Eq is meaningful.
                        -- Ord may not mean anything, but at least there some 
                        -- computable total ordering on Partitions, and that is helpful
                        -- sometimes.

instance (Show a) => Show (Partition a) where
    show p = "fromDisjointSets " ++ show (nontrivialSets p)

-- | A partition in which every element of @a@ is in its own set.  Semantics:
-- @[[discrete]] = { { x } | x in a }@
discrete :: Partition a
discrete = Partition Map.empty Map.empty

-- | Synonym for @discrete@.
empty :: Partition a
empty = discrete

-- | Takes a list of disjoint sets and constructs a partition containing those sets,
-- with every remaining element being given its own set. The precondition is
-- not checked.
-- 
-- /O/ (/n/ log /n/), where /n/ is the total number of elements in the given sets.
fromDisjointSets :: (Ord a) => [Set.Set a] -> Partition a
fromDisjointSets sets = Partition { 
        forwardMap = Map.fromList [ (x, Set.findMin s) | s <- sets', x <- Set.toList s ],
        backwardMap = Map.fromList [ (Set.findMin s, s) | s <- sets' ]
    }
    where
      sets' = filter (not.isSingleton) sets
      isSingleton s = 1 == Set.size s

-- | Takes a list of (not necessarily disjoint) sets and constructs a partition
--   that associates all elements shared in /any/ of the sets.
-- 
--   /O/ (/n/ /k/ log /n/), where /k/ is the maximum set-size and /n/ = /l/ /k/ is
--   the total number of non-discrete elements.
fromSets :: (Ord a) => [Set.Set a] -> Partition a
fromSets = foldr joins discrete
 where joins set | (p0:ps) <- Set.toList set
                    = foldr ((.) . joinElems p0) id ps
       joins _empty = id

-- | Returns a list of all nontrivial sets (sets with more than one element) in the 
-- partition.
nontrivialSets :: Partition a -> [Set.Set a]
nontrivialSets = Map.elems . backwardMap

-- | @joinElems x y@ merges the two sets containing @x@ and @y@ into a single set.  Semantics:
-- @[[joinElems x y p]] = (p \`minus\` find x \`minus\` find y) \`union\` { find x \`union\` find y }@.
-- 
-- /O/ (max(/k/ log /n/, /k/ log /k/)), where /k/ is the size of nontrivial subsets
-- and /n/ is the total number of elements in such sets.
joinElems :: (Ord a) => a -> a -> Partition a -> Partition a
joinElems x y p = case compare x' y' of
                 LT -> go x' y'
                 EQ -> p
                 GT -> go y' x'
    where
    x' = rep p x
    y' = rep p y

    go into other = Partition { 
                        forwardMap = compose [ Map.insert o into | o <- Set.toList otherSrc ] (forwardMap p),
                        backwardMap = Map.insert into (Set.union (repFind p into) otherSrc) 
                                    . Map.delete other
                                    $ backwardMap p
                    }
        where
        otherSrc = repFind p other

-- | @find p x@ finds the set that the element @x@ is associated with.  Semantics: 
-- @[[find p x]] = the unique s in p such that x in s@.
find :: (Ord a) => Partition a -> a -> Set.Set a
find p = repFind p . rep p

-- | @rep p x@ finds the minimum element in the set containing @x@.
rep :: (Ord a) => Partition a -> a -> a
rep p x = fromMaybe x (Map.lookup x (forwardMap p))

-- Find the set that x is in given that x is already a representative element.
repFind :: (Ord a) => Partition a -> a -> Set.Set a
repFind p x = fromMaybe (Set.singleton x) (Map.lookup x (backwardMap p))

compose :: [a -> a] -> a -> a
compose = foldr (.) id
