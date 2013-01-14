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
    ( Partition, discrete, empty, join, find )
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

-- | A partition in which every element of @a@ is in its own set.  Semantics:
-- @[[discrete]] = { { x } | x in a }@
discrete :: Partition a
discrete = Partition Map.empty Map.empty

-- | Synonym for "discrete".
empty :: Partition a
empty = discrete

-- | @join x y@ merges the two sets containing @x@ and @y@ into a single set.  Semantics:
-- @[[join x y p]] = (p `minus` find x `minus` find y) `union` { find x `union` find y }@
join :: (Ord a) => a -> a -> Partition a -> Partition a
join x y p = case compare x' y' of
                 LT -> go x' y'
                 EQ -> p
                 GT -> go y' x'
    where
    x' = repr p x
    y' = repr p y

    go into other = Partition { 
                        forwardMap = compose [ Map.insert o into | o <- Set.toList otherSrc ] (forwardMap p),
                        backwardMap = Map.insert into (Set.union (reprFind p into) otherSrc) 
                                    . Map.delete other
                                    $ backwardMap p
                    }
        where
        otherSrc = reprFind p other

-- | @find p x@ finds the set that the element @x@ is associated with.  Semantics: 
-- @[[find p x]] = the unique s in p such that x in s@.
find :: (Ord a) => Partition a -> a -> Set.Set a
find p = reprFind p . repr p

-- Find the representative element for an element.
repr :: (Ord a) => Partition a -> a -> a
repr p x = fromMaybe x (Map.lookup x (forwardMap p))

-- Find the set that x is in given that x is already a representative element.
reprFind :: (Ord a) => Partition a -> a -> Set.Set a
reprFind p x = fromMaybe (Set.singleton x) (Map.lookup x (backwardMap p))

compose :: [a -> a] -> a -> a
compose = foldr (.) id
