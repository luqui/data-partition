module Data.Parititon where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

data Partition a 
    = Partition { forwardMap :: Map.Map a a, backwardMap :: Map.Map a (Set.Set a) }
    deriving (Show)

empty :: Partition a
empty = Partition Map.empty Map.empty

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

find :: (Ord a) => Partition a -> a -> Set.Set a
find p = reprFind p . repr p

repr :: (Ord a) => Partition a -> a -> a
repr p x = fromMaybe x (Map.lookup x (forwardMap p))

reprFind :: (Ord a) => Partition a -> a -> Set.Set a
reprFind p x = fromMaybe (Set.singleton x) (Map.lookup x (backwardMap p))

compose :: [a -> a] -> a -> a
compose = foldr (.) id
