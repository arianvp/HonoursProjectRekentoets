-- Special container type that holds elements in a non-ordered way.
module Bag where

import Data.Maybe
import qualified Data.Map as M

newtype Bag a = Bag (M.Map a Int)
              deriving (Show,Read,Ord,Eq)

-- Constructs an empty Bag
empty :: Bag a
empty = Bag M.empty

-- Constructs a Bag holding one element
singleton :: a -> Bag a
singleton a = Bag $ M.singleton a 1

-- Inserts a given element in a Bag
insert :: (Ord a) => a -> Bag a -> Bag a
insert x (Bag map) = Bag $ M.insertWith (+) x 1 map

-- Inserts the elements of the list into the Bag
insertList :: (Ord a) => [a] -> Bag a -> Bag a
insertList []     bag       = bag
insertList (x:xs) (Bag map) = insertList xs (Bag $ M.insertWith (+) x 1 map)

-- Combines two bags into one.
union :: (Ord a) => Bag a -> Bag a -> Bag a
union (Bag x) (Bag y) = Bag $ foldr (\(k,v) map -> M.insertWith (+) k v map) x (M.assocs y)

-- Returns the number of elements of the bag.
size :: Bag a -> Int
size (Bag map) = sum $ M.elems map 

-- Checks if the bag contains only a single item.
isSingleton :: (Ord a) => Bag a -> Bool
isSingleton (Bag map) = checkVal $ M.elems map
    where checkVal [v] = v == 1
          checkVal _   = False

-- Removes a given element from a bag.
-- In case the element isn't in the bag, the bag will be returned as is.
remove :: (Ord a) => a -> Bag a -> Bag a
remove a b@(Bag map) | isNothing value     = b
                     | fromJust value <= 1 = Bag $ M.delete a map
                     | otherwise           = Bag $ M.insertWith (flip (-)) a 1 map
    where value = M.lookup a map

-- Constructs a bag from a list of elements.
fromList :: (Ord a) => [a] -> Bag a
fromList = foldl f empty
    where
        f (Bag map) x = Bag $ M.insertWith (+) x 1 map

-- Converts a Bag into a list.
toList :: Bag a -> [a]
toList (Bag m) = concatMap f $ M.toList m
    where f (a,b) = replicate b a


-- Converts a Bag into a list without repeating any elements
toListU :: Bag a -> [a]
toListU (Bag m) = map f $ M.toList m
    where f (a,b) = a
