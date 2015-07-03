-- Special container type that holds elements in a non-ordered way.
module Bag (module Bag, module D) where

import Data.Maybe
import Data.MultiSet as D

type Bag a = D.MultiSet a
{-
toList :: Bag a -> [a]
toList = D.toList 

toListU :: Bag a -> [a]
toListU = D.distinctElems

fromList :: (Ord a) => [a] -> Bag a
fromList = D.fromList

delete :: (Ord a) => a -> Bag a -> Bag a
delete = D.delete 

singleton :: a -> Bag a
singleton = D.singleton

empty :: Bag a
empty = D.empty

insert :: (Ord a) => a -> Bag a -> Bag a
insert = D.insert

union :: (Ord a) => Bag a -> Bag a -> Bag a
union = D.union

size :: Bag a -> Int
size = D.size 
-}
----

isSingleton :: (Ord a) => Bag a -> Bool
isSingleton = (1==) . D.size

insertList :: (Ord a) => [a] -> Bag a -> Bag a
insertList []     = id
insertList (x:xs) = insertList xs . D.insert x

deleteList :: (Ord a) => [a] -> Bag a -> Bag a
deleteList []     = id
deleteList (x:xs) = deleteList xs . D.delete x

headL = head . D.toList