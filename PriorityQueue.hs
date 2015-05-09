module PriorityQueue (
	PriorityQueue(..)
) where

class PriorityQueue pq where
	pqEmpty :: pq a
	pqIsEmpty :: pq a -> Bool
	pqInsert :: Ord a => a -> pq a -> pq a
	pqMeld :: Ord a => pq a -> pq a -> pq a
	pqFindMin :: Ord a => pq a -> a
	pqDeleteMin :: Ord a => pq a -> pq a
