module PriorityQueue (
    BinomialQueue(BQ),
    empty,
    isEmpty,
    insert,
    meld,
    findMin,
    deleteMin
    ) where

type Rank = Int
data Tree a = Node (a, Rank, [Tree a])
data BinomialQueue a = BQ [Tree a]

empty :: BinomialQueue a
empty = BQ []

isEmpty :: BinomialQueue a -> Bool
isEmpty (BQ []) = True
isEmpty _ = False

insert :: Ord a => a -> BinomialQueue a -> BinomialQueue a
insert x (BQ ts) = ins (Node (x, 0, [])) ts

meld :: Ord a => BinomialQueue a -> BinomialQueue a -> BinomialQueue a
meld (BQ []) h2 = h2
meld h1 (BQ []) = h1
meld (h1@(BQ (t1:ts1))) (h2@(BQ (t2:ts2))) =
    if rank t1 < rank t2
        then meld (BQ ts1) h2
        else if rank t2 < rank t1
            then meld h1 (BQ ts2)
            else ins (link t1 t2) ts
            where
                (BQ ts) = (meld (BQ ts1) (BQ ts2))

findMin :: Ord a => BinomialQueue a -> a
findMin (BQ []) = error "findMin: empty list"
findMin (BQ [t]) = root t
findMin (BQ (t:ts)) =
    let x = findMin (BQ ts)
    in if root t < x then root t else x

deleteMin :: Ord a => BinomialQueue a -> BinomialQueue a
deleteMin (BQ []) = error "deleteMin: empty list"
deleteMin (BQ ts) =
    let
        getMin [t] = (t, [])
        getMin (t:ts) =
            let (t', ts') = getMin ts
            in if root t < root t'
                then (t, ts)
                else (t', t:ts')
        (Node (x, r, c), ts2) = getMin ts
    in
        meld (BQ (reverse c)) (BQ ts2)

root :: Tree a -> a
root (Node (x, _, _)) = x

rank :: Tree a -> Int
rank (Node (_, r, _)) = r

link :: Ord a => Tree a -> Tree a -> Tree a
link (t1@(Node (x1, r1, c1))) (t2@(Node (x2, r2, c2))) =
    if x1 < x2
        then Node (x1, r1 + 1, t2:c1)
        else Node (x2, r2 + 1, t1:c2)

ins :: Ord a => Tree a -> [Tree a] -> BinomialQueue a
ins t [] = BQ [t]
ins t (t':ts) =
    if rank t < rank t'
        then BQ (t:t':ts)
        else ins (link t t') ts
