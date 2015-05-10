import Test.QuickCheck
import PriorityQueue
import BinomialQueue

instance (Ord a, Arbitrary a) => Arbitrary (BinomialQueue a) where
    arbitrary = genBinomialQueue

genBinomialQueue :: (Ord a, Arbitrary a) => Gen (BinomialQueue a)
genBinomialQueue =
    frequency [
        (1, genEmptyBinomialQueue),
        (9, genNonEmptyBinomialQueue)
    ]

genEmptyBinomialQueue :: (Ord a, Arbitrary a) => Gen (BinomialQueue a)
genEmptyBinomialQueue = return empty

genNonEmptyBinomialQueue :: (Ord a, Arbitrary a) => Gen (BinomialQueue a)
genNonEmptyBinomialQueue = sized (\size -> do
    let genA = arbitrary :: (Arbitrary a) => Gen a
    xs <- vectorOf size genA
    return $ insertValues xs)

-- *********************************
-- PriorityQueue variants of helpers
-- *********************************

insertValuesPQ :: (Ord a, PriorityQueue pq) => [a] -> pq a
insertValuesPQ xs =
    loop xs pqEmpty
    where
        loop (x:rest) h = loop rest (pqInsert x h)
        loop _ h = h

getValuesPQ :: (Ord a, PriorityQueue pq) => pq a -> [a]
getValuesPQ h =
    reverse $ loop h []
    where
        loop h xs = 
            if pqIsEmpty h
                then xs
                else
                    loop h2 (x:xs)
                    where
                        x = pqFindMin h
                        h2 = pqDeleteMin h

isOrderedCorrectlyPQ :: (Ord a, PriorityQueue pq) => pq a -> Bool
isOrderedCorrectlyPQ h =
    case tryGetMin h of
        Just m -> loop h m
        _ -> True
    where
        tryGetMin h =
            case pqIsEmpty h of
                False -> Just $ pqFindMin h
                True -> Nothing
        loop h previousMin =
            case tryGetMin h2 of
                Just m -> m >= previousMin && loop h2 m
                _ -> True
            where
                h2 = pqDeleteMin h

-- *********************************
-- BinomialQueue variants of helpers
-- *********************************

insertValues :: Ord a => [a] -> BinomialQueue a
insertValues xs =
    loop xs empty
    where
        loop (x:rest) h = loop rest (insert x h)
        loop _ h = h

getValues :: Ord a => BinomialQueue a -> [a]
getValues h =
    reverse $ loop h []
    where
        loop h xs = 
            if isEmpty h
                then xs
                else
                    loop h2 (x:xs)
                    where
                        x = findMin h
                        h2 = deleteMin h

--isOrderedCorrectly :: Ord a => BinomialQueue a -> Bool
--isOrderedCorrectly h =
--    case tryGetMin h of
--        Just m -> loop h m
--        _ -> True
--    where
--        tryGetMin h =
--            case isEmpty h of
--                False -> Just $ findMin h
--                True -> Nothing
--        loop h previousMin =
--            case tryGetMin h2 of
--                Just m -> m >= previousMin && loop h2 m
--                _ -> True
--            where
--                h2 = deleteMin h

-- **************
-- Property tests
-- **************

prop_FindMinWhenOnlyOneItem :: Int -> Bool
prop_FindMinWhenOnlyOneItem x = 
    findMin h == x
    where
        h = insert x empty

prop_DeleteMinWhenOnlyOneItem :: Int -> Bool
prop_DeleteMinWhenOnlyOneItem x = 
    isEmpty h2
    where
        h1 = insert x empty
        h2 = deleteMin h1

prop_FindMinWhenTwoItemsReturnsMinOfTwoItems :: Int -> Int -> Bool
prop_FindMinWhenTwoItemsReturnsMinOfTwoItems x y = 
    findMin h2 == min x y
    where
        h1 = insert x empty
        h2 = insert y h1

prop_InsertingTheSameValueSeveralTimes :: Int -> Int -> Bool
prop_InsertingTheSameValueSeveralTimes n x = 
    getValues h == xs
    where
        l = abs n `mod` 10
        xs = replicate l x
        h = insertValues xs

prop_DeleteMinAfterInsertingTheSameValueSeveralTimes :: Int -> Int -> Bool
prop_DeleteMinAfterInsertingTheSameValueSeveralTimes n x =
    getValues h2 == drop 1 xs
    where
        l = (abs n `mod` 10) + 2
        xs = (replicate l x) ++ [succ x]
        h1 = insertValues xs
        h2 = deleteMin h1

prop_IsOrderedCorrectlyPQ :: (Ord a, PriorityQueue pq) => pq a -> Bool
prop_IsOrderedCorrectlyPQ h = isOrderedCorrectlyPQ h

prop_IsOrderedCorrectlyAfterMeldPQ :: (Ord a, PriorityQueue pq) => pq a -> pq a -> Bool
prop_IsOrderedCorrectlyAfterMeldPQ h1 h2 = isOrderedCorrectlyPQ $ pqMeld h1 h2

main :: IO ()
main = do
    quickCheck prop_FindMinWhenOnlyOneItem
    quickCheck prop_DeleteMinWhenOnlyOneItem
    quickCheck prop_FindMinWhenTwoItemsReturnsMinOfTwoItems
    quickCheck prop_InsertingTheSameValueSeveralTimes
    quickCheck prop_DeleteMinAfterInsertingTheSameValueSeveralTimes
    quickCheck (prop_IsOrderedCorrectlyPQ :: BinomialQueue Int -> Bool)
    quickCheck (prop_IsOrderedCorrectlyAfterMeldPQ :: BinomialQueue Int -> BinomialQueue Int -> Bool)
