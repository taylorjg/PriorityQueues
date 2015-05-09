{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import BinomialQueue

instance Arbitrary (BinomialQueue Int) where
    arbitrary = genBinomialQueueInt

genBinomialQueueInt :: Gen (BinomialQueue Int)
genBinomialQueueInt =
    frequency [
        (1, genEmptyBinomialQueueInt),
        (9, genNonEmptyBinomialQueueInt)
    ]

genEmptyBinomialQueueInt :: Gen (BinomialQueue Int)
genEmptyBinomialQueueInt = return empty

genNonEmptyBinomialQueueInt :: Gen (BinomialQueue Int)
genNonEmptyBinomialQueueInt = sized (\size -> do
    xs <- vectorOf size genInt
    return $ insertValues xs)

genInt :: Gen Int
genInt = arbitrary

insertValues :: [Int] -> BinomialQueue Int
insertValues xs =
    loop xs empty
    where
        loop (x:rest) h = loop rest (insert x h)
        loop _ h = h

getValues :: BinomialQueue Int -> [Int]
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

isOrderedCorrectly :: BinomialQueue Int -> Bool
isOrderedCorrectly h =
    case tryGetMin h of
        (True, m) -> loop h m
        _ -> True
    where
        tryGetMin h =
            case isEmpty h of
                False -> (True, findMin h)
                True -> (False, 0)
        loop h previousMin =
            case tryGetMin h2 of
                (True, m) -> m >= previousMin && loop h2 m
                _ -> True
            where
                h2 = deleteMin h

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

prop_IsOrderedCorrectly :: BinomialQueue Int -> Bool
prop_IsOrderedCorrectly h = isOrderedCorrectly h

prop_DeleteMinAfterInsertingTheSameValueSeveralTimes :: Int -> Int -> Bool
prop_DeleteMinAfterInsertingTheSameValueSeveralTimes n x =
    getValues h2 == drop 1 xs
    where
        l = (abs n `mod` 10) + 2
        xs = (replicate l x) ++ [succ x]
        h1 = insertValues xs
        h2 = deleteMin h1

prop_IsOrderedCorrectlyAfterMeld :: BinomialQueue Int -> BinomialQueue Int -> Bool
prop_IsOrderedCorrectlyAfterMeld h1 h2 =
    isOrderedCorrectly $ meld h1 h2

main :: IO ()
main = do
    quickCheck prop_FindMinWhenOnlyOneItem
    quickCheck prop_DeleteMinWhenOnlyOneItem
    quickCheck prop_FindMinWhenTwoItemsReturnsMinOfTwoItems
    quickCheck prop_IsOrderedCorrectly
    quickCheck prop_InsertingTheSameValueSeveralTimes
    quickCheck prop_DeleteMinAfterInsertingTheSameValueSeveralTimes
    quickCheck prop_IsOrderedCorrectlyAfterMeld
