import Test.QuickCheck
import PriorityQueue

genHeap :: Gen (BinomialQueue Int)
genHeap = frequency [(1, genEmpty), (9, genNonEmpty)]

genEmpty :: Gen (BinomialQueue Int)
genEmpty = return empty

genNonEmpty :: Gen (BinomialQueue Int)
genNonEmpty = sized (\size -> do
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

prop_FindMinReturnsOnlyItem :: Int -> Bool
prop_FindMinReturnsOnlyItem x = 
    findMin h == x
    where
        h = insert x empty

prop_DeleteMinDeletesOnlyItem :: Int -> Bool
prop_DeleteMinDeletesOnlyItem x = 
    isEmpty h2
    where
        h1 = insert x empty
        h2 = deleteMin h1

prop_FindMinReturnsMinOfTwoItems :: Int -> Int -> Bool
prop_FindMinReturnsMinOfTwoItems x y = 
    findMin h2 == min x y
    where
        h1 = insert x empty
        h2 = insert y h1

prop_IsOrderedCorrectly :: [Int] -> Bool
prop_IsOrderedCorrectly xs =
    isOrderedCorrectly h
    where
        h = insertValues xs

-- This test currently fails when xs contains the
-- same value repeated >= 6 times (fine for 5 times)
prop_InsertingTheSameValueSeveralTimes :: Int -> Int -> Bool
prop_InsertingTheSameValueSeveralTimes n x = 
    getValues h == xs
    where
        l = abs n `mod` 10
        xs = replicate l x
        h = insertValues xs

main :: IO ()
main = do
    quickCheck prop_FindMinReturnsOnlyItem
    quickCheck prop_DeleteMinDeletesOnlyItem
    quickCheck prop_FindMinReturnsMinOfTwoItems
    quickCheck prop_IsOrderedCorrectly
    quickCheck prop_InsertingTheSameValueSeveralTimes
