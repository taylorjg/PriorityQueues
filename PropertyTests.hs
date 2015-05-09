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

prop_FindMinReturnsOnlyItem :: Int -> Bool
prop_FindMinReturnsOnlyItem a = 
    let h = insert a empty
    in findMin h == a

prop_DeleteMinDeletesOnlyItem :: Int -> Bool
prop_DeleteMinDeletesOnlyItem a = 
    let
        h1 = insert a empty
        h2 = deleteMin h1
    in
        isEmpty h2

prop_FindMinReturnsMinOfTwoItems :: Int -> Int -> Bool
prop_FindMinReturnsMinOfTwoItems a b = 
    let
        h1 = insert a empty
        h2 = insert b h1
    in
        findMin h2 == min a b

main :: IO ()
main = do
    quickCheck prop_FindMinReturnsOnlyItem
    quickCheck prop_DeleteMinDeletesOnlyItem
    quickCheck prop_FindMinReturnsMinOfTwoItems

    --let h1 = empty
    --putStrLn $ show $ isEmpty h1

    --let h2 = insert 10 h1
    --putStrLn $ show $ isEmpty h2
    --putStrLn $ show $ findMin h2

    --let h3 = insert 11 h2
    --putStrLn $ show $ isEmpty h3
    --putStrLn $ show $ findMin h3

    --let h4 = insert 5 h2
    --putStrLn $ show $ isEmpty h4
    --putStrLn $ show $ findMin h4

    --let h5 = deleteMin h4
    --putStrLn $ show $ isEmpty h5
    --putStrLn $ show $ findMin h5
