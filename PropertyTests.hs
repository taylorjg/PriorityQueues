import Test.QuickCheck
import PriorityQueue

main :: IO ()
main = do

    let h1 = empty
    putStrLn $ show $ isEmpty h1

    let h2 = insert 10 h1
    putStrLn $ show $ isEmpty h2
    putStrLn $ show $ findMin h2

    let h3 = insert 11 h2
    putStrLn $ show $ isEmpty h3
    putStrLn $ show $ findMin h3

    let h4 = insert 5 h2
    putStrLn $ show $ isEmpty h4
    putStrLn $ show $ findMin h4

    let h5 = deleteMin h4
    putStrLn $ show $ isEmpty h5
    putStrLn $ show $ findMin h5
