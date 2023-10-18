module Commands where

import Data.List
import System.Exit

-- Lib Functions

myFst :: (a, b) -> a
myFst (x, y) = x

mySnd  :: (a, b) -> b
mySnd (x, y) = y

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- PushSwap_Checker functions

swap :: [Int] -> [Int]
swap [] = []
swap (x:[]) = [x]
swap (x:x2:xs) = x2:x:xs

takep :: [Int] -> [Int] -> ([Int], [Int])
takep l_a [] = (l_a, [])
takep l_a (x:xs) = ((x:l_a), xs)

rotate :: [Int] -> [Int]
rotate [] = []
rotate (x:xs)
    | length (x:xs) < 2 = (x:xs)
    | otherwise = xs ++ [x]

revrotate :: [Int] -> [Int]
revrotate x = myReverse (rotate (myReverse x))

-- Command recognition

execrr :: [String] -> [Int] -> [Int] -> IO ()
execrr [] l_a l_b
    | l_a == (sort l_a) && l_b == [] = putStrLn "OK"
    | otherwise = putStr "KO: " >> print (l_a, l_b)
execrr (x:xs) y z
    | x == "rra" = execs xs (revrotate y) z
    | x == "rrb" = execs xs y (revrotate z)
    | x == "rrr" = execs xs (revrotate y) (revrotate z)
    | otherwise = putStrLn "Unknown Command" >> exitWith (ExitFailure 84)

execr :: [String] -> [Int] -> [Int] -> IO ()
execr [] l_a l_b
    | l_a == (sort l_a) && l_b == [] = putStrLn "OK"
    | otherwise = putStr "KO: " >> print (l_a, l_b)
execr (x:xs) y z
    | x == "ra" = execs xs (rotate y) z
    | x == "rb" = execs xs y (rotate z)
    | x == "rr" = execs xs (rotate y) (rotate z)
    | otherwise = execrr (x:xs) y z
    
execp :: [String] -> [Int] -> [Int] -> IO ()
execp [] l_a l_b
    | l_a == (sort l_a) && l_b == [] = putStrLn "OK"
    | otherwise = putStr "KO: " >> print (l_a, l_b)
execp (x:xs) l_a l_b
    | x == "pa" = execs xs (fst (takep l_a l_b)) (snd (takep l_a l_b))
    | x == "pb" = execs xs (snd (takep l_b l_a)) (fst (takep l_b l_a))
    | otherwise = execr (x:xs) l_a l_b

execs :: [String] -> [Int] -> [Int] -> IO ()
execs [] l_a l_b
    | l_a == (sort l_a) && l_b == [] = putStrLn "OK"
    | otherwise = putStr "KO: " >> print (l_a, l_b)
execs (x:xs) l_a l_b
    | x == "sa" = execs xs (swap l_a) l_b
    | x == "sb" = execs xs l_a (swap l_b)
    | x == "sc" = execs xs (swap l_a) (swap l_b)
    | otherwise = execp (x:xs) l_a l_b