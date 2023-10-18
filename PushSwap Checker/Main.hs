import System.Environment
import System.Exit
import Data.List
import Commands

--Checking arguments section

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys)
    | x == y = True
    | otherwise = myElem x ys

isNbr :: [Char] -> Bool
isNbr [] = False
isNbr (x:[]) = myElem x "1234567890"
isNbr (x:xs)
    | myElem x "1234567890" == True = isNbr xs
    | otherwise = False

readInt :: [Char] -> Maybe Int
readInt x = case isNbr x of
    True -> Just (read x :: Int)
    False -> Nothing

checkArgs :: [String] -> IO Int
checkArgs [] = return 0
checkArgs ((x:xs):ys)
    | x == '-' && (readInt xs) /= Nothing = checkArgs ys
    | (readInt (x:xs)) /= Nothing = checkArgs ys
    | otherwise = putStrLn "Invalid argument found" >> exitWith (ExitFailure 84)

emptyArgs :: [String] -> IO Int
emptyArgs [] = putStrLn "No arguments" >> exitWith (ExitFailure 84)
emptyArgs x = checkArgs x

-- End of argument checking section

-- Result printing

main :: IO ()
main = do
    commandLine <- getLine
    args <- getArgs
    emptyArgs args
    let commands = words commandLine
    let nbr = map (read::String->Int) args
    execs commands nbr []