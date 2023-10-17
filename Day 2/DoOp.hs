import System.Environment
import System.Exit

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys)
    | x == y = True
    | otherwise = myElem x ys

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (div x y)

safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (x:xs) 0 = Just x
safeNth (x:xs) i = safeNth xs (i-1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just x) = Just (x + 1)

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup x ((a1, a2):as)
    | x == a1 = Just a2
    | otherwise = myLookup x as

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ Nothing _ = Nothing
maybeDo _ _ Nothing = Nothing
maybeDo func (Just x) (Just y) = Just (func x y)

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

getLineLength :: IO Int
getLineLength = do
    l <- getLine
    return (length l)

borderLine :: Int -> Int -> IO ()
borderLine _ 0 = putStrLn "+"
borderLine x y
    | x == y = putStr "+" >> borderLine x (y-1)
    | otherwise = putStr "-" >> borderLine x (y-1)

printLines :: Int -> Int -> Int -> IO () --x=lenght of lines; y=remaining characters in line; z=remaining lines
printLines x 0 0 = putStrLn "|"
printLines x 0 z = putStrLn "|" >> printLines x x (z-1)
printLines x y z
    | x == y = putStr "|" >> printLines x (y-1) z
    | otherwise = putStr " " >> printLines x (y-1) z

printBox :: Int -> IO ()
printBox 1 = putStrLn "++"
printBox 2 = borderLine 4 4 >> borderLine 4 4
printBox x
    | x > 0 = borderLine (2*x-1) (2*x-1) >>
    printLines (2*x-1) (2*x-1) (x-2) >>
    borderLine (2*x-1) (2*x-1)
    | otherwise = return ()

concatLines :: Int -> IO String
concatLines 0 = return ("")
concatLines x = do
    line <- getLine
    text <- concatLines (x-1)
    return (line ++ text)

getInt :: IO (Maybe Int)
getInt = do
    line <- getLine
    if isNbr line == True
        then return (Just (read line))
        else return (Nothing) 

calculate :: Int -> [Char] -> Int -> IO Int
calculate x "/" y
    | y == 0 = exitWith (ExitFailure 84)
    | otherwise = print (div x y) >> return 0
calculate x "%" y
    | y == 0 = exitWith (ExitFailure 84)
    | otherwise = print (mod x y) >> return 0
calculate x ope y
    | ope == "+" = print(x+y) >> return 0
    | ope == "-" = print(x-y) >> return 0
    | ope == "*" = print (x*y) >> return 0
    | otherwise = exitWith (ExitFailure 84)

operate :: [String] -> IO Int
operate [x, ope, y]
    | (isNbr x == True) && (isNbr y == True) = calculate (read x :: Int) ope (read y :: Int)
    | otherwise = exitWith (ExitFailure 84)

-- Makefile tes main
main :: IO Int
main = do
    args <- getArgs
    case length args of
        3 -> operate args
        _ -> exitWith (ExitFailure 84)