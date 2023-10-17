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

-- Makefile test main
main :: IO ()
main = return ()