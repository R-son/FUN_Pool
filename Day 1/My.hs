mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x = x < 0

myAbs :: Int -> Int
myAbs x
    | x < 0 = -x
    | otherwise = x

myMin :: Int -> Int -> Int
myMin x y
    | x <= y = x
    | otherwise = y

myMax :: Int -> Int -> Int
myMax x y
    | x >= y = x
    | otherwise = y

myTuple :: a -> b -> (a, b)
myTuple x y = (x, y)

myTruple :: a -> b -> c -> (a, b, c)
myTruple x y z = (x, y, z)

myFst :: (a, b) -> a
myFst (x, y) = x

mySnd  :: (a, b) -> b
mySnd (x, y) = y

mySwap :: (a, b) -> (b, a)
mySwap (x, y) = (y, x)

myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (x:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myNth :: [a] -> Int -> a 
myNth [] _ = error "Index does not exist !"
myNth (x:xs) 0 = x
myNth (x:xs) i = myNth xs (i-1)

myTake :: Int -> [a] -> [a]
myTake 0 x = []
myTake n (x:xs)
    | n < 0 = error "Negative value"
    | n > myLength (x:xs) = (x:xs)
    | otherwise = x:(myTake (n-1) xs)

myDrop :: Int -> [a] -> [a]
myDrop 0 x = x
myDrop n (x:xs)
    | n < 0 = error "Negative value"
    | n > myLength (x:xs) = []
    | otherwise = myDrop (n-1) xs

myAppend :: [a] -> [a] -> [a]
myAppend [] y = y
myAppend (x:xs) y = x:myAppend xs y

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit (x:[]) = []
myInit (x:xs) = x:(myInit xs)

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = myAppend [(x, y)] (myZip xs ys)

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([],[])
myUnzip ((x, y):xs) = (x:fst, y:snd)
    where
        fst = myFst (myUnzip xs)
        snd = mySnd (myUnzip xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter func (x:xs) 
    | func x = x:(myFilter func xs)
    | otherwise = myFilter func xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap func (x:xs) = (func x):(myMap func xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl func x [] = x
myFoldl func x (y:ys) = myFoldl func (func x y) ys

myFoldr :: (a -> b -> b) -> b -> [a] ->  b
myFoldr func x [] = x
myFoldr func x y = myFoldr func (func (myLast y) x) (myInit y)

myPartition :: (a -> Bool) -> [a] ->([a], [a])
myPartition _ [] = ([],[])
myPartition func x = ((myFilter func x),(myFilter (not.func) x))


myQuickSort ::  (a -> a -> Bool) -> [a] -> [a]
myQuickSort _ [] = []
myQuickSort func (x:xs) = myAppend arg2 (myAppend [x] arg1)
    where
        arg1 = myQuickSort func (myFilter (func x) xs)
        arg2 = myQuickSort func (myFilter (not.func x) xs)