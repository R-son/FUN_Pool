myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit (x:[]) = []
myInit (x:xs) = x:(myInit xs)

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter func (x:xs) 
    | func x = x:(myFilter func xs)
    | otherwise = myFilter func xs

myNth :: [a] -> Int -> a 
myNth [] _ = error "Index does not exist !"
myNth (x:xs) 0 = x
myNth (x:xs) i = myNth xs (i-1)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl func x [] = x
myFoldl func x (y:ys) = myFoldl func (func x y) ys

myFoldr :: (a -> b -> b) -> b -> [a] ->  b
myFoldr func x [] = x
myFoldr func x y = myFoldr func (func (myLast y) x) (myInit y)

myPartition :: (a -> Bool) -> [a] ->([a], [a])
myPartition _ [] = ([],[])
myPartition func x = ((myFilter func x),(myFilter (not.func) x))

myAppend :: [a] -> [a] -> [a]
myAppend [] y = y
myAppend (x:xs) y = x:myAppend xs y

myQuickSort ::  (a -> a -> Bool) -> [a] -> [a]
myQuickSort _ [] = []
myQuickSort func (x:xs) = myAppend arg2 (myAppend [x] arg1)
    where
        arg1 = myQuickSort func (myFilter (func x) xs)
        arg2 = myQuickSort func (myFilter (not.func x) xs)