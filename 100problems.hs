myLast :: [a] -> a
myLast [] =error "cannot be empty"
myLast [x] = x
myLast(_:xs) = myLast xs



myButLast :: [a]->a
myButLast [] = error "Cannot be empty"
myButLast [a] = error "more than one element required"
myButLast [x,_] = x 
myButLast (_:xs) = myButLast xs 

elementAt :: [a] ->Int -> a
elementAt [] _ = error "Out of range"
elementAt (x:_) 0 = x
elementAt (x:xs) n = elementAt xs (n-1)  

myLength :: [a] ->Int
myLength [] = 0
myLength (_:xs) =  1 + myLength xs


myReverse :: [a] -> [a]
myReverse xs = append xs []
  where append [] ys = ys
        append (x:xs_) ys = append xs_ (x:ys)


isPalindrome :: Eq a => [a] ->Bool
isPalindrome [] = True
isPalindrome xs = xs == myReverse xs 


data Foo = Bu | Fd 
instance  Eq Foo where
   (==) Bu Fd = False 
   (==) Fd Bu = False
   (==) _ _ = True


compress :: [a] ->[a]
compress []= error "empty list"
compress (y:(x:xs)) |  x==y  = compress xs | otherwise   

compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys





