module FPH
where
import Data.List
import Data.Char
double x  =  if x>100 then x else x*2
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]  
removeupper st = [ c | c <- st, c `elem` ['A'..'Z']]  
rightTriangles x = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 
remove st =[c| c<-st,c `elem`['A'..'Z']]
factorial :: Integer->Integer
factorial 0= 1
factorial n = n * factorial(n-1)
sayMe :: (Integral a) => a -> String  
sayMe 1 ="One"
sum' :: (Num d) => [d] -> d
sum' [] = 0
sum'(x:xs)=x+ sum' xs 
calBMI :: (RealFloat a)=>[(a,a)]->[a]
calBMI xs=[bmi w h |(w,h) <-xs]
	where bmi weight height = weight / height ^ 2
hword :: String -> Bool
hword[] = False
hword (x:xs)= (x=='h')||(hword xs)
gen :: Int -> String
gen 0 = "sentences can go on"
gen n = gen(n-1) ++ "and on (n)"

removest st = [c | c<-st ,c `notElem` ['A']]
reversal :: String ->String
reversal [] =[]
reversal (x:t) = reversal t ++ [x]


sonnet18 ="Shall I compare thee to a summer’s day? \n"
	++ "Thou art more lovely and more temperate: \n"
	++ "Rough winds do shake the darling buds of May, \n"
	++ "And summer’s lease hath all too short a date: \n"
	++ "Sometime too hot the eye of heaven shines, \n"
	++ "And often is his gold complexion dimm’d; \n"
	++ "And every fair from fair sometime declines, \n"
	++ "By chance or nature’s changing course untrimm’d; \n"
	++ "But thy eternal summer shall not fade \n"
	++ "Nor lose possession of that fair thou owest; \n"
	++ "Nor shall Death brag thou wander’st in his shade, \n"
	++ "When in eternal lines to time thou growest: \n"
	++ " So long as men can breathe or eyes can see, \n"
	++ " So long lives this and this gives life to thee."

average :: [Int] -> Rational
average [] = error "empty  "
average xs =toRational(sum xs) / toRational  (length xs)


count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) | x == y = succ (count x ys)| otherwise = count x ys



preprocess :: String -> String
preprocess = (map toLower) . filter (`notElem` "?;:,.")
process :: String -> [String]
process = sort . nub . words

cnt :: String -> [(String,Int)]
cnt sonnet = [ (x,n)| x <- (process.preprocess) sonnet,n <- [count x (words (preprocess sonnet))],n > 1 ]

doublMe x = x +x

removenonUpperCase :: [Char] -> [Char]
removenonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]   

 


