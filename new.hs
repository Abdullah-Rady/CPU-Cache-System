import Data.List

decToBin x = reverse (decToBin' x)
decToBin' 0 = []
decToBin' y = let (a,b) = quotRem y 2 in b : decToBin' a

data ListItem a = Single a | Multiple Int a
    deriving (Show)

decode :: [ListItem a] -> [a]
decode [] = []
decode ((Single x):xs) = x:decode xs
decode ((Multiple 2 x):xs) = x:x:decode xs
decode ((Multiple n x):xs) = x:decode ((Multiple (n-1) x):xs)


dupli:: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli:: (Integral a) => [b] -> a -> [b]
repli [] _ = []
repli (x:xs) a = (dupl x a) ++ repli xs a

dupl:: (Integral a) => b -> a -> [b]
dupl _ 0 = []
dupl x a = x:dupl x (a-1)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x: xs)) = flatten (x) ++ flatten (List xs)

compress::(Eq b) => [b] -> [b]
compress [x] = [x]
compress (x:y:xs) = if x /= y then x : compress(y:xs) else compress(y:xs)


data Binary = B Int Binary | B0 | B1 deriving Show

myLength :: Binary -> Int
myLength list = myLength_acc list 0
myLength_acc B0 n = (n + 1)
myLength_acc B1 n = (n + 1)
myLength_acc (B _ (bin)) n = myLength_acc bin (n + 1)

divisionBin a 0 = a
divisionBin list a = divisionBinhelper list ((myLength list) - floor(log a / log 2)) 0
divisionBinhelper (B x (bin)) a b = if b == (a - 1) then divisionBinhelper1 (B x (bin)) else B x (divisionBinhelper (bin) a (b+1))
divisionBinhelper1 (B x (bin)) = if x == 1 then B1 else B0

outt s list= (s, list) 


quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
