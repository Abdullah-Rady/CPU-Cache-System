data Binary = B Int Binary | B0 | B1 deriving Show
len:: Binary -> Int
len(B0) = 1
len(B1) = 1
len (B _ (x)) = 1 + len(x)
divisionBin b 0 = b
divisionBin b a = divisionBin' (len(b) - ceiling(log a/log 2)) b 1
divisionBin' b (B x (xs)) p = if b == p then if x == 1 then ((B1)) else ((B0)) else B x (divisionBin' b xs (p+1))
