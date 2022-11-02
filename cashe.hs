data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)

convertBinToDec :: Integral i => i -> i
convertBinToDec 0 = 0
convertBinToDec i = 2 * convertBinToDec (div i 10) + (mod i 10)

replaceIthItem _ [] _ = []
replaceIthItem newVal (x:xs) n  | n == 0 = newVal:xs  
                | otherwise = x:replaceIthItem newVal xs (n-1)

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

logBase2 num = logBase 2 num

getNumBits numOfSets cacheType _ = if cacheType == "fullyAssoc" then 0
                              else ceiling (logBase2 numOfSets)

fillZeros :: (Eq a, Num a) => [Char] -> a -> [Char] 
fillZeros x 0 = x 
fillZeros x a  = ('0' : fillZeros x (a-1))


twopower 0 = 1
twopower x = 2 * twopower (x-1)

tenpower 0 = 1
tenpower x = 10 * tenpower (x-1)

concat1 a b = read((show) a ++ (show) b)

convertStringToInt x = (read :: String -> Int) x



getSet cache bitsNum index = (splitEvery (twopower bitsNum) cache) !! index

validGet [] _ = -1
validGet ((It _ _ v _):xs) ind  | v == True = validGet xs (ind + 1)
                  | v == False = ind
                  | otherwise = -1

orderGet [] _ _ indM = indM
orderGet ((It _ _ _ o1):xs) maxO ind indM   | o1 > maxO = orderGet xs o1 (ind + 1) ind
                      | o1 <= maxO = orderGet xs maxO (ind + 1) indM
                      | otherwise = indM
orderAdder [] = []
orderAdder ((It t d v o):xs) | v==True = It t d v (o+1):orderAdder xs
               | v==False = It t d v o:orderAdder xs



--replaceIthItem a (h:t) i = replaceIthItemh a 0 (h:t) i
--replaceIthItemh a pos (h:t) i = if pos == i then (a:t) else h : replaceIthItemh a (pos+1) (t) i



convertAddress address bitsNum "fullyAssoc" = (address, bitsNum)
convertAddress binAddress bitsNum "setAssoc" = (div binAddress (tenpower bitsNum), mod binAddress (tenpower bitsNum)) 


getDataFromCache address cache "fullyAssoc" 0 = getDataFromCacheh (convertStringToInt address) cache "fullyAssoc" 0 0

getDataFromCache stringAddress cache "setAssoc" bitsNum = getDataFromCacheHelper2 (reverse stringAddress) cache "setAssoc" bitsNum
  (convertBinToDec (read  (reverse (take bitsNum (reverse stringAddress))) :: Int))

getDataFromCacheh _ [] "fullyAssoc" 0 _ = NoOutput
getDataFromCacheh address ((It (T tag) (D dat) valid _ ): t) "fullyAssoc" 0 hopsnum = if (address==tag && valid) then Out (dat, hopsnum) else getDataFromCacheh address (t) "fullyAssoc" 0 (hopsnum+1) 


getDataFromCacheHelper2 stringAddress cache "setAssoc" bitsNum index = getDataFromCacheHelper index (reverse (drop bitsNum stringAddress)) 
  (getSet cache bitsNum index)  "setAssoc"

getDataFromCacheHelper _ _ [] "setAssoc" = NoOutput
getDataFromCacheHelper index tag ((It (T currtag) (D currdata) valid _ ): y) "setAssoc" = if ((read tag :: Int) == currtag && valid) then Out (currdata, index)
                                                                                                    else getDataFromCacheHelper index tag (y) "setAssoc"

replaceInCache tag index memory oldCache "directMap" bitsNum = ((memory !! (convertBinToDec (concat1 tag index))) , 
  replaceIthItem (It (T tag) (D(memory !! (convertBinToDec (concat1 tag index)))) True 0) oldCache (convertBinToDec index))

replaceInCache tag _ memory oldCache "fullyAssoc" bitsNum | (validGet oldCache 0) /= -1 = ((memory !! (convertBinToDec tag)) ,
  replaceIthItem (It (T tag) (D(memory !! (convertBinToDec tag))) True 0) (orderAdder(oldCache)) (validGet oldCache 0) )
  | otherwise = ((memory !! (convertBinToDec tag)) ,
  replaceIthItem (It (T tag) (D(memory !! (convertBinToDec tag))) True 0) (orderAdder(oldCache)) (orderGet oldCache (-1) 0 0) )

replaceInCache tag idx memory oldCache "setAssoc" bitsNum = ((memory !! (convertBinToDec (concat1 tag idx))) ,
 concat(replaceIthItem (replaceInCacheAH tag idx memory (getSet oldCache bitsNum idx) "setAssoc" bitsNum) (splitEvery (ceiling (fromIntegral (length oldCache) / (2**bitsNum))) oldCache) (convertBinToDec idx)))

replaceInCacheAH tag _ memory oldCache "fullyAssoc" bitsNum | (validGet oldCache 0) /= -1 = replaceIthItem (It (T tag) (D(memory !! (convertBinToDec tag))) True 0) (orderAdder(oldCache)) (validGet oldCache 0)
  | otherwise = replaceIthItem (It (T tag) (D(memory !! (convertBinToDec tag))) True 0) (orderAdder(oldCache)) (orderGet oldCache (-1) 0 0)

replaceInCacheAH tag idx memory oldCache "setAssoc" bitsNum | (validGet oldCache 0) /= -1 = replaceIthItem (It (T tag) (D(memory !! (convertBinToDec (concat1 tag idx)))) True 0) (orderAdder(oldCache)) (validGet oldCache 0)
  | otherwise = replaceIthItem (It (T tag) (D(memory !! (convertBinToDec (concat1 tag idx)))) True 0) (orderAdder(oldCache)) (orderGet oldCache (-1) 0 0)





getData stringAddress cache memory cacheType bitsNum
                                                    | x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
                                                    | otherwise = (getX x, cache)
                                                    where 
                                                        x = getDataFromCache stringAddress cache cacheType bitsNum 
                                                        address = read stringAddress:: Int
                                                        (tag, index) = convertAddress address bitsNum cacheType 
getX (Out (d, _)) = d


runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets = ((d:prevData), finalCache)
  where
    bitsNum = round(logBase2 numOfSets)
    (d, updatedCache) = getData addr cache memory cacheType bitsNum
    (prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets