import qualified System.Random as R
import Data.List(foldl')

type Candidate = [Int]
type Selection = [Int]
type Card = ([Int],[Int])
type CardResult = [Bool]

data CardStatus = Bingo
                | Lizhi Int
                | Blank
                deriving(Show)




main = do
  g <- R.newStdGen
  let cs = newCandidate g
  
  g <- R.newStdGen
  let card1 = newCard g cs 
  
  g <- R.newStdGen
  let card2 = newCard g cs
      
  g <- R.newStdGen
  let card3 = newCard g cs 
  
  putStrLn $ "cs: " ++ (show cs)
  putStrLn $ "card1: " ++ (show card1)
  putStrLn $ "card2: " ++ (show card2)
  putStrLn $ "card3: " ++ (show card3)

  s <- doBingo 1 card1 card2 card3 (cs,[])
  s <- doBingo 2 card1 card2 card3 s
  s <- doBingo 3 card1 card2 card3 s
  s <- doBingo 4 card1 card2 card3 s
  s <- doBingo 5 card1 card2 card3 s
  s <- doBingo 6 card1 card2 card3 s
  s <- doBingo 7 card1 card2 card3 s
  s <- doBingo 8 card1 card2 card3 s
  s <- doBingo 9 card1 card2 card3 s
  s <- doBingo 10 card1 card2 card3 s
  s <- doBingo 11 card1 card2 card3 s
  s <- doBingo 12 card1 card2 card3 s
  s <- doBingo 13 card1 card2 card3 s
  s <- doBingo 14 card1 card2 card3 s
  s <- doBingo 15 card1 card2 card3 s
  s <- doBingo 16 card1 card2 card3 s
  s <- doBingo 17 card1 card2 card3 s
  s <- doBingo 18 card1 card2 card3 s
  s <- doBingo 19 card1 card2 card3 s
  s <- doBingo 20 card1 card2 card3 s

  return ()
  

doBingo turn card1 card2 card3 s = do
  g <- R.newStdGen
  let s'@(rem,sel) = pickRandom g s
  let cr1 = processCard card1 sel
  let cr2 = processCard card2 sel
  let cr3 = processCard card3 sel
  
  putStrLn $ (show turn) ++ ": sel: " ++ (show sel)
  putStrLn $ (show turn) ++ ": card1: " ++ (show $ sbl cr1)
  putStrLn $ (show turn) ++ ": card2: " ++ (show $ sbl cr2)
  putStrLn $ (show turn) ++ ": card3: " ++ (show $ sbl cr3)
  putStrLn $ (show turn) ++ ": eval1: " ++ (show $ evalCard cr1)
  putStrLn $ (show turn) ++ ": eval2: " ++ (show $ evalCard cr2)
  putStrLn $ (show turn) ++ ": eval3: " ++ (show $ evalCard cr3)

  return s'
  

sbl = map (\b -> case b of
              True -> "T"
              False -> "F")


newCandidate :: R.RandomGen g => g -> [Int]
newCandidate g = selectRandom g [0..99] 30

newCard :: R.RandomGen g => g -> [Int] -> Card
newCard g cs = splitAt 12 $ selectRandom g cs 24


pickRandom :: R.RandomGen g => g -> ([Int],[Int]) -> ([Int],[Int])
pickRandom g (rem,sel) = ((reduce rem n),(rem !! n):sel)
  where
    (n,_) = R.randomR (0,((length rem)-1)) g


--
-- xs から c個 ランダムに取り出す
--
selectRandom :: R.RandomGen g => g -> [Int] -> Int -> [Int]
selectRandom g xs c = gradualSelect xs (decrescIntRandom g ((length xs)-1) c) []


-- xs から {rsの要素}番目の値を順次取り出す
-- xs  候補リスト
-- rs  Int乱数リスト
-- acc アキュムレータ
gradualSelect :: [a] -> [Int] -> [a] -> [a]
gradualSelect _ [] acc = acc
gradualSelect xs (r:rs) acc = gradualSelect (reduce xs r) rs ((xs !! r):acc)


-- [0 -> m, 0 -> (m-1), 0 -> (m-2), ...] と範囲が順次減少するInt乱数リストを c個生成
-- g 乱数発生器
-- m 上限値の初期値
-- c 生成するリストの長さ
decrescIntRandom :: R.RandomGen g => g -> Int -> Int -> [Int]
decrescIntRandom g m c = map ( \(i,f)-> toInt (m-i) f ) $ zip [0..(c-1)] (R.randoms g :: [Float])
  where
    toInt :: Int -> Float -> Int -- Floatの乱数(f)を上限mのInt乱数に変換
    toInt m f = truncate $ (fromIntegral (m+1)) * f 


-- xs から n 番めを除去
reduce :: [a] -> Int -> [a]
reduce xs n = (take n xs) ++ (drop (n+1) xs)



(+++) :: CardStatus -> CardStatus -> CardStatus
(+++) Bingo _ = Bingo
(+++) _ Bingo = Bingo
(+++) (Lizhi n1) (Lizhi n2) = Lizhi (n1 + n2)
(+++) l@(Lizhi _) _ = l
(+++) _ l@(Lizhi _) = l
(+++) _ _ = Blank



processCard :: Card -> Selection -> CardResult
processCard (card1, card2) sel =
  (map (\i -> elem i sel ) card1) ++ [True] ++ (map (\i -> elem i sel ) card2)


evalCard :: CardResult -> CardStatus
evalCard cr =
  (foldl' (\acc i -> acc +++ evalV i cr ) Blank [0..4])
  +++ (foldl' (\acc i -> acc +++ evalH i cr ) Blank [0..4])
  +++ evalL cr +++ evalR cr

  
evalV n cr = eval $ v n cr
evalH n cr = eval $ h n cr
evalL = eval . lTrB
evalR = eval . rTlB


eval :: CardResult -> CardStatus
eval cr = 
  case length $ filter id cr of
    5 -> Bingo
    4 -> Lizhi 1
    otherwise -> Blank


v :: Int -> [a] -> [a]
v y l = take 5 $ drop (y*5) l

h :: Int -> [a] -> [a]
h x l = foldr (\i acc -> (l !! ((i*5)+x)) : acc) [] [0..4]

lTrB :: [a] -> [a]
lTrB l = foldr (\i acc -> (l !! (i*6)) : acc) [] [0..4]

rTlB :: [a] -> [a]
rTlB l = foldr (\i acc -> (l !! ((i+1)*4)) : acc) [] [0..4]




test = do
  let cr = processCard ([1,2,3,4,5,6,7,8,9,10,11,12],[14,15,16,17,18,19,20,21,22,23,24,25]) [1,2,4,5,7,8,9,10,6]
  print cr
  print $ evalCard cr


testEc = do
  print $ evalCard $ map (==1) [1,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalCard $ map (==1) [1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalCard $ map (==1) [1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0]
  print $ evalCard $ map (==1) [1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,1,1]
  print $ evalCard $ map (==1) [1,1,1,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,1,1]
  print $ evalCard $ map (==1) [1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,1,1,1]

testEv = do
  print $ evalV 0 $ map (==1) [1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalV 1 $ map (==1) [0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalV 2 $ map (==1) [0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalV 3 $ map (==1) [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0]
  print $ evalV 4 $ map (==1) [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0]

testEh = do
  print $ evalH 0 $ map (==1) [1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0]
  print $ evalH 1 $ map (==1) [0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0]
  print $ evalH 2 $ map (==1) [0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0]
  print $ evalH 3 $ map (==1) [0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0]
  print $ evalH 4 $ map (==1) [0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0]

testEl = do
  print $ evalL $ map (==1) [1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0]

testEr = do
  print $ evalR $ map (==1) [0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0]


testv = do
  print $ v 0 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ v 1 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ v 2 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ v 3 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ v 4 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]

testh = do
  print $ h 0 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ h 1 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ h 2 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ h 3 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ h 4 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]

testl = do
  print $ lTrB [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]

testr = do
  print $ rTlB [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]

