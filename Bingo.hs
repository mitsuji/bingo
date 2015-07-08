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
  let c = cd g [0..99] 34
  print c
  
  g <- R.newStdGen
  print $ cd g c 24
  
  g <- R.newStdGen
  print $ cd g c 24
  
  g <- R.newStdGen
  print $ cd g c 24


--
-- xs から c個 ランダムに取り出す
--
cd :: R.RandomGen g => g -> [Int] -> Int -> [Int]
cd g xs c = sel xs $ rs g (length xs -1) c


-- xs から rsの要素番目の値を順次取り出す
-- xs 候補リスト
-- rs Int乱数リスト
sel ::  [Int] -> [Int] -> [Int]
sel xs rs = f1 xs rs []
  where
    f1 :: [a] -> [Int] -> [a] -> [a]
    f1 _ [] acc = acc
    f1 xs (r:rs) acc = f1 (td xs r) rs ((xs !! r):acc)

    td :: [a] -> Int -> [a] -- xs から n 番めを除去
    td xs n = (take n xs) ++ (drop (n+1) xs)


-- 0 から m, m-1, m-2, ... のInt乱数を c個生成する
-- m は1個ずつ減る
rs :: R.RandomGen g => g -> Int -> Int -> [Int]
rs g m c = map ( \(i,f)-> intR (m-i) f ) $ zip [0..(c-1)] (R.randoms g :: [Float])
  where
    intR :: Int -> Float -> Int -- Floatの乱数(f)を上限mのIntの乱数に変換
    intR m f = truncate $ (fromIntegral (m+1)) * f 





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

