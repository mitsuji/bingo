import Data.List(foldl')

type Selection = [Int]
type Card = ([Int],[Int])
type CardResult = [Bool]

data CardStatus = Bingo
                | Lizhi Int
                | Blank
                deriving(Show)


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
h x l = foldr (\i acc -> (take 1 $ drop ((i*5)+x) l) ++ acc) [] [0..4]

lTrB :: [a] -> [a]
lTrB l = foldr (\i acc -> (take 1 $ drop (i*6) l) ++ acc) [] [0..4]

rTlB :: [a] -> [a]
rTlB l = foldr (\i acc -> (take 1 $ drop ((i+1)*4) l) ++ acc) [] [0..4]




test = do
  let cr = processCard ([1,2,3,4,5,6,7,8,9,10,11,12],[14,15,16,17,18,19,20,21,22,23,24,25]) [1,2,4,5,7,8,9,10,6]
  print cr
  print $ evalCard cr


testEc = do
  print $ evalCard $ map (\i-> i==1) [1,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalCard $ map (\i-> i==1) [1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalCard $ map (\i-> i==1) [1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0]
  print $ evalCard $ map (\i-> i==1) [1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,1,1]
  print $ evalCard $ map (\i-> i==1) [1,1,1,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,1,1]
  print $ evalCard $ map (\i-> i==1) [1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,1,1,1]

testEv = do
  print $ evalV 0 $ map (\i-> i==1) [1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalV 1 $ map (\i-> i==1) [0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalV 2 $ map (\i-> i==1) [0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalV 3 $ map (\i-> i==1) [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0]
  print $ evalV 4 $ map (\i-> i==1) [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0]

testEh = do
  print $ evalH 0 $ map (\i-> i==1) [1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0]
  print $ evalH 1 $ map (\i-> i==1) [0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0]
  print $ evalH 2 $ map (\i-> i==1) [0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0]
  print $ evalH 3 $ map (\i-> i==1) [0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0]
  print $ evalH 4 $ map (\i-> i==1) [0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0]

testEl = do
  print $ evalL $ map (\i-> i==1) [1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0]

testEr = do
  print $ evalR $ map (\i-> i==1) [0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0]


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

