module Bingo (
  Card,
  State,
  CardStatus,
  newCandidate,
  newCard,
  processCard,
  evalCard,
  ) where

import qualified System.Random as R
import Data.Monoid(Monoid,mempty,mappend,mconcat)
import qualified Lottery as Lot



type Card = ([Int],[Int])
type State = ([Int],[Int])


data CardStatus = Bingo
                | Lizhi Int
                | Blank
                deriving(Show)
instance Monoid CardStatus where
  mempty = Blank
  mappend Bingo _ = Bingo
  mappend _ Bingo = Bingo
  mappend (Lizhi n1) (Lizhi n2) = Lizhi (n1 + n2)
  mappend l@(Lizhi _) _ = l
  mappend _ l@(Lizhi _) = l
  mappend _  _ = Blank



      
newCandidate9 g = newCandidate g 9
newCandidate25 g = newCandidate g 25
newCandidate49 g = newCandidate g 49


newCard9 g = newCard g 9
newCard25 g = newCard g 25
newCard49 g = newCard g 49


newCandidate :: R.RandomGen g => g -> Int -> [Int]
newCandidate g n = Lot.pick g [0..99] $ truncate $ (fromIntegral n) * 1.4


newCard :: R.RandomGen g => g -> Int -> [Int] -> Card
newCard g n cs = splitAt h $ Lot.pick g cs t
  where
    t = n-1
    h = div t 2


processCard :: Card -> [Int] -> [Bool]
processCard (sect1, sect2) sel =
  (map (\i -> elem i sel ) sect1) ++ [True] ++ (map (\i -> elem i sel ) sect2)


evalCard :: [Bool] -> CardStatus
evalCard xs =
  (mconcat $ (map evalH [0..(l-1)]) ++ (map evalV [0..(l-1)]) ++ [evalLT2RB, evalRT2LB]) xs
  where
    l = truncate $ sqrt $ fromIntegral $ length xs

  
evalH n cr = eval $ filterH n cr
evalV n cr = eval $ filterV n cr
evalLT2RB = eval . filterLT2RB
evalRT2LB = eval . filterRT2LB


eval :: [Bool] -> CardStatus
eval xs = 
  case length $ filter id xs of
    n | n == l     -> Bingo
      | n == (l-1) -> Lizhi 1
      | otherwise  -> Blank
  where
    l = length xs
    

filterH :: Int -> [a] -> [a]
filterH n xs = take l $ drop (n*l) xs
  where
    l = truncate $ sqrt $ fromIntegral $ length xs

filterV :: Int -> [a] -> [a]
filterV n xs = foldr (\i acc -> (xs !! ((i*l)+n)) : acc) [] [0..(l-1)]
  where
    l = truncate $ sqrt $ fromIntegral $ length xs

filterLT2RB :: [a] -> [a]
filterLT2RB xs = foldr (\i acc -> (xs !! (i*(l+1))) : acc) [] [0..(l-1)]
  where
    l = truncate $ sqrt $ fromIntegral $ length xs

filterRT2LB :: [a] -> [a]
filterRT2LB xs = foldr (\i acc -> (xs !! ((i+1)*(l-1))) : acc) [] [0..(l-1)]
  where
    l = truncate $ sqrt $ fromIntegral $ length xs




testP = do
  let cr = processCard ([1,2,3,4,5,6,7,8,9,10,11,12],[14,15,16,17,18,19,20,21,22,23,24,25]) (10:9:8:7:5:4:2:1:[])
  print $ sbl cr
  print $ evalCard cr
  where
    sbl = map (\b -> case b of
                  True -> 1
                  False -> 0)

testEc = do
  print $ evalCard $ map (==1) [1,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalCard $ map (==1) [1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalCard $ map (==1) [1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0]
  print $ evalCard $ map (==1) [1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,1,1]
  print $ evalCard $ map (==1) [1,1,1,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,1,1]
  print $ evalCard $ map (==1) [1,1,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,1,1,1]

testEh = do
  print $ evalH 0 $ map (==1) [1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalH 1 $ map (==1) [0,0,0,0,0,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalH 2 $ map (==1) [0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0]
  print $ evalH 3 $ map (==1) [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0]
  print $ evalH 4 $ map (==1) [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0]

testEv = do
  print $ evalV 0 $ map (==1) [1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0]
  print $ evalV 1 $ map (==1) [0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0]
  print $ evalV 2 $ map (==1) [0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0]
  print $ evalV 3 $ map (==1) [0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0]
  print $ evalV 4 $ map (==1) [0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0]

testEl = do
  print $ evalLT2RB $ map (==1) [1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0]

testEr = do
  print $ evalRT2LB $ map (==1) [0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0]


testh = do
  print $ filterH 0 [1,2,3,4,5,6,7,8,9]
  print $ filterH 1 [1,2,3,4,5,6,7,8,9]
  print $ filterH 2 [1,2,3,4,5,6,7,8,9]

  print $ filterH 0 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ filterH 1 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ filterH 2 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ filterH 3 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ filterH 4 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]


testv = do
  print $ filterV 0 [1,2,3,4,5,6,7,8,9]
  print $ filterV 1 [1,2,3,4,5,6,7,8,9]
  print $ filterV 2 [1,2,3,4,5,6,7,8,9]

  print $ filterV 0 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ filterV 1 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ filterV 2 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ filterV 3 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
  print $ filterV 4 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]


testl = do
  print $ filterLT2RB [1,2,3,4,5,6,7,8,9]
  
  print $ filterLT2RB [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]


testr = do
  print $ filterRT2LB [1,2,3,4,5,6,7,8,9]

  print $ filterRT2LB [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]

