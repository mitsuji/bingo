import qualified System.Random as R
import Data.Monoid(Monoid,mempty,mappend,mconcat)
import qualified Lottery as Lot




type Card = ([Int],[Int])


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
  let (x,s'@(_,sel)) = Lot.draw g s
  let cr1 = processCard card1 sel
  let cr2 = processCard card2 sel
  let cr3 = processCard card3 sel
  
  putStrLn $ (show turn) ++ ": x: " ++ (show x)
  putStrLn $ (show turn) ++ ": sel: " ++ (show sel)
  putStrLn $ (show turn) ++ ": card1: " ++ (show $ sbl cr1)
  putStrLn $ (show turn) ++ ": card2: " ++ (show $ sbl cr2)
  putStrLn $ (show turn) ++ ": card3: " ++ (show $ sbl cr3)
  putStrLn $ (show turn) ++ ": eval1: " ++ (show $ evalCard cr1)
  putStrLn $ (show turn) ++ ": eval2: " ++ (show $ evalCard cr2)
  putStrLn $ (show turn) ++ ": eval3: " ++ (show $ evalCard cr3)

  return s'
  
  where
    sbl = map (\b -> case b of
                  True -> 1
                  False -> 0)



      
newCandidate :: R.RandomGen g => g -> [Int]
newCandidate g = Lot.pick g [0..99] 30


newCard :: R.RandomGen g => g -> [Int] -> Card
newCard g cs = splitAt 12 $ Lot.pick g cs 24


processCard :: Card -> [Int] -> [Bool]
processCard (sect1, sect2) sel =
  (map (\i -> elem i sel ) sect1) ++ [True] ++ (map (\i -> elem i sel ) sect2)


evalCard :: [Bool] -> CardStatus
evalCard =
  mconcat $ (map evalH [0..4]) ++ (map evalV [0..4]) ++ [evalLT2RB, evalRT2LB]

  
evalH n cr = eval $ filterH n cr
evalV n cr = eval $ filterV n cr
evalLT2RB = eval . filterLT2RB
evalRT2LB = eval . filterRT2LB


eval :: [Bool] -> CardStatus
eval cr = 
  case length $ filter id cr of
    5 -> Bingo
    4 -> Lizhi 1
    otherwise -> Blank


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




test = do
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

