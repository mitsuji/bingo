import Control.Monad(foldM_)
import qualified System.Random as R
import qualified Bingo as B
import qualified Lottery as Lot



main = test



test = do
  (card1, card2, card3, ss) <- prepareBingo 25
  foldM_ (\acc i -> doBingo i card1 card2 card3 acc) ss [1..20]


prepareBingo :: Int -> IO (B.Card,B.Card,B.Card,B.State)
prepareBingo n = do
  g <- R.newStdGen
  let cs = B.newCandidate g n
  
  g <- R.newStdGen
  let card1 = B.newCard g n cs 
  
  g <- R.newStdGen
  let card2 = B.newCard g n cs
      
  g <- R.newStdGen
  let card3 = B.newCard g n cs 
  
  putStrLn $ "cs: " ++ (show cs)
  putStrLn $ "card1: " ++ (show card1)
  putStrLn $ "card2: " ++ (show card2)
  putStrLn $ "card3: " ++ (show card3)

  return (card1,card2,card3,(cs,[]))


doBingo :: Int -> B.Card -> B.Card -> B.Card -> B.State -> IO B.State
doBingo n card1 card2 card3 st = do
  g <- R.newStdGen
  let (x,st'@(_,ss)) = Lot.draw g st
  let r1 = B.processCard card1 ss
  let r2 = B.processCard card2 ss
  let r3 = B.processCard card3 ss
  
  putStrLn $ (show n) ++ ": x: " ++ (show x)
  putStrLn $ (show n) ++ ": ss: " ++ (show ss)
  putStrLn $ (show n) ++ ": r1: " ++ (show $ sbl r1)
  putStrLn $ (show n) ++ ": r2: " ++ (show $ sbl r2)
  putStrLn $ (show n) ++ ": r3: " ++ (show $ sbl r3)
  putStrLn $ (show n) ++ ": e1: " ++ (show $ B.evalCard r1)
  putStrLn $ (show n) ++ ": e2: " ++ (show $ B.evalCard r2)
  putStrLn $ (show n) ++ ": e3: " ++ (show $ B.evalCard r3)

  return st'
  
  where
    sbl = map (\b -> case b of
                  True -> 1
                  False -> 0)


