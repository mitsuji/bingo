{-# LANGUAGE PackageImports #-}
module Lottery (
  draw,
  pick,
  ) where


import qualified System.Random as R

import Control.Applicative ((<$>))
import "mtl" Control.Monad.RWS (execRWS,get,tell,put)


--
-- rem から 1個ランダムに取り出し, sel に追加する
-- 取り出したものと新しい状態を返す
--
draw :: R.RandomGen g => g -> ([a],[a]) -> (a,([a],[a]))
draw g (rem,sel) = (x,((reduce rem n),x:sel))
  where
    x = rem !! n
    (n,_) = R.randomR (0,((length rem)-1)) g



pick :: R.RandomGen g => g -> [a] -> Int -> ([a],g)
pick g xs c = (\((g,_),w) -> (w,g) ) $ execRWS (
  do
    let c' = (length xs) - c
    whileDo ( (c'<) . length . snd <$> get) $ do
      (g',xs) <- get
      let (i,g'') = R.randomR (0, (length xs)-1) g'
      tell [xs !! i]
      put (g'', reduce xs i)
  ) () (g,xs)
  where

    whileDo :: Monad m => m Bool -> m () -> m ()
    whileDo fcond fbody = loop
      where
        loop = do
          c <- fcond
          if c
            then fbody >> loop
            else return ()        


reduce :: [a] -> Int -> [a]
reduce xs n = (take n xs) ++ (drop (n+1) xs)

