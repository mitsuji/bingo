{-# LANGUAGE PackageImports #-}
module Lottery (
  draw,
  pick,
  pick',
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


--
-- xs から c個 ランダムに取り出す
--
pick :: R.RandomGen g => g -> [a] -> Int -> [a]
pick g xs c = gradualSelect xs (decrescIntRandom g ((length xs)-1) c) []


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



pick' :: R.RandomGen g => g -> [a] -> Int -> ([a],g)
pick' g xs c = (\((g,_),w) -> (w,g) ) $ execRWS (
  do
    let c' = (length xs) - c
    whileDo ( (c'<) . length . snd <$> get) $ do
      (g',xs) <- get
      let (i,g'') = R.randomR (0, (length xs)-1) g'
      tell [xs !! i]
      put (g'', reduce xs i)
  ) () (g,xs)
  where

    reduce :: [a] -> Int -> [a]
    reduce xs n = (take n xs) ++ (drop (n+1) xs)

    whileDo :: Monad m => m Bool -> m () -> m ()
    whileDo fcond fbody = loop
      where
        loop = do
          c <- fcond
          if c
            then fbody >> loop
            else return ()        
