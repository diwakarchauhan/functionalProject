{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -XBangPatterns #-}
module TwoD where

import Data.Array.Repa
import System.Time
import Data.List
import Data.Ord
import Data.Array.Repa.Algorithms.Randomish


----------------------------------------------------------------------------------------------------------------------------------------------------

mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

harmean :: (Floating a) => [a] -> a
harmean xs = fromIntegral (length xs) / (sum $ Data.List.map (1/) xs)

geomean :: (Floating a) => [a] -> a
geomean xs = (foldr1 (*) xs)**(1 / fromIntegral (length xs))

median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

----------------------------------------------------------------------------------------------------------------------------------------------------

createArr :: Int -> Int -> (Array U DIM2 Double)
createArr m n = randomishDoubleArray (Z :.(m::Int):.(n::Int)) 0 255 1
-------------------------------------------------------------------
setBoundaryAll :: (Array U ((Z :. Int) :. Int) Double ) -> Double->(IO (Array U DIM2 Double))
setBoundaryAll array val = computeP $ traverse array id (\f (Z:. i:. j) -> if j == 0 || i == 0 || i == dimOne -1 || j == dimTwo -1 then val
                                                                                        else f(Z:. i :. j))
    where dimOne = dim !! 0
          dimTwo = dim !! 1
          dim = (listOfShape (extent array))                                                                                                                                   

setBoundaryLeft :: (Array U ((Z :. Int) :. Int) Double ) -> Double ->(IO (Array U DIM2 Double))
setBoundaryLeft array val = computeP $ traverse array id (\f (Z:. i:. j) -> if j == 0  then val else f(Z:. i :. j))

setBoundaryRight :: (Array U ((Z :. Int) :. Int) Double ) -> Double ->(IO (Array U DIM2 Double))
setBoundaryRight array val = computeP $ traverse array id (\f (Z:. i:. j) -> if j == dimTwo - 1  then val else f(Z:. i :. j))
    where dimTwo = dim !! 1
          dim = (listOfShape (extent array))

setBoundaryTop :: (Array U ((Z :. Int) :. Int) Double ) -> Double ->(IO (Array U DIM2 Double))
setBoundaryTop array val = computeP $ traverse array id (\f (Z:. i:. j) -> if i == 0  then val else f(Z:. i :. j))

setBoundaryBottom :: (Array U ((Z :. Int) :. Int) Double ) -> Double ->(IO (Array U DIM2 Double))
setBoundaryBottom array val = computeP $ traverse array id (\f (Z:. i:. j) -> if i == dimOne - 1  then val else f(Z:. i :. j))
    where dimOne = dim !! 0
          dim = (listOfShape (extent array))

----------------------------------------------------------------------------------------------------------------------------------------------------

createArrTwice ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrTwice  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then 1-exp(-f(Z:.i :.j))
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))


createArrAverage9 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrAverage9  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then mean [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j), f(Z:.i+1 :.j), f(Z:.i :.j-1), f(Z:.i :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))


createArrAverage5 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrAverage5  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then (f(Z:.i-1 :.j-1) + f(Z:.i+1 :.j+1)+ f(Z:.i-1 :.j+1)+ f(Z:.i+1 :.j-1)+ f(Z:.i :.j))/5
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))


createArrMinimum9 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrMinimum9  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then minimum [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j), f(Z:.i+1 :.j), f(Z:.i :.j-1), f(Z:.i :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))

createArrMinimum5 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrMinimum5  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then minimum [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))


createArrMaximum9 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrMaximum9  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then maximum [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j), f(Z:.i+1 :.j), f(Z:.i :.j-1), f(Z:.i :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))

createArrMaximum5 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrMaximum5  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then maximum [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))


createArrMedian9 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrMedian9  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then median [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j), f(Z:.i+1 :.j), f(Z:.i :.j-1), f(Z:.i :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))


createArrMedian5 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrMedian5  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then median [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))

createArrHarMean9 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrHarMean9  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then harmean [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j), f(Z:.i+1 :.j), f(Z:.i :.j-1), f(Z:.i :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))


createArrHarMean5 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrHarMean5  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then harmean [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))

createArrGeoMean9 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrGeoMean9  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then geomean [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j), f(Z:.i+1 :.j), f(Z:.i :.j-1), f(Z:.i :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))


createArrGeoMean5 ::(Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double)
createArrGeoMean5  y = traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then geomean [f(Z:.i-1 :.j-1), f(Z:.i+1 :.j+1), f(Z:.i-1 :.j+1), f(Z:.i+1 :.j-1), f(Z:.i :.j)]
                                                                  else f(Z:.i:.j))
    where row = dim!! 0
          col = dim!! 1
          dim = (listOfShape (extent y))

----------------------------------------------------------------------------------------------------------------------------------------------------

computeFuncIter::(Array D ((Z :. Int) :. Int) Double )->((Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double))->Int ->(IO (Array U DIM2 Double))
computeFuncIter y func numIter  | numIter == 0  = computeP y
                                 | otherwise = do
                                                t <- computeP (func y)::IO (Array U DIM2 Double)
                                                let u = Data.Array.Repa.map (+0) t
                                                computeFuncIter u func  (numIter - 1)
                                               

computeFuncChange::(Array D ((Z :. Int) :. Int) Double )->((Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double))->Double ->(IO (Array U DIM2 Double))
computeFuncChange y func val = do
                                temp1 <- sumAllP (func y)
                                temp2 <- (sumAllP y)
                                if temp1 - temp2  >= val then computeP (func y) 
                                else do  
                                     t <- computeP (func y)::IO (Array U DIM2 Double)
                                     let u = Data.Array.Repa.map (+0) t
                                     computeFuncChange u  func val
getClockTimeMS = do
    (TOD s p) <- getClockTime
    --print s
    --print p
    return $ fromIntegral (s * 1000)
    
computeFuncTime::(Array D ((Z :. Int) :. Int) Double )->((Array D ((Z :. Int) :. Int) Double ) ->(Array D DIM2 Double))->Integer ->(IO (Array U DIM2 Double))
computeFuncTime y func timeLimit = do
                                    input <- getClockTimeMS
                                    let t   =  (func y)
                                    input1 <- getClockTimeMS
                                    let diff = (input1 - input)
                                    if diff >= timeLimit then computeP t  else computeFuncTime (func y) func (timeLimit - diff)

----------------------------------------------------------------------------------------------------------------------------------------------------


