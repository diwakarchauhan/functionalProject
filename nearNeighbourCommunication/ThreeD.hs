{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -XBangPatterns #-}

module ThreeD where

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

createArr :: Int -> Int -> Int -> (Array U DIM3 Double)
createArr m n o = randomishDoubleArray (Z :.(m::Int):.(n::Int):.(o::Int)) 0 255 1

----------------------------------------------------------------------------------------------------------------------------------------------------
setBoundaryLeft :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryLeft array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if i == 0  then val else f(Z:. i :. j :. k))

setBoundaryRight :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryRight array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if i == dimOne - 1  then val else f(Z:. i :. j :. k))
    where dimOne = dim !! 0
          dim = (listOfShape (extent array))
          
setBoundaryTop :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryTop array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if k == 0  then val else f(Z:. i :. j :. k))

setBoundaryBottom :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryBottom array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if k == dimThree - 1  then val else f(Z:. i :. j :. k))
    where dimThree = dim !! 2
          dim = (listOfShape (extent array))
          

setBoundaryUp :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryUp array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if j == 0  then val else f(Z:. i :. j :. k))


setBoundaryDown :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryDown array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if j == dimTwo - 1  then val else f(Z:. i :. j :. k))
    where dimTwo = dim !! 1
          dim = (listOfShape (extent array))

setBoundaryAll :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryAll array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if i == 0 || i == dimOne -1 || j == 0 || j == dimTwo - 1 || k == 0 || k == dimThree - 1 then val else f(Z:. i :. j :. k))
    where dimOne = dim!! 0
          dimTwo = dim !! 1
          dimThree = dim!! 2
          dim = (listOfShape (extent array))

------------------------------------------------------------------------------------------------------------
createArrTwice :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrTwice  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then 2*f(Z:.i :.j :. k)
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))


createArrAverageAll :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrAverageAll  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then mean [f(Z:.i-1 :.j-1 :.k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j-1 :.k), f(Z:.i-1 :.j :.k), f(Z:.i+1 :.j :.k), f(Z:.i :.j+1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :.k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i :.j-1 :.k-1), f(Z:.i-1 :.j :.k-1), f(Z:.i+1 :.j :.k-1), f(Z:.i :.j+1 :.k-1), f(Z:.i :.j :.k-1), f(Z:.i-1 :.j-1 :.k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1), f(Z:.i :.j-1 :.k+1), f(Z:.i-1 :.j :.k+1), f(Z:.i+1 :.j :.k+1), f(Z:.i :.j+1 :.k+1), f(Z:.i :.j :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))


createArrAverageCorners :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrAverageCorners  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then mean [f(Z:.i-1 :.j-1 :. k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :. k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i-1 :.j-1 :. k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))


createArrMinimumAll :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrMinimumAll  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then minimum [f(Z:.i-1 :.j-1 :.k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j-1 :.k), f(Z:.i-1 :.j :.k), f(Z:.i+1 :.j :.k), f(Z:.i :.j+1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :.k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i :.j-1 :.k-1), f(Z:.i-1 :.j :.k-1), f(Z:.i+1 :.j :.k-1), f(Z:.i :.j+1 :.k-1), f(Z:.i :.j :.k-1), f(Z:.i-1 :.j-1 :.k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1), f(Z:.i :.j-1 :.k+1), f(Z:.i-1 :.j :.k+1), f(Z:.i+1 :.j :.k+1), f(Z:.i :.j+1 :.k+1), f(Z:.i :.j :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))

createArrMinimumCorners :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrMinimumCorners  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then minimum [f(Z:.i-1 :.j-1 :. k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :. k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i-1 :.j-1 :. k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))


createArrMaximumAll :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrMaximumAll  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then maximum [f(Z:.i-1 :.j-1 :.k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j-1 :.k), f(Z:.i-1 :.j :.k), f(Z:.i+1 :.j :.k), f(Z:.i :.j+1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :.k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i :.j-1 :.k-1), f(Z:.i-1 :.j :.k-1), f(Z:.i+1 :.j :.k-1), f(Z:.i :.j+1 :.k-1), f(Z:.i :.j :.k-1), f(Z:.i-1 :.j-1 :.k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1), f(Z:.i :.j-1 :.k+1), f(Z:.i-1 :.j :.k+1), f(Z:.i+1 :.j :.k+1), f(Z:.i :.j+1 :.k+1), f(Z:.i :.j :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))

createArrMaximumCorners :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrMaximumCorners  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then maximum [f(Z:.i-1 :.j-1 :. k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :. k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i-1 :.j-1 :. k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))


createArrMedianAll :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrMedianAll  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then median [f(Z:.i-1 :.j-1 :.k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j-1 :.k), f(Z:.i-1 :.j :.k), f(Z:.i+1 :.j :.k), f(Z:.i :.j+1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :.k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i :.j-1 :.k-1), f(Z:.i-1 :.j :.k-1), f(Z:.i+1 :.j :.k-1), f(Z:.i :.j+1 :.k-1), f(Z:.i :.j :.k-1), f(Z:.i-1 :.j-1 :.k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1), f(Z:.i :.j-1 :.k+1), f(Z:.i-1 :.j :.k+1), f(Z:.i+1 :.j :.k+1), f(Z:.i :.j+1 :.k+1), f(Z:.i :.j :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
          dep = dim !! 2
          dim = (listOfShape (extent y))


createArrMedianCorners :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrMedianCorners  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then median [f(Z:.i-1 :.j-1 :. k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :. k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i-1 :.j-1 :. k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))

createArrHarMeanAll :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrHarMeanAll  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then harmean [f(Z:.i-1 :.j-1 :.k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j-1 :.k), f(Z:.i-1 :.j :.k), f(Z:.i+1 :.j :.k), f(Z:.i :.j+1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :.k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i :.j-1 :.k-1), f(Z:.i-1 :.j :.k-1), f(Z:.i+1 :.j :.k-1), f(Z:.i :.j+1 :.k-1), f(Z:.i :.j :.k-1), f(Z:.i-1 :.j-1 :.k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1), f(Z:.i :.j-1 :.k+1), f(Z:.i-1 :.j :.k+1), f(Z:.i+1 :.j :.k+1), f(Z:.i :.j+1 :.k+1), f(Z:.i :.j :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))


createArrHarMeanCorners :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrHarMeanCorners  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then harmean [f(Z:.i-1 :.j-1 :. k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :. k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i-1 :.j-1 :. k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))

createArrGeoMeanAll :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrGeoMeanAll  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then geomean [f(Z:.i-1 :.j-1 :.k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j-1 :.k), f(Z:.i-1 :.j :.k), f(Z:.i+1 :.j :.k), f(Z:.i :.j+1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :.k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i :.j-1 :.k-1), f(Z:.i-1 :.j :.k-1), f(Z:.i+1 :.j :.k-1), f(Z:.i :.j+1 :.k-1), f(Z:.i :.j :.k-1), f(Z:.i-1 :.j-1 :.k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1), f(Z:.i :.j-1 :.k+1), f(Z:.i-1 :.j :.k+1), f(Z:.i+1 :.j :.k+1), f(Z:.i :.j+1 :.k+1), f(Z:.i :.j :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))


createArrGeoMeanCorners :: (Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double)
createArrGeoMeanCorners  y = traverse y id (\f (Z:.i :. j :. k) -> if j  > 0 && i  > 0 && k > 0 && j < col -1 && i < row -1 && k < dep -1 then geomean [f(Z:.i-1 :.j-1 :. k), f(Z:.i+1 :.j+1 :.k), f(Z:.i-1 :.j+1 :.k), f(Z:.i+1 :.j-1 :.k), f(Z:.i :.j :.k), f(Z:.i-1 :.j-1 :. k-1), f(Z:.i+1 :.j+1 :.k-1), f(Z:.i-1 :.j+1 :.k-1), f(Z:.i+1 :.j-1 :.k-1), f(Z:.i-1 :.j-1 :. k+1), f(Z:.i+1 :.j+1 :.k+1), f(Z:.i-1 :.j+1 :.k+1), f(Z:.i+1 :.j-1 :.k+1)]
                                                                  else f(Z:.i:.j:.k))
    where row = dim !! 0
          col = dim !! 1
	  dep = dim !! 2
          dim = (listOfShape (extent y))

----------------------------------------------------------------------------------------------------------------------------------------------------

computeFuncIter::(Array D (((Z :. Int) :. Int) :. Int) Double )->((Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double))->Int ->(IO (Array U DIM3 Double))
computeFuncIter y func numIter  = do 
                                    t <- computeP (func y)::IO (Array U DIM3 Double)
                                    let u = Data.Array.Repa.map (+0) t
                                    computeFuncIter u func  (numIter - 1)
                                 
computeFuncChange::(Array D (((Z :. Int) :. Int):.Int) Double )->((Array D (((Z :. Int) :. Int) :. Int) Double ) ->(Array D DIM3 Double))->Double ->(IO (Array U DIM3 Double))
computeFuncChange y func val = do
                                temp1 <- sumAllP (func y)
                                temp2 <- (sumAllP y)
                                if temp1 - temp2  >= val then computeP (func y) 
                                else do  
                                     t <- computeP (func y)::IO (Array U DIM3 Double)
                                     let u = Data.Array.Repa.map (+0) t
                                     computeFuncChange u  func val
------------------------------------------------------------------------------------------------------------------------------------------------------