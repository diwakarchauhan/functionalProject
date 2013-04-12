-- This is haskell code
-- :set -XTypeOperators

{-# LANGUAGE TypeOperators #-} 

import Data.Array.Repa
--import Data.Array.Repa.IO.Matrix
import Language.Haskell.Extension

--create array by y =  fromListUnboxed(Z :.(4::Int):.(4::Int)) [1..16]
createArr::(Array U ((Z :. Int) :. Int) Double ) -> Int ->Int ->(IO (Array U DIM2 Double))
createArr  y row col= computeP $ traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then(sum'' (f(Z:.i :.j))  (f(Z:.i :.j+1)) (f(Z:.i :.j-1))  (f(Z:.i+1 :.j)) (f(Z:.i-1 :.j)))
                                                                  else f(Z:.i:.j))
                                                                  
sum'' x1 x2 x3 x4 x5 = (x1+x2+x3+x4+x5)/5                                                                  

--functions of 1D array
setBoundaryLeft1D :: (Array U (Z :. Int) Double) -> Double -> (IO (Array U DIM1 Double))
setBoundaryLeft1D array val = computeP $ traverse array id (\f(Z :. i) -> if i == 0 then val else f(Z:.i))


setBoundaryRight1D :: (Array U (Z :. Int) Double) -> Double -> (IO (Array U DIM1 Double))
setBoundaryRight1D array val = computeP $ traverse array id (\f(Z :. i) -> if i == dim - 1 then val else f(Z:.i))
    where dim = (listOfShape (extent array))!! 0

setBoundaryAll1D :: (Array U (Z :. Int) Double) -> Double -> (IO (Array U DIM1 Double))
setBoundaryAll1D array val= computeP $ traverse array id (\f(Z :. i) -> if i == 0 || i == dim - 1 then val else f(Z:.i))
    where dim = (listOfShape (extent array))!! 0


--Functions of 2D array
setBoundaryAll2D :: (Array U ((Z :. Int) :. Int) Double ) -> Double->(IO (Array U DIM2 Double))
setBoundaryAll2D array val = computeP $ traverse array id (\f (Z:. i:. j) -> if j > 0 || i > 0 || i < dimOne -1 || j < dimTwo -1 then f(Z:. i :. j)
                                                                                        else val)
    where dimOne = dim !! 0
          dimTwo = dim !! 1
          dim = (listOfShape (extent array))                                                                                                                                   

setBoundaryLeft2D :: (Array U ((Z :. Int) :. Int) Double ) -> Double ->(IO (Array U DIM2 Double))
setBoundaryLeft2D array val = computeP $ traverse array id (\f (Z:. i:. j) -> if j == 0  then val else f(Z:. i :. j))

setBoundaryRight2D :: (Array U ((Z :. Int) :. Int) Double ) -> Double ->(IO (Array U DIM2 Double))
setBoundaryRight2D array val = computeP $ traverse array id (\f (Z:. i:. j) -> if j == dimTwo - 1  then val else f(Z:. i :. j))
    where dimTwo = dim !! 1
          dim = (listOfShape (extent array))

setBoundaryTop2D :: (Array U ((Z :. Int) :. Int) Double ) -> Double ->(IO (Array U DIM2 Double))
setBoundaryTop2D array val = computeP $ traverse array id (\f (Z:. i:. j) -> if i == 0  then val else f(Z:. i :. j))

setBoundaryBottom2D :: (Array U ((Z :. Int) :. Int) Double ) -> Double ->(IO (Array U DIM2 Double))
setBoundaryBottom2D array val = computeP $ traverse array id (\f (Z:. i:. j) -> if i == dimOne - 1  then val else f(Z:. i :. j))
    where dimOne = dim !! 0
          dim = (listOfShape (extent array))

--Functions of 3D array

   --assume the array as cube positioned with z axis downward and x axis to right
-- left-right i = 0-dimOne
-- top-bottom  k = 0-dimThree
-- up-down j = 0-dimTwo

setBoundaryLeft3D :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryLeft3D array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if i == 0  then val else f(Z:. i :. j :. k))

setBoundaryRight3D :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryRight3D array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if i == dimOne - 1  then val else f(Z:. i :. j :. k))
    where dimOne = dim !! 0
          dim = (listOfShape (extent array))
          
setBoundaryTop3D :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryTop3D array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if k == 0  then val else f(Z:. i :. j :. k))

setBoundaryBottom3D :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryBottom3D array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if k == dimThree - 1  then val else f(Z:. i :. j :. k))
    where dimThree = dim !! 2
          dim = (listOfShape (extent array))
          

setBoundaryUp3D :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryUp3D array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if j == 0  then val else f(Z:. i :. j :. k))


setBoundaryDown3D :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryDown3D array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if j == dimTwo - 1  then val else f(Z:. i :. j :. k))
    where dimTwo = dim !! 1
          dim = (listOfShape (extent array))

setBoundaryAll3D :: (Array U (((Z :. Int) :. Int):. Int) Double ) -> Double ->(IO (Array U DIM3 Double))
setBoundaryAll3D array val = computeP $ traverse array id (\f (Z:. i:. j :. k) -> if i == 0 || i == dimOne -1 || j == 0 || j == dimTwo - 1 || k == 0 || k == dimThree - 1 then val else f(Z:. i :. j :. k))
    where dimOne = dim!! 0
          dimTwo = dim !! 1
          dimThree = dim!! 2
          dim = (listOfShape (extent array))
          