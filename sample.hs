-- This is haskell code
-- :set -XTypeOperators

{-# LANGUAGE TypeOperators #-} 

import Data.Array.Repa
import Language.Haskell.Extension

--create array by y =  fromListUnboxed(Z :.(4::Int):.(4::Int)) [1..16]
createArr::(Array U ((Z :. Int) :. Int) Double ) -> Int ->Int ->(IO (Array U DIM2 Double))
createArr  y row col= computeP $ traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then(f(Z:.i :.j) + f(Z:.i :.j+1)+f(Z:.i :.j-1) + f(Z:.i+1 :.j))/5
                                                                  else f(Z:.i:.j))