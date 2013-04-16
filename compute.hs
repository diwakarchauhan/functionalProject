{-# LANGUAGE TypeOperators #-} 

import Data.Array.Repa
import Language.Haskell.Extension

createArr::(Array U ((Z :. Int) :. Int) Double ) -> Int ->Int ->(IO (Array U DIM2 Double))
createArr  y row col= traverse y id (\f (Z:.i :. j) -> if j  > 0 && i  > 0 && j < col -1 && i < row -1 then((f(Z:.i:.j) + f(Z:.i:.j+1))/2) else f(Z:.i:.j))
