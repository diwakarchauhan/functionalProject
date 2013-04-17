import Data.Array.Repa
import TwoD
import Data.Array.Repa.IO.Matrix
mmMult  :: Monad m
     => Array U DIM2 Double
     -> Array U DIM2 Double
     -> m (Array U DIM2 Double)
     
mmMult a b = sumP (Data.Array.Repa.zipWith (*) aRepl bRepl)
    where
      t     = transpose2D b
      aRepl = extend (Z :.All :.colsB :.All) a
      bRepl = extend (Z :.rowsA :.All :.All) t
      (Z :.colsA :.rowsA) = extent a
      (Z :.colsB :.rowsB) = extent b
      
      
      
transpose2D :: (Source r e) => Array r DIM2 e -> Array D DIM2 e
transpose2D a = backpermute (swap e) swap a
     where
       e = extent a
       swap (Z :. i :. j) = Z :. j :. i
       
main::IO()
main  = do 
        print "starting program execution"
        let row = 256
        let col = 256
        u1 <- TwoD.getClockTimeMS
        let a1 = TwoD.createArr row col
        let a2 = TwoD.createArr col row
        arr1 <-  TwoD.setBoundaryAll a1 0
        arr2 <-  TwoD.setBoundaryAll a2 0 
        t <- mmMult a1 a2
        u2 <- TwoD.getClockTimeMS
        print "computed functions"
        print (u2 - u1)
        print "written to file"
        writeMatrixToTextFile "testmult.txt" t
        u3 <- TwoD.getClockTimeMS
        print (u3-u2)
--backpermute :: (Shape sh2, Shape sh1, Source r e) =>
  --   sh2 -> (sh2 -> sh1) -> Array r sh1 e -> Array D sh2 e