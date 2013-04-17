import qualified TwoD
import qualified ThreeD
import Data.Array.Repa
import Data.Array.Repa.IO.Matrix

main::IO() --(Array U DIM2 Double)
main = do
            u1 <- TwoD.getClockTimeMS
            print "Starting the program execution... "
            let t = ThreeD.createArr 500 500 20
            print "Setting the array boundaries to 0"
            arr <-  ThreeD.setBoundaryAll t 0
            let arr1 = Data.Array.Repa.map (+0) arr
            t1 <- (ThreeD.computeFuncIter arr1 ThreeD.createArrAverageAll 1000)
            u2 <- TwoD.getClockTimeMS
            print "program ended, execution time is .."
            print (u2-u1)
            --print "writing output to file test3d.txt"
            --writeMatrixToTextFile "test3d.txt" t1
            u3 <- TwoD.getClockTimeMS
            print (u3-u2)