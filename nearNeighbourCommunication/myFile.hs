import qualified TwoD
import qualified ThreeD
import Data.Array.Repa
import Data.Array.Repa.IO.Matrix
import System.CPUTime
--let t = TwoD.createArr 1024 1024
--arr <- TwoD.setBoundaryAll t 0

main::IO() --(Array U DIM2 Double)
main = do
            print "==================================="
            print "Executing first program"
            u1 <- TwoD.getClockTimeMS
            print "Program execution started..."
            print 1024
            print 1024
            let t = TwoD.createArr 1024 1024
            print "Stting the boundaries to 0"
            arr <-  TwoD.setBoundaryAll t 0
            print "Writing data to file data.txt"
            writeMatrixToTextFile "data.txt" arr
            let arr1 = Data.Array.Repa.map (+0) arr
            t1 <- (TwoD.computeFuncIter arr1 TwoD.createArrAverage5 500)
            u2 <- TwoD.getClockTimeMS
            print "Time taken to run the program"
            print (u2-u1)
            print "Writing results to file test.txt"
            writeMatrixToTextFile "test.txt" t1
            u3 <- TwoD.getClockTimeMS
            print (u3-u2)
            print "======================================"
            
            u1 <- TwoD.getClockTimeMS
            
            print "Program execution started..."
            let t = TwoD.createArr 1024 1024
            arr <-  TwoD.setBoundaryAll t 0
            print "Writing data to file data1.txt"
            writeMatrixToTextFile "data1.txt" arr
            let arr1 = Data.Array.Repa.map (+0) arr
            t1 <- (TwoD.computeFuncIter arr1 TwoD.createArrAverage9 500)
            u2 <- TwoD.getClockTimeMS
            print "Time taken to run the program"
            print (u2-u1)
            print "Writing results to file test1.txt"
            writeMatrixToTextFile "test1.txt" t1
            u3 <- TwoD.getClockTimeMS
            print (u3-u2)