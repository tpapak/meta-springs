import qualified TestHS as T
import Test.Meta.Springs as SPR

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTestsIO $ SPR.ioTests
