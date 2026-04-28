import qualified TestHS as T
import Test.Meta.CommonEffect as CommonEffect
import Test.Meta.SpringREMLTest as SpringREMLTest
import Test.MixedModel.DyestuffTest as DyestuffTest
import Test.MixedModel.SleepstudyTest as SleepstudyTest
import Test.MixedModel.IPDSimpleTest as IPDSimpleTest
import Test.MixedModel.SleepstudyRETest as SleepstudyRETest
import Test.MixedModel.PlaqueGLMMTest as PlaqueGLMMTest
import Test.MixedModel.SpringNetTest as SpringNetTest
import Test.MixedModel.SpringRelaxTest as SpringRelaxTest

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn "Test Begins"
  T.reportTestsIO $ CommonEffect.ioTests
  T.reportTestsIO $ SpringREMLTest.ioTests
  T.reportTestsIO $ DyestuffTest.ioTests
  T.reportTestsIO $ SleepstudyTest.ioTests
  T.reportTestsIO $ IPDSimpleTest.ioTests
  T.reportTestsIO $ SleepstudyRETest.ioTests
  T.reportTestsIO $ PlaqueGLMMTest.ioTests
  T.reportTestsIO $ SpringNetTest.ioTests
  T.reportTestsIO $ SpringRelaxTest.ioTests
