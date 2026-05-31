import qualified TestHS as T

-- meta-analysis NMA / REML core
import Test.Meta.Studies as Studies
import Test.Meta.Effects as Effects
import Test.Meta.Multiarm as Multiarm
import Test.Numerics as Numerics
import Test.Meta.CommonEffect as CommonEffect
import Test.Meta.NMA as NMA
import Test.Meta.BinomialSpringTest as BinomialSpringTest
import Test.Meta.REMLValidation as REMLValidation
import Test.Meta.SpringREMLTest as SpringREMLTest
import Test.Meta.GroupingTest as GroupingTest

-- slmm mixed-model / spring-relaxation validation
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
  -- NMA / REML core
  T.reportTestsIO Studies.ioTests
  T.reportTestsIO Effects.ioTests
  T.reportTestsIO Multiarm.ioTests
  T.reportTestsIO Numerics.ioTests
  T.reportTestsIO CommonEffect.ioTests
  T.reportTestsIO NMA.ioTests
  T.reportTestsIO BinomialSpringTest.ioTests
  T.reportTestsIO REMLValidation.ioTests
  T.reportTestsIO SpringREMLTest.ioTests
  T.reportTestsIO GroupingTest.ioTests
  -- mixed-model / spring-relaxation validation
  T.reportTestsIO DyestuffTest.ioTests
  T.reportTestsIO SleepstudyTest.ioTests
  T.reportTestsIO IPDSimpleTest.ioTests
  T.reportTestsIO SleepstudyRETest.ioTests
  T.reportTestsIO PlaqueGLMMTest.ioTests
  T.reportTestsIO SpringNetTest.ioTests
  T.reportTestsIO SpringRelaxTest.ioTests
