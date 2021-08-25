import Test.Hspec (describe, hspec, it)
import Test.QuickCheck (Testable (property))
import Year2015.Day1 (moveFloor)

prop_moveFloorInverse :: Int -> Bool
prop_moveFloorInverse floor = (moveFloor (moveFloor floor ')') '(' == floor) && (moveFloor (moveFloor floor '(') ')' == floor)

main :: IO ()
main = hspec $ do
  describe "Day 1 tests" $ do it "returns the original floor if ')' and '(' are called in succession or vice versa" $ property prop_moveFloorInverse
