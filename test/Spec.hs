import qualified MT
import           Data.List                      ( unfoldr )
import           Data.Maybe                     ( mapMaybe )
import           Data.Word                      ( Word64 )
import           System.Random                  ( RandomGen(genWord64) )
import           Text.Read                      ( readMaybe )
import           Test.Hspec

main :: IO ()
main = do
  contents <- readFile "cref/5489.txt"
  let xs  = mapMaybe readMaybe . words $ contents :: [Word64]
      mts = word64s $ MT.fromSeed 5489
  hspec $ do
    describe "MT.hs" $ do
      it "correctly matches up with the results located in \"5489.txt\"" $ do
        and (zipWith (==) xs mts) `shouldBe` True

word64s :: RandomGen g => g -> [Word64]
word64s = unfoldr (Just . genWord64)
