module Main where

import qualified Data.ByteString as B
import           Data.List ( permutations )
import qualified Data.Map.Strict as Map
import           Data.Word
import           Shamir
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

genContext :: Gen Context
genContext = do
    s <- listOf1 arbitrary
    n <- choose (3, 255)
    k <- choose (2, n)
    i <- choose (0, n - 1)
    return $
        Context
            (B.pack s)
            (fromIntegral n)
            (fromIntegral k)
            (take k $ permutations [1 .. (fromIntegral n)] !! i)

data Context = Context
    { secret :: B.ByteString
    , shares :: Word8
    , threshold :: Word8
    , recovered :: [Word8]
    } deriving (Show)

-- | The results of a split should always be combinable.
prop_bijective :: Property
prop_bijective =
    forAll genContext $ \c ->
    monadicIO $ do
        s <- run $ Shamir.split (shares c) (threshold c) (secret c)
        assert $ secret c == (combine . subset c) s
        where
          subset c = Map.filterWithKey (\v _ -> v `elem` recovered c)

main :: IO ()
main = quickCheck prop_bijective
