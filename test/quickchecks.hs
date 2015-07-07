module Main where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import           Data.Word
import           Shamir
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

instance Arbitrary B.ByteString where
    arbitrary =
        fmap B.pack arbitrary

prop_canCombineShares :: B.ByteString -> Positive Word8 -> Positive Word8 -> Property
prop_canCombineShares s pN pK =
    (n > 1) ==>  -- have to split into more than one share
    (k > 1) ==>  -- have to require more than one share to combine
    (n >= k) ==> -- have to require no more shares to combine than we create
    monadicIO $ do
        shares <- run $ Shamir.split n k s
        -- TODO: specify which shares via quickcheck inputs
        assert $ s == (combine . Map.filterWithKey (\v _ -> v <= k)) shares
  where
    n = getPositive pN
    k = getPositive pK

main :: IO ()
main = quickCheck prop_canCombineShares
