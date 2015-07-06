import           Criterion.Main
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import           Shamir

main :: IO ()
main =
    defaultMain
        [ bench "split" $
          nfIO (split 5 3 secret)
        , bench "combine" $
          nf combine shares]
  where
    secret =
        C.pack "hello world"
    shares =
        Map.fromList
            [ (1, B.pack [64, 163, 216, 189, 193])
            , (3, B.pack [194, 250, 117, 212, 82])
            , (5, B.pack [95, 17, 153, 111, 252])]
