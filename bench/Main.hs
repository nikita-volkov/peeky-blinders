import qualified Data.Persist as Persist
import qualified Data.Serialize as Cereal
import qualified Data.Store as Store
import Gauge.Main
import qualified PeekyBlinders as Pb
import Prelude

main =
  defaultMain
    [ bgroup "int32-le-triplet" $
        let input = Cereal.runPut (Cereal.putInt32le 1 <> Cereal.putInt32le 2 <> Cereal.putInt32le 3)
            b name peek = bench name $ nf peek input
         in [ b "peeky-blinders/statically" $ Pb.decodeByteString $ Pb.statically $ (,,) <$> Pb.leSignedInt4 <*> Pb.leSignedInt4 <*> Pb.leSignedInt4,
              b "peeky-blinders/dynamically" $ Pb.decodeByteString $ (,,) <$> Pb.statically Pb.leSignedInt4 <*> Pb.statically Pb.leSignedInt4 <*> Pb.statically Pb.leSignedInt4,
              b "store" $ either (const Nothing) Just . Store.decode @(Int32, Int32, Int32),
              b "cereal" $ Cereal.runGet ((,,) <$> Cereal.getInt32le <*> Cereal.getInt32le <*> Cereal.getInt32le),
              b "persist" $ Persist.runGet ((,,) <$> Persist.getLE @Int32 <*> Persist.getLE @Int32 <*> Persist.getLE @Int32)
            ]
    ]
