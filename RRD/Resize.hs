module RRD.Resize
    ( shrink
    , grow 
    )where
import Bindings.Librrd
import Foreign.C.Error (throwErrnoIf_)
import RRD.Util (withCStringArray)
import RRD.Types


grow :: FilePath -> RraId -> Int -> IO ()
grow path rraId rows = resize' path rraId "GROW" rows

shrink :: FilePath -> RraId -> Int -> IO ()
shrink path rraId rows = resize' path rraId "SHRINK" rows

resize' :: FilePath -> RraId -> String -> Int -> IO ()
resize' path rraId action rows = withCStringArray ["argv0" ,path, show rraId, action, show rows] $ \ c'argc c'argv -> do
    throwErrnoIf_  (== -1) ("rrd_resize " ++ show path) (c'rrd_resize c'argc c'argv) 
