module RRD.Misc
    ( lastUpdate
    , first
    ) where
import Bindings.Librrd
import Foreign.C.String (withCString)
import Control.Exception (bracket)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal (malloc, free)
import Foreign.Storable (peek)
import Data.Maybe (fromMaybe)
import RRD.Types
import RRD.Util (throwErrnoOrRrdErrorIf, throwErrnoOrRrdErrorIf_)


-- TODO make sure it's leak free, it's probably so, but it's better to make some actuall tests
lastUpdate :: FilePath -> IO Int
lastUpdate path =
    withCString path $ \c'path ->
        bracket malloc free $ \lastUpdate_ret ->
            bracket malloc free $ \dsCount_ret ->
                bracket malloc free $ \dsNames_ret ->
                    bracket malloc free $ \lastDs_ret -> do
                        throwErrnoOrRrdErrorIf_ (/=0) ("rrd " ++ show path)
                            (c'rrd_lastupdate_r c'path lastUpdate_ret dsCount_ret dsNames_ret lastDs_ret)
                        fromEnum `fmap` peek lastUpdate_ret

first :: FilePath -> Maybe RraId -> IO Int
first path rraIndex = withCString path $ \c'path ->
    throwErrnoOrRrdErrorIf (== -1) ("rrd_first " ++ show path) $
        fromEnum `fmap` c'rrd_first_r c'path (toEnum $ fromMaybe 0 rraIndex)
