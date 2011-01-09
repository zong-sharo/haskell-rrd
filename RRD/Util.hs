module RRD.Util
    ( unfoldM
    , withCStringArray
    ) where
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (withArray)
import Foreign.C.String (newCString, CString)
import Foreign.Ptr (Ptr)
import Control.Exception (bracket)


unfoldM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM op seed = unfoldM' [] op seed >>= (return . reverse)
    where
    unfoldM' :: Monad m => [a] -> (b -> m (Maybe (a, b))) -> b -> m [a]
    unfoldM' acc op seed = do
        a <- op seed
        maybe (return acc) (\(value, seed') -> unfoldM' (value : acc) op seed') a


withCStringArray :: Num len => [String] -> (len -> Ptr CString -> IO a) -> IO a
withCStringArray xs op =
    bracket
        (mapM newCString xs) -- XXX this will leak if memory exhausted in the middle of mapM
        (mapM_ free)
        (\c'xs -> withArray c'xs (\arr -> op (fromIntegral $ length xs) arr))
