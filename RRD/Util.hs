module RRD.Util
    ( unfoldM
    , withCStringArray
    , throwErrnoOrRrdErrorIf
    , throwErrnoOrRrdErrorIf_
    , throwErrnoOrRrdErrorIfNull
    ) where
import Bindings.Librrd
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (withArray)
import Foreign.C.String (newCString, CString, peekCString)
import Foreign.C.Error (throwErrno)
import Foreign.Ptr (Ptr, nullPtr)
import Control.Exception (bracket)
import Control.Monad (when)


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

throwErrnoOrRrdErrorIf :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoOrRrdErrorIf pred loc f = do
    res <- f
    if pred res
       then do
           rrdErrcode <- c'rrd_test_error
           when (rrdErrcode /= 0) $ do
               rrdError <- c'rrd_get_error >>= peekCString
               c'rrd_clear_error
               ioError $ userError rrdError
           throwErrno loc

       else return res

throwErrnoOrRrdErrorIf_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoOrRrdErrorIf_ pred loc f = throwErrnoOrRrdErrorIf pred loc f >> return ()


throwErrnoOrRrdErrorIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoOrRrdErrorIfNull = throwErrnoOrRrdErrorIf (== nullPtr)
