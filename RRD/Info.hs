module RRD.Info
    ( InfoValue(..)
    , rrd_info
    ) where
import Bindings.Librrd
import Foreign.C.String (withCString, peekCString) 
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CULong)
import Foreign.Storable (peek)
import Foreign.Marshal.Error (throwIfNull)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (nullPtr, Ptr)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Word (Word8)
import RRD.Types
import RRD.Util (unfoldM)


data InfoValue
    = RRDDouble  Double
    | RRDCounter CULong
    | RRDString  String
    | RRDInt     Int
    | RRDBlob    [Word8]
    deriving Show


rrd_info :: FilePath -> IO [(String, InfoValue)]
rrd_info path = do
    info_linked_list <- throwErrnoIfNull ("rrd_info " ++ show path) (withCString path c'rrd_info_r)
    unfoldM extractNode info_linked_list
        `finally`
        c'rrd_info_free info_linked_list

    where
    extractNode :: Ptr C'rrd_info_t -> IO (Maybe ((String, InfoValue), Ptr C'rrd_info_t))
    extractNode ptr | ptr == nullPtr = return Nothing
                    | otherwise = do
                        node <- peek ptr
                        when (c'rrd_info_t'key node == nullPtr ) $
                            ioError $ userError "info key name is null"
                        keyName <- peekCString $ c'rrd_info_t'key node

                        value <- marshallInfoVal  (c'rrd_info_t'type node) (c'rrd_info_t'value node)

                        return $ Just ((keyName, value), c'rrd_info_t'next node)

    marshallInfoVal :: C'rrd_info_type_t -> C'rrd_infoval -> IO InfoValue
    marshallInfoVal tag val | tag == c'RD_I_VAL = return $ RRDDouble $ realToFrac $ c'rrd_infoval'u_val val
                            | tag == c'RD_I_CNT = return $ RRDCounter $ c'rrd_infoval'u_cnt val
                            | tag == c'RD_I_STR = fmap RRDString $ peekCString $ c'rrd_infoval'u_str val
                            | tag == c'RD_I_INT = return $ RRDInt $ fromIntegral $ c'rrd_infoval'u_int val
                            | tag == c'RD_I_BLO = fmap (RRDBlob . map fromIntegral) $ peekArray -- ??? should it fail on zero-sized blob?
                                 (fromIntegral $ c'rrd_blob_t'size $ c'rrd_infoval'u_blo val) (c'rrd_blob_t'ptr $ c'rrd_infoval'u_blo val)

                            | otherwise         = ioError $ userError $ "unknown info node type: " ++ show tag
