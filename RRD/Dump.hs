{-# LANGUAGE ForeignFunctionInterface #-}
module RRD.Dump
    ( dumpWithCb
    , dumpWithCb'
    , HeaderType(..)
    ) where
import Bindings.Librrd
import Data.Word (Word8)
import Foreign.C.Types (CInt, CSize)
import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr, castPtr, nullPtr)
import Foreign.Marshal.Array (peekArray)
import Control.Exception (bracket)
import RRD.Util (throwErrnoOrRrdErrorIf_)


type RrdDumpCb = Ptr () -> CSize -> Ptr () -> IO CSize
foreign import ccall "wrapper"
    liftCb :: RrdDumpCb -> IO (FunPtr RrdDumpCb)

data HeaderType
    = NoHeader
    | DtdHeader
    | XsdHeader
    deriving Show

header2cint :: HeaderType -> CInt
header2cint NoHeader = 0
header2cint DtdHeader = 1
header2cint XsdHeader = 2

dumpWithCb :: FilePath -> ([Word8] -> IO a) -> IO ()
dumpWithCb path cb = dumpWithCb' path XsdHeader cb

dumpWithCb' :: FilePath -> HeaderType -> ([Word8] -> IO a) -> IO ()
dumpWithCb' path headerType cb =
    bracket
        (liftCb $ wrapCb cb)
        (freeHaskellFunPtr) $ \c'cb ->
            withCString path $ \c'path ->
                throwErrnoOrRrdErrorIf_ (/=0) "rrd dump" $
                    c'rrd_dump_cb_r c'path (header2cint headerType) c'cb nullPtr

wrapCb :: ([Word8] -> IO a) -> RrdDumpCb
wrapCb cb bytes size _ = do
    peekArray (fromEnum size) (castPtr bytes) >>= cb
    return size
