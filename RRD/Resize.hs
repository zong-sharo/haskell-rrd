module RRD.Resize
    ( shrink
    , grow 
    )where
import Bindings.Librrd
import System.Directory (getCurrentDirectory, setCurrentDirectory, canonicalizePath, renameFile, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Error
import System.FilePath ((</>))
import Control.Exception (bracket)
import Control.Monad (when)
import RRD.Util (withCStringArray, throwErrnoOrRrdErrorIf_)
import RRD.Types

{-
c'rrd_resize is a just plain binding to rrdtool resize, thus it creates a
resized rrd database in the current directory under name resized.rr.

It's also worth to notice that if file under name "resize.rrd" already exists
in the currety directory rrd_resize will fail.

This behaviour is work-arounded by creating and switching to a
temporary directory, calling rrd_resize, moving resized copy elsewhere
and changing current directory back to the original.
-}

-- TODO: silently altered PWD may lead to unforeseen consequences that are hard to debug.
-- this feature should be documented somehow.
-- providing PWD-messing free interface is a good idea too.
grow :: FilePath -> FilePath -> RraId -> Int -> IO ()
grow source dest rraId rows = resize' source dest rraId "GROW" rows

shrink :: FilePath -> FilePath -> RraId -> Int -> IO ()
shrink source dest rraId rows = resize' source dest rraId "SHRINK" rows

resize' :: FilePath -> FilePath -> RraId -> String -> Int -> IO ()
resize' source dest rrdId action rows | source == dest =
    ioError $ flip ioeSetErrorString "source and dest files must be different"
        $ mkIOError illegalOperationErrorType "rrd resize" Nothing Nothing
-- ??? does it worth to canonicalize paths before equality check?
resize' source dest rrdId action rows =
    bracket
        getCurrentDirectory
        setCurrentDirectory
        ( \old_pwd  -> do
            destAbsPath <- canonicalizePath (old_pwd </> dest)
            destAlredyExists <- doesFileExist destAbsPath
            when destAlredyExists $
                ioError $ flip ioeSetErrorString "destination file already exists"
                    $ mkIOError alreadyExistsErrorType "rrd resize" Nothing (Just destAbsPath)

            withSystemTempDirectory "rrd-resize-XXXXXX" $ \tmpdir -> do
                setCurrentDirectory tmpdir
                rrd_resize source rrdId action rows
                renameFile (tmpdir </> "resize.rrd") destAbsPath
        )

rrd_resize :: FilePath -> RraId -> String -> Int -> IO ()
rrd_resize path rraId action rows =
    withCStringArray ["argv0" ,path, show rraId, action, show rows] $ \ c'argc c'argv -> do
        throwErrnoOrRrdErrorIf_  (== -1) ("rrd_resize " ++ show path) (c'rrd_resize c'argc c'argv) 
