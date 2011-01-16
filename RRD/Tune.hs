module RRD.Tune
    ( Tune(..)
    , DataSourceTune(..)
    , renderTune
    , tune
    ) where
import Bindings.Librrd
import RRD.Types
import RRD.Util (withCStringArray, throwErrnoOrRrdErrorIf_, showF)
import Data.List


data Tune
    = DataSourceTune String [DataSourceTune]
    | PositiveDelta Double
    | NegativeDelta Double
    | FailureThreshold Int
    | FailuresWindowLength Int
    | Alpha Double
    | Beta Double
    | Gamma Double
    | GammaDeviation Double
    | SmoothingWindow Double
    | SmoothingWindowDeviation Double
    deriving Show

data DataSourceTune
    = AlterHeartBeat HeartBeat
    | AlterMinimum Minimum
    | AlterMaximum Maximum
    | RenameTo DataSourceName
    | ResetAbberant
    deriving Show
-- XXX --data-source-type wtf?

renderTune :: Tune -> [String]
renderTune (DataSourceTune name ops) = concatMap (renderDSTune name) ops
renderTune (PositiveDelta delta') = ["--deltapos", showF delta']
renderTune (NegativeDelta delta') = ["--deltaneg", showF delta']
renderTune (FailureThreshold threshold') = ["--failure-threshold", show threshold']
renderTune (FailuresWindowLength len') = ["--window-length", show len']
renderTune (Alpha a') = ["--alpha", showF a']
renderTune (Beta b') = ["--beta", showF b']
renderTune (Gamma g') = ["--gamma", showF g']
renderTune (GammaDeviation gd') = ["--gamma-deviation", showF gd']
renderTune (SmoothingWindow window') = ["--smoothing-window", showF window']
renderTune (SmoothingWindowDeviation dev') = ["--smoothing-window-deviation", showF dev']

renderDSTune :: DataSourceName -> DataSourceTune -> [String]
renderDSTune name (AlterHeartBeat heartbeat') = ["--heartbeat", name ++ ":" ++ show heartbeat']
renderDSTune name (AlterMinimum min') = ["--minimum", name ++ ":" ++ showF min']
renderDSTune name (AlterMaximum max') = ["--maximum", name ++ ":" ++ showF max']
renderDSTune name (RenameTo name') = ["--data-source-rename", name ++ ":" ++ name']
renderDSTune name ResetAbberant = ["--aberrant-reset", name]

tune :: FilePath -> [Tune] -> IO ()
tune path tunes | null tunes = return ()
                | otherwise = withCStringArray ("argv0" : path : concatMap renderTune tunes) $ \len arr -> do
                    -- yup, "argv0" it's that simple. rrd_tune both used for console app and api (rrdtool-1.4.4)
                    throwErrnoOrRrdErrorIf_ (/=0) ("rrd_tune " ++ show path) $ c'rrd_tune len arr
