module Main where
import RRD.Types



type StartingTime = Integer
type StepInterval = Int


data DataSource = DataSource DataSourceType DataSourceName HeartBeat Minimum Maximum
    deriving Show

data DataSourceType
    = Gauge
    | Counter
    | Derive
    | Abosolute
    deriving Show

type XfilesFactor = Int
type Steps = Int
type GenerationsCount = Int

data Archive = Archive ConsolidationFunction XfilesFactor Steps GenerationsCount
    deriving Show

data ConsolidationFunction
    = Average
    | Min
    | Max
    | Last
    deriving Show

createDatabase :: FilePath -> StartingTime -> StepInterval -> [DataSource] -> [Archive] -> IO ()
-- ^ creates a rrd database, throws an exception in case of failure
createDatabase = undefined
