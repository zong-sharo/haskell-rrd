{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

import Control.Applicative

import Text.Printf

----------------------------------------------------------------------------------
-- RPN calculator

-- | To perform operations usually performed in RPN notation you can either input raw rpn value or building it
-- using high level functions.
--
-- You can use following operations
--
--


data CDEF = CDEF String (Rpn CDEF Double) deriving (Show)
data VDEF = VDEF String (Rpn VDEF Double) deriving (Show)

data Reference a 
    = DataSource String FilePath String       -- variable name, filename / fieldname
    | Expression String (Rpn a Double)        -- variable name, expression

data Rpn a b where 
        Constant :: Double      -> Rpn a Double
        Bool     :: Bool        -> Rpn a Bool
        Ref      :: Reference a -> Rpn a Double
        Array    :: [Rpn a b]   -> Rpn a [b]

------------------------------------------------------------------------------------------
       -- Boolean operations {{{
        Lt       :: Rpn a Double -> Rpn a Double -> Rpn a Bool 
        Gt       :: Rpn a Double -> Rpn a Double -> Rpn a Bool 
        Le       :: Rpn a Double -> Rpn a Double -> Rpn a Bool 
        Ge       :: Rpn a Double -> Rpn a Double -> Rpn a Bool 
        Eq       :: Rpn a Double -> Rpn a Double -> Rpn a Bool 
        Ne       :: Rpn a Double -> Rpn a Double -> Rpn a Bool 

        -- | 
        IsUnk    :: Rpn a Double -> Rpn a Bool                      -- is unknown
        IsInf    :: Rpn a Double -> Rpn a Bool                      -- is infinity
        If       :: Rpn a Bool  -> Rpn a Double -> Rpn a Double -> Rpn a Double
        -- }}}

        -- Comparing {{{
        Min      :: Rpn a Double -> Rpn a Double -> Rpn a Double
        Max      :: Rpn a Double -> Rpn a Double -> Rpn a Double
        Limit    :: Rpn a Double -> Rpn a Double -> Rpn a Double -> Rpn a Double     -- if a <= c <= a then c else unknown
        -- }}}

        -- Arithmetics {{{
        Add      :: Rpn a Double -> Rpn a Double -> Rpn a Double
        Sub      :: Rpn a Double -> Rpn a Double -> Rpn a Double
        Mul      :: Rpn a Double -> Rpn a Double -> Rpn a Double
        Div      :: Rpn a Double -> Rpn a Double -> Rpn a Double
        Mod      :: Rpn a Double -> Rpn a Double -> Rpn a Double

        AddNan   :: Rpn a Double -> Rpn a Double -> Rpn a Double              -- if inf/unknown safe addition

        Sin      :: Rpn a Double -> Rpn a Double
        Cos      :: Rpn a Double -> Rpn a Double
        Log      :: Rpn a Double -> Rpn a Double
        Exp      :: Rpn a Double -> Rpn a Double
        Sqrt     :: Rpn a Double -> Rpn a Double
        Atan     :: Rpn a Double -> Rpn a Double
        Atan2    :: Rpn a Double -> Rpn a Double -> Rpn a Double

        Floor    :: Rpn a Double -> Rpn a Double
        Ceil     :: Rpn a Double -> Rpn a Double

        Deg2rad  :: Rpn a Double -> Rpn a Double
        Rad2deg  :: Rpn a Double -> Rpn a Double

        Abs      :: Rpn a Double -> Rpn a Double

        Avg      :: Rpn a [Double] -> Rpn a Double
        Sum      :: Rpn a [Double] -> Rpn a Double
        Sort     :: Rpn a [a] -> Rpn a [a]
        Rev      :: Rpn a [b] -> Rpn a [b]
        AHead    :: Rpn a [Double] -> Rpn a Double
        ATail    :: Rpn a [b] -> Rpn a [b]
        ALast    :: Rpn a [Double] -> Rpn a Double
        And      :: Rpn a [Bool] -> Rpn a Bool
        Any      :: Rpn a [Bool] -> Rpn a Bool
        -- }}}

        -- Set operations {{{
--        Sort     :: FIXME
--        Rev      :: FIXME
        Trend    :: Rpn CDEF Double -> Rpn CDEF Double        
        TrendNan :: Rpn CDEF Double -> Rpn CDEF Double
--        Predict :: FIXME
--        PredictSigma :: FIXME
-- }}}

        -- Stack manipulations {{{
--        Dup      :: Rpn a Double
--        Pop      :: Rpn a Double
--        Exchange :: Rpn a Double
        -- }}}

        -- Special values {{{
        Inf      :: Rpn a Double
        Unknown  :: Rpn a Double
        MinusInf :: Rpn a Double

        Prev     :: Rpn CDEF Double
--        Prev (vname) ???
        Count    :: Rpn CDEF Double

        -- Time
        Now      :: Rpn a Double
        Time     :: Rpn a Double
        LTime    :: Rpn a Double
        -- }}}

        -- Constantuables {{{
        Maximum  :: Rpn VDEF Double
        Minimum  :: Rpn VDEF Double
        Average  :: Rpn VDEF Double
        StdDev   :: Rpn VDEF Double
        Last     :: Rpn VDEF Double
        First    :: Rpn VDEF Double
        Total    :: Rpn VDEF Double
        Percent  :: Rpn VDEF Double
        PercentNan :: Rpn VDEF Double
        Lslslope :: Rpn VDEF Double
        Lslint   :: Rpn VDEF Double
        Lslcorrel :: Rpn VDEF Double
        -- }}}

------------------------------------------------------------------------------------------


{-
TODO:


-}

sin_     = Sin
cos_     = Cos
log_     = Log
exp_     = Exp
pi_      = Constant pi
sqrt_    = Sqrt
asinh_ x = log (x + sqrt (x*x + 1))
acosh_ x = log (x + sqrt (x*x - 1))
sinh_  x = 0.5 * (exp x - exp (-x))
cosh_  x = 0.5 * (exp x - exp (-x)) 
atanh_ x = 0.5 * log ((1 + x) / (1 - x))


infixl 7 .*., ./.
infixl 6 .+., .-.
infixl 5 .%.
infixl 3 .>., .>=., .<., .<=., ./=., .==.


(.+.) = Add
(.-.) = Sub
(.*.) = Mul
(./.) = Div
(.%.) = mod
abs_ = Abs
signum_ x = If (x .>=. 0) (1) (-1) --error "Dunno how to do it"

instance Show (Rpn a Double) where
    show = show . renderRpnD

instance Show (Rpn a Bool) where
    show = show . renderRpnB


(.>.) :: Rpn a Double -> Rpn a Double -> Rpn a Bool -- etc
(.>.) = Gt
(.<.) = Lt
(.>=.) = Ge
(.<=.) = Le
(./=.) = Ne
(.==.) = Eq


mkRpnList :: [Rpn a b] -> Rpn a [b]
mkRpnList = Array

renderArray :: Rpn a [Double] -> [String]
renderArray (Array l)  = concatMap renderRpnD l
renderArray (Sort  a)  = renderArray a ++ [show . lengthOfArray $ a] ++ ["SORT"]
renderArray (Rev   a)  = renderArray a ++ [show . lengthOfArray $ a] ++ ["REV"]
renderArray (ATail a)  = renderArray (Rev a) ++ ["POP"]

renderArrayB :: Rpn a [Bool] -> [String]
renderArrayB (Array l)  = concatMap renderRpnB l
renderArrayB (Sort  a)  = renderArrayB a ++ [show . lengthOfArray $ a] ++ ["SORT"]
renderArrayB (Rev   a)  = renderArrayB a ++ [show . lengthOfArray $ a] ++ ["REV"]
renderArrayB (ATail a)  = renderArrayB (Rev a) ++ ["POP"]



lengthOfArray :: Rpn a [b] -> Int
lengthOfArray (Array a) = length a
lengthOfArray (Sort a)  = lengthOfArray a
lengthOfArray (Rev a)   = lengthOfArray a
lengthOfArray (ATail a) = lengthOfArray a - 1

renderRpnB :: Rpn a Bool -> [String]
renderRpnB (Bool True)  = ["1"]
renderRpnB (Bool False) = ["0"]
renderRpnB (And a)  = renderArrayB a ++ replicate (lengthOfArray a - 1) "*" -- 1 * 1 * 1 = 1; 1 * 1 * 1 * 0 = 0
renderRpnB (Any a)  = renderArrayB a ++ replicate (lengthOfArray a - 1) "+" -- any value other than 0 is considered to be true
renderRpnB (Lt a b) = r2 "LT" a b
renderRpnB (Gt a b) = r2 "GT" a b
renderRpnB (Le a b) = r2 "LE" a b
renderRpnB (Ge a b) = r2 "GE" a b
renderRpnB (Eq a b) = r2 "EQ" a b
renderRpnB (Ne a b) = r2 "NE" a b
renderRpnB (IsInf a) = r1 "ISINF" a
renderRpnB (IsUnk a) = r1 "UN" a



renderRpnD :: Rpn a Double -> [String]
-- constants and variables
renderRpnD (Constant v) = [showF v]
renderRpnD (Ref (DataSource s _ _)) = [s]
renderRpnD (Ref (Expression s _)) = [s]

-- operations over arrays
renderRpnD (Avg a)   = renderArray a ++ [show . lengthOfArray $ a] ++ ["AVG"]
renderRpnD (Sum a)   = renderArray a ++ replicate (lengthOfArray a - 1) "+"
renderRpnD (AHead a) = renderArray a ++ replicate (lengthOfArray a) "POP"
renderRpnD (ALast a) = renderRpnD (AHead (Rev a))

-- arithmetics
renderRpnD (Add a b) = r2 "+" a b
renderRpnD (Mul a b) = r2 "*" a b
renderRpnD (Sub a b) = r2 "-" a b
renderRpnD (Div a b) = r2 "/" a b
renderRpnD (Mod a b) = r2 "%" a b
renderRpnD (AddNan a b) = r2 "ADDNAN" a b
renderRpnD (Min a b) = r2 "MIN" a b
renderRpnD (Max a b) = r2 "MAX" a b
renderRpnD (Limit a b c) = r3 "LIMIT" a b c
renderRpnD (If a b c) = renderRpnB a ++ r2 "IF" b c

-- trigonometry
renderRpnD (Sin a) = r1 "SIN" a
renderRpnD (Cos a) = r1 "COS" a
renderRpnD (Log a) = r1 "LOG" a
renderRpnD (Exp a) = r1 "EXP" a
renderRpnD (Abs a) = r1 "ABS" a
renderRpnD (Sqrt a) = r1 "SQRT" a
renderRpnD (Atan a) = r1 "ATAN" a
renderRpnD (Atan2 a b) = r2 "ATAN2" a b
renderRpnD (Floor a) = r1 "FLOOR" a
renderRpnD (Ceil a) = r1 "CEIL" a
renderRpnD (Rad2deg a) = r1 "RAD2DEG" a
renderRpnD (Deg2rad a) = r1 "DEG2RAD" a
renderRpnD (Inf) = ["INF"]
renderRpnD (MinusInf) = ["NEGINF"]
renderRpnD (Unknown) = ["UNKN"]

-- special values
renderRpnD (Prev) = r0 "PREV"
renderRpnD (Count) = r0 "COUNT"
renderRpnD (Now) = r0 "NOW"
renderRpnD (Time) = r0 "TIME"
renderRpnD (LTime) = r0 "LTIME"
renderRpnD (Maximum) = r0 "MAXIMUM"
renderRpnD (Minimum) = r0 "MINIMUM"
renderRpnD (Average) = r0 "AVERAGE"
renderRpnD (StdDev) = r0 "STDEV"
renderRpnD (Last) = r0 "LAST"
renderRpnD (First) = r0 "FIRST"
renderRpnD (Total) = r0 "TOTAL"
--renderRpnD (Percent) = r0 "PERCENT"

-- analysis
renderRpnD (Trend a) = renderRpnD a ++ ["TREND"]
renderRpnD (TrendNan a) = renderRpnD a ++ ["TRENDNAN"]
renderRpnD (Lslslope) = r0 "LSLSLOPE"
renderRpnD (Lslint) = r0 "LSLINT"
renderRpnD (Lslcorrel) = r0 "LSLCORREL"





-- {{{ helpers

r0 s       = [s]
r1 s a     = renderRpnD a ++ [s]
r2 s a b   = renderRpnD a ++ renderRpnD b ++ [s]
r3 s a b c = renderRpnD a ++ renderRpnD b ++ renderRpnD c ++ [s]

showF a = printf "%f" a


-- }}}}
