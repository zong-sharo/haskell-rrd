{-# LANGUAGE GADTs #-}
module RRD.RPN.Prim
    ( Reference(..)
    , TimeFormat(..)
    , RpnExpr(..)
    , UnaryOperator(..)
    , BinaryOperator(..)
    , Symbol(..)
    , Aggregate(..)
    , AggregateFunction(..)
    , RpnValue(..)
    , PrimExprEq
    , PrimExprOrd
    , refName
    ) where
import RRD.Util (showF)


data Reference
    = DataSource String FilePath String
    | Expr String (RpnExpr Double)
    | AggregateExpr String Aggregate

refName :: Reference -> String
refName (DataSource a _ _) = a
refName (Expr a _) = a
refName (AggregateExpr a _) = a

data TimeFormat = UTC | LocalTime

-- missing stuff:
-- trend(nan)
-- predict(sigma)
-- potentially usefull stuff to add:
-- none for now
-- rejected stuff (can be implemented one level above)
-- head

data RpnExpr a where
    Pure    :: RpnValue a => a -> RpnExpr a
    Ref     :: Reference -> RpnExpr Double
    List    :: [RpnExpr a] -> RpnExpr [a]
    UOp     :: UnaryOperator a b -> RpnExpr a -> RpnExpr b
    BOp     :: BinaryOperator a b c -> RpnExpr a -> RpnExpr b -> RpnExpr c
    If      :: RpnExpr Bool -> RpnExpr a -> RpnExpr a -> RpnExpr a
    Limit   :: RpnExpr a -> RpnExpr a -> RpnExpr a -> RpnExpr a
    Sym     :: Symbol -> RpnExpr Double
    Foldr   :: BinaryOperator a b b -> RpnExpr b -> RpnExpr [a] -> RpnExpr b

class RpnValue a where
    renderValue :: a -> String

instance RpnValue Double where
    renderValue = showF

instance RpnValue Bool where
    renderValue False = "0"
    renderValue True = "1"

class PrimExprEq a
instance PrimExprEq Double
instance PrimExprEq Bool
instance PrimExprEq a => PrimExprEq [a]

class PrimExprEq a => PrimExprOrd a
instance PrimExprOrd Double
instance PrimExprOrd Bool
instance PrimExprOrd a => PrimExprOrd [a]

data UnaryOperator a b where
    IsUnknown   :: UnaryOperator Double Bool
    IsInfinity  :: UnaryOperator Double Bool
    Bool2Double :: UnaryOperator Bool Double

    Sin         :: UnaryOperator Double Double
    Cos         :: UnaryOperator Double Double
    Log         :: UnaryOperator Double Double
    Exp         :: UnaryOperator Double Double
    Sqrt        :: UnaryOperator Double Double
    Atan        :: UnaryOperator Double Double

    Deg2Rad     :: UnaryOperator Double Double
    Rad2Deg     :: UnaryOperator Double Double

    Floor       :: UnaryOperator Double Double
    Ceil        :: UnaryOperator Double Double
    Abs         :: UnaryOperator Double Double

    Sort        :: PrimExprOrd a => UnaryOperator [a] [a] -- kinda overkill?
    Reverse     :: UnaryOperator [a] [a]
    Average     :: UnaryOperator [Double] Double -- nobody will miss avg on bools, or what?

    Tail        :: UnaryOperator [a] [a]
--    Head        :: UnaryOperator [a] a

data BinaryOperator a b c where
    Less           :: PrimExprOrd a => BinaryOperator a a Bool
    Greater        :: PrimExprOrd a => BinaryOperator a a Bool
    LessOrEqual    :: PrimExprOrd a => BinaryOperator a a Bool
    GreaterOrEqual :: PrimExprOrd a => BinaryOperator a a Bool
    Equal          :: PrimExprEq a => BinaryOperator a a Bool
    NotEqual       :: PrimExprEq a => BinaryOperator a a Bool

    Min            :: PrimExprOrd a => BinaryOperator a a a
    Max            :: PrimExprOrd a => BinaryOperator a a a

    Addition       :: BinaryOperator Double Double Double
    Subtraction    :: BinaryOperator Double Double Double
    Multiplication :: BinaryOperator Double Double Double
    Division       :: BinaryOperator Double Double Double
    Modulo         :: BinaryOperator Double Double Double

    AddNan         :: BinaryOperator Double Double Double

    Atan2          :: BinaryOperator Double Double Double

    Cons           :: BinaryOperator [a] a [a]

    Flip           :: BinaryOperator a b c -> BinaryOperator b a c

data Symbol
    = Unknown
    | Infinity
    | NegativeIntinity
    | Previous
    | PreviousOf Reference
    | Counter
    | Now
    | RecordTime TimeFormat

data Aggregate = Aggregate Reference AggregateFunction

data AggregateFunction
    = Maximum
    | Minimum
    | AverageValue -- ??? lame?
    | StandardDeviation
    | Last
    | First
    | Total
    | Percent Double
    | PercentNan Double
    | LeastSquareLineSlope
    | LeastSquareLineInt
    | LeastSquareLineCorrelationCoefficient
