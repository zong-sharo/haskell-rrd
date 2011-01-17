{-# LANGUAGE GADTs, PackageImports #-}
module RRD.RPN.Render where
import RRD.RPN.Prim
import "monads-tf" Control.Monad.Writer (Writer, runWriter, tell)
import System.FilePath (takeBaseName)


type Render = Writer [String]

runRender :: Render a -> [String]
runRender = snd . runWriter

push = tell . (:[])

length' :: RpnExpr [a] -> Int
length' (BOp Cons xs x) = length' xs + 1
length' (List xs) = length xs
-- no other constructor can have [a] type

renderRpn :: RpnExpr a -> Render ()
renderRpn (Pure a) = push $ renderValue a
renderRpn (Ref ref) = push (refName ref)
renderRpn (List xs) = mapM_ renderRpn $ reverse xs
renderRpn (UOp op a) = renderRpn a >> mapM_ push (reprUOp op)
renderRpn (BOp op a b) = renderRpn a >> renderRpn b >> mapM_ push (reprBOp op)
renderRpn (If pred a b) = renderRpn a >> renderRpn b >> renderRpn pred >> push "IF"
renderRpn (Limit a lower upper) = renderRpn a >> renderRpn lower >> renderRpn upper
renderRpn (Sym s) = push (reprSymbol s)
renderRpn (Foldr op seed xs)
    | length' xs == 0 = renderRpn seed
    | otherwise = do
        renderRpn xs
        renderRpn seed
        mapM_ push $ concat $ replicate (length' xs) $ reprBOp op

reprSymbol :: Symbol -> String
reprSymbol sym =
    case sym of
         Unknown -> "UNKN"
         Infinity -> "INF"
         NegativeIntinity -> "NEGINF"
         Previous -> "PREV"
         PreviousOf ref -> "PREV(" ++ refName ref ++ ")"
         Counter -> "COUNT"
         Now -> "NOW"
         RecordTime UTC -> "TIME"
         RecordTime LocalTime -> "LTIME"

reprUOp :: UnaryOperator a b -> [String]
reprUOp op =
    case op of
         IsUnknown -> ["UN"]
         IsInfinity -> ["ISINF"]
         Bool2Double -> []
         Sin -> ["SIN"]
         Cos -> ["COS"]
         Log -> ["LOG"]
         Exp -> ["EXP"]
         Sqrt -> ["SQRT"]
         Atan -> ["ATAN"]
         Floor -> ["FLOOR"]
         Ceil -> ["CEIL"]
         Deg2Rad -> ["DEG2RAD"]
         Rad2Deg -> ["RAD2DEG"]
         Abs -> ["ABS"]
         Sort -> ["SORT"]
         Reverse -> ["REV"]
         Average -> ["AVG"]
         Tail -> ["POP"]

reprBOp :: BinaryOperator a b c -> [String]
reprBOp op =
    case op of
         Less -> ["LT"]
         Greater -> ["GT"]
         LessOrEqual -> ["LE"]
         GreaterOrEqual -> ["GE"]
         Equal -> ["EQ"]
         NotEqual -> ["NE"]
         Min -> ["MIN"]
         Max -> ["MAX"]
         Addition -> ["+"]
         Subtraction -> ["-"]
         Multiplication -> ["*"]
         Division -> ["/"]
         Modulo -> ["%"]
         AddNan -> ["ADDNAN"]
         Atan2 -> ["ATAN2"]
         Cons -> []
         Flip op -> ["EXC"] ++ reprBOp op

renderAggregate :: Aggregate -> Render ()
renderAggregate (Aggregate ref f) = push (refName ref) >> mapM_ push (reprAggregateFunction f)

reprAggregateFunction :: AggregateFunction -> [String]
reprAggregateFunction f =
    case f of
         Maximum -> ["MAXIMUM"]
         Minimum -> ["MINIMUM"]
         AverageValue -> ["AVERAGE"]
         StandardDeviation -> ["STDEV"]
         Last -> ["LAST"]
         First -> ["FIRST"]
         Total -> ["TOTAL"]
         Percent n -> [renderValue n, "PERCENT"]
         PercentNan n -> [renderValue n, "PERCENTNAN"]
         LeastSquareLineSlope -> ["LSLSLOPE"]
         LeastSquareLineInt -> ["LSLINT"]
         LeastSquareLineCorrelationCoefficient -> ["LSLCORREL"]
