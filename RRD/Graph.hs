module RRD.Graph where


import Bindings.Librrd
import RRD.Types
import RRD.Util (withCStringArray)
import Foreign.C.Error (throwErrnoIf_)
import Text.Printf
import Data.List


-- RrdOptions

type GraphOptions = [GraphOption]

data GraphOption 
    = VerticalTitle String
    | HorizontalTitle String
    | FullSize
    | OnlyGraph
    | Rigid
    | UpperLimit Double
    | LowerLimit Double
    | Width Int
    | Height Int
    | AltAutoscale
    | AltAutoscaleMin
    | AltAutoscaleMax
    | NoGridFit
    | XGrid String
    | YGrid String
    | AltYGrid
    | Logarithmic
    | UnitsExponent Int
    | UnitsLength Int
    | UnitsSi
    | RightAxis Double Double
    | RightAxisLabel String
    | RightAxisFormat String
    | NoLegend
    | ForceRulesLegend
    | LegendPosition String
    | LegendDirection String
    | GraphColor String String
    | GridDash Int Int
    | Border Int
    | DynamicLabels
    | Zoom Double
    | GraphFont String Int String
    | FontMode String
    | PangoMarkup
    | GraphMode String
    | SlopeMode
    | ImageFormat String
    | TabWidth Int
    | GraphBase Int
    | GraphWatermark String


-- {{{                   

-- |A horizontal string at the top of the graph
graphVerticalTitle :: String -> GraphOptions -> GraphOptions
graphVerticalTitle t = (:) (VerticalTitle t)

-- | Vertically placed string at the left hand side of the graph
graphHorizontalTitle :: String -> GraphOptions -> GraphOptions
graphHorizontalTitle t = (:) (HorizontalTitle t)


-- | Height of canvas, default height is 100 pixels, 
grapHeight :: Int -> GraphOptions -> GraphOptions
grapHeight h = (:) (Height h)

-- | Width of canvas, default width is 400 pixels
grapWidth :: Int -> GraphOptions -> GraphOptions
grapWidth w = (:) (Width w)

-- | If you specify the 'graphFullSize' option, the width and height specify the final dimensions of the
-- output image and the canvas is automatically resized to fit.
graphFullSize :: GraphOptions -> GraphOptions
graphFullSize = (:) FullSize

-- | If you specify the 'graphOnlyGraph' option and set the height < 32 pixels you will get a tiny graph image
-- (thumbnail) to use as an icon for use in an overview, for example. All labeling will be stripped off the
-- graph.
graphOnlyGraph :: GraphOptions -> GraphOptions
graphOnlyGraph = (:) OnlyGraph

----------------------------------------------------------
-- Limits

-- | By default the graph will be autoscaling so that it will adjust the y-axis to the range of the data. You
-- can change this behavior by explicitly setting the limits. The displayed y-axis will then range at least
-- 'graphLowerLimit' to 'graphUpperLimit'. Autoscaling will still permit those boundaries to be stretched
-- unless the 'graphRigid' option is set.
graphUpperLimit :: Double -> GraphOptions -> GraphOptions
graphUpperLimit u = (:) (UpperLimit u)

graphLowerLimit :: Double -> GraphOptions -> GraphOptions
graphLowerLimit l = (:) (LowerLimit l)

-- | Do not repmit to autoscale stretch given boundaries
graphRigid :: GraphOptions -> GraphOptions
graphRigid = (:) Rigid

-- | Sometimes the default algorithm for selecting the y-axis scale is not satisfactory. Normally the scale is
-- selected from a predefined set of ranges and this fails miserably when you need to graph something like 260
-- + 0.001 * sin(x). This option calculates the minimum and maximum y-axis from the actual minimum and maximum
-- data values. Our example would display slightly less than 260-0.001 to slightly more than 260+0.001 (this
-- feature was contributed by Sasha Mikheev).
graphAltAutoscale :: GraphOptions -> GraphOptions
graphAltAutoscale = (:) AltAutoscale

-- |Where 'graphAltAutoscale' will modify both the absolute maximum AND minimum values, this option will only
-- affect the minimum value. The maximum value, if not defined on the command line, will be 0. This option can
-- be useful when graphing router traffic when the WAN line uses compression, and thus the throughput may be
-- higher than the WAN line speed.
graphAltAutoscaleMin :: GraphOptions -> GraphOptions
graphAltAutoscaleMin = (:) AltAutoscaleMin

-- | Where --alt-autoscale will modify both the absolute maximum AND minimum values, this option will only
-- affect the maximum value. The minimum value, if not defined on the command line, will be 0. This option can
-- be useful when graphing router traffic when the WAN line uses compression, and thus the throughput may be
-- higher than the WAN line speed.
graphAltAutoscaleMax :: GraphOptions -> GraphOptions
graphAltAutoscaleMax = (:) AltAutoscaleMax


-- | In order to avoid anti-aliasing blurring effects RRDtool snaps points to device resolution pixels, this
-- results in a crisper appearance. If this is not to your liking, you can use this switch to turn this
-- behavior off.
--
-- Grid-fitting is turned off for PDF, EPS, SVG output by default.
graphNoGridFit :: GraphOptions -> GraphOptions
graphNoGridFit = (:) NoGridFit

----------------------------------------------------------
-- X-Axis

-- | The x-axis label is quite complex to configure. If you don't have very special needs it is probably best
-- to rely on the auto configuration to get this right. You can specify the string "none" to suppress the grid
-- and labels altogether.
--
-- The grid is defined by specifying a certain amount of time in the ?TM positions. You can choose from
-- SECOND, MINUTE, HOUR, DAY, WEEK, MONTH or YEAR. Then you define how many of these should pass between each
-- line or label. This pair (?TM:?ST) needs to be specified for the base grid (G??), the major grid (M??) and
-- the labels (L??). For the labels you also must define a precision in LPR and a strftime format string in
-- LFM. LPR defines where each label will be placed. If it is zero, the label will be placed right under the
-- corresponding line (useful for hours, dates etcetera). If you specify a number of seconds here the label is
-- centered on this interval (useful for Monday, January etcetera).

 -- > graphXGrid "MINUTE:10:HOUR:1:HOUR:4:0:%X"
 --
 --This places grid lines every 10 minutes, major grid lines every hour, and labels every 4 hours. The labels
 --are placed under the major grid lines as they specify exactly that time.
 --
 -- > graphXGrid "HOUR:8:DAY:1:DAY:1:86400:%A"
 --
 -- This places grid lines every 8 hours, major grid lines and labels each day. The labels are placed exactly
 -- between two major grid lines as they specify the complete day and not just midnight.
graphXGrid :: String -> GraphOptions -> GraphOptions
graphXGrid s = (:) (XGrid s)

----------------------------------------------------------
-- Y-Axis

-- | Y-axis grid lines appear at each grid step interval. Labels are placed every label factor lines. You can
-- specify "none" to suppress the grid and labels altogether. The default for this option is to automatically
-- select sensible values.
--
-- If you have set 'graphYGrid' to "none" not only the labels get suppressed, also the space reserved for the
-- labels is removed. You can still add space manually if you use the 'graphUnitsLength' command to explicitly
-- reserve space.
graphYGrid :: String -> GraphOptions -> GraphOptions
graphYGrid s = (:) (YGrid s)

-- | Place the Y grid dynamically based on the graph's Y range. The algorithm ensures that you always have a
-- grid, that there are enough but not too many grid lines, and that the grid is metric. That is the grid
-- lines are placed every 1, 2, 5 or 10 units. This parameter will also ensure that you get enough decimals
-- displayed even if your graph goes from 69.998 to 70.001. (contributed by Sasha Mikheev).
graphAltYGrid :: GraphOptions -> GraphOptions
graphAltYGrid = (:) AltYGrid

-- | Logarithmic y-axis scaling.
graphLogarithmic :: GraphOptions -> GraphOptions
graphLogarithmic = (:) Logarithmic

-- | This sets the 10**exponent scaling of the y-axis values. Normally, values will be scaled to the
-- appropriate units (k, M, etc.). However, you may wish to display units always in k (Kilo, 10e3) even if the
-- data is in the M (Mega, 10e6) range, for instance. Value should be an integer which is a multiple of 3
-- between -18 and 18 inclusively. It is the exponent on the units you wish to use. For example, use 3 to
-- display the y-axis values in k (Kilo, 10e3, thousands), use -6 to display the y-axis values in u (Micro,
-- 10e-6, millionths). Use a value of 0 to prevent any scaling of the y-axis values.
--
-- This option is very effective at confusing the heck out of the default RRDtool autoscaling function and
-- grid painter. If RRDtool detects that it is not successful in labeling the graph under the given
-- circumstances, it will switch to the more robust 'graphAltYGrid' mode.
graphUnitsExponent :: Int -> GraphOptions -> GraphOptions
graphUnitsExponent u = (:) (UnitsExponent u)

-- | How many digits should RRDtool assume the y-axis labels to be? You may have to use this option to make
-- enough space once you start fiddling with the y-axis labeling.
graphUnitsLength :: Int -> GraphOptions -> GraphOptions
graphUnitsLength l = (:) (UnitsLength l)

-- | With this option y-axis values on logarithmic graphs will be scaled to the appropriate units (k, M, etc.)
-- instead of using exponential notation. Note that for linear graphs, SI notation is used by default.
graphUnitsSi :: GraphOptions -> GraphOptions
graphUnitsSi = (:) UnitsSi




------------------------------------------------------------------------
-- Right Y Axis

-- | A second axis will be drawn to the right of the graph. It is tied to the left axis via the scale and
-- shift parameters. 
graphRightAxis :: Double -> Double -> GraphOptions -> GraphOptions
graphRightAxis scale shift = (:) (RightAxis scale shift)

-- | You can also define a label for the right axis.
graphRightAxisLabel :: String -> GraphOptions -> GraphOptions
graphRightAxisLabel l = (:) (RightAxisLabel l)

-- | By default the format of the axis labels gets determined automatically. If you want to do this your self,
-- use this option with the same %lf arguments you know from the PRINT and GPRINT commands.
graphRightAxisFormat :: String -> GraphOptions -> GraphOptions
graphRightAxisFormat f = (:) (RightAxisFormat f)


------------------------------------------------------------------------
-- Legend

-- | Suppress generation of the legend; only render the graph.
graphNoLegend :: GraphOptions -> GraphOptions
graphNoLegend = (:) NoLegend

-- | Force the generation of HRULE and VRULE legends even if those HRULE or VRULE will not be drawn because out
-- of graph boundaries (mimics behavior of pre 1.0.42 versions).
graphForceRulesLegend :: GraphOptions -> GraphOptions
graphForceRulesLegend = (:) ForceRulesLegend

-- | Place the legend at the given side of the graph. The default is south. In west or east position it is
-- necessary to add line breaks manually.
graphLegendNorth :: GraphOptions -> GraphOptions
graphLegendNorth = (:) (LegendPosition "north")

graphLegendSouth :: GraphOptions -> GraphOptions
graphLegendSouth = (:) (LegendPosition "south")

graphLegendWest :: GraphOptions -> GraphOptions
graphLegendWest = (:) (LegendPosition "west")

graphLegendEast :: GraphOptions -> GraphOptions
graphLegendEast = (:) (LegendPosition "east")

-- | Place the legend items in the given vertical order. The default is topdown. Using bottomup the legend
-- items appear in the same vertical order as a stack of lines or areas.
graphLegendTopdown :: GraphOptions -> GraphOptions
graphLegendTopdown = (:) (LegendDirection "topdown")

graphLegendBottomup :: GraphOptions -> GraphOptions
graphLegendBottomup = (:) (LegendDirection "bottomup")


-- | Override the default colors for the standard elements of the graph. The "COLORTAG" is one of "BACK"
-- background, "CANVAS" for the background of the actual graph, "SHADEA" for the left and top border, "SHADEB"
-- for the right and bottom border, "GRID", "MGRID" for the major grid, "FONT" for the color of the font,
-- "AXIS" for the axis of the graph, "FRAME" for the line around the color spots, and finally "ARROW" for the
-- arrow head pointing up and forward. Each color is composed out of three hexadecimal numbers specifying its
-- rgb color component (00 is off, FF is maximum) of red, green and blue. Optionally you may add another
-- hexadecimal number specifying the transparency (FF is solid). You may set this option several times to
-- alter multiple defaults.
-- 
-- A green arrow is made by: 
-- > graphColor "ARROW" "#00FF00"
graphColor :: String -> String -> GraphOptions -> GraphOptions
graphColor elt col | valid_elt elt = (:) (GraphColor elt col)
    where
        valid_elt :: String -> Bool
        valid_elt = flip elem ["COLORTAG", "BACK", "CANVAS", "SHADEA", "SHADEB", "GRID", "MGRID", "FONT", "AXIS", "FRAME", "ARROW"]




-- | By default the grid is drawn in a 1 on, 1 off pattern. With this option you can set this yourself
-- > graphGridDash 1 3         -- for a dot grid
-- > graphGridDash 1 0         -- for uninterrupted grid lines
graphGridDash :: Int -> Int -> GraphOptions -> GraphOptions
graphGridDash on off = (:) (GridDash on off)

-- | Width in pixels for the 3d border drawn around the image. Default 2, 0 disables the border.
graphBorder :: Int -> GraphOptions -> GraphOptions
graphBorder b = (:) (Border b)

-- | Pick the shape of the color marker next to the label according to the element drawn on the graph.
graphDynamicLabels :: GraphOptions -> GraphOptions
graphDynamicLabels = (:) DynamicLabels

-- | Zoom the graphics by the given amount. The factor must be > 0
graphZoom :: Double -> GraphOptions -> GraphOptions
graphZoom z = (:) (Zoom z)

-- | This lets you customize which font to use for the various text elements on the RRD graphs. "DEFAULT" sets
-- the default value for all elements, "TITLE" for the title, "AXIS" for the axis labels, "UNIT" for the vertical
-- unit label, "LEGEND" for the graph legend, "WATERMARK" for the watermark on the edge of the graph.
--
-- Use Times for the title: 
-- > graphFont "TITLE" 13 (Just "Times")
--
-- If you do not give a font string you can modify just the size of the default font: 
-- > graphFont "TITLE" 13 Nothing
--
-- If you specify the size 0 then you can modify just the font without touching the size. This is especially
-- useful for altering the default font without resetting the default fontsizes: 
-- > graphFont "DEFAULT" 0 (Just "Courier")
--
-- RRDtool comes with a preset default font. You can set the environment variable RRD_DEFAULT_FONT if you want
-- to change this.
--
-- RRDtool uses Pango for its font handling. This means you can to use the full Pango syntax when selecting
-- your font:
--
-- The font name has the form "[FAMILY-LIST] [STYLE-OPTIONS] [SIZE]", where FAMILY-LIST is a comma separated
-- list of families optionally terminated by a comma, STYLE_OPTIONS is a whitespace separated list of words
-- where each WORD describes one of style, variant, weight, stretch, or gravity, and SIZE is a decimal number
-- (size in points) or optionally followed by the unit modifier "px" for absolute size. Any one of the options
-- may be absent.
graphFont :: String -> Int -> Maybe String -> GraphOptions -> GraphOptions
graphFont elt s f | valid_elt elt = (:) (GraphFont elt s (maybe "." id f))
    where
        valid_elt :: String -> Bool
        valid_elt = flip elem ["DEFAULT", "TITLE", "AXIS", "UNIT", "LEGEND", "WATERMARK"]



-- | There are 3 font render modes:

-- | Full Hinting and Anti-aliasing (default)
graphFontNormal :: GraphOptions -> GraphOptions
graphFontNormal = (:) (FontMode "normal")

-- | Slight Hinting and Anti-aliasing
graphFontLight :: GraphOptions -> GraphOptions
graphFontLight = (:) (FontMode "light")

-- | Full Hinting and NO Anti-aliasing
graphFontMono :: GraphOptions -> GraphOptions
graphFontMono = (:) (FontMode "mono")

-- | All text in RRDtool is rendered using Pango. With the 'graphPangoMarkup' option, all text will be processed by
-- pango markup. This allows to embed some simple html like markup tags using
-- > <span key="value">text</span>
-- Apart from the verbose syntax, there are also the following short tags available.
-- > b     Bold
-- > big   Makes font relatively larger, equivalent to <span size="larger">
-- > i     Italic
-- > s     Strikethrough
-- > sub   Subscript
-- > sup   Superscript
-- > small Makes font relatively smaller, equivalent to <span size="smaller">
-- > tt    Monospace font
-- > u     Underline 
graphPangoMarkup :: GraphOptions -> GraphOptions
graphPangoMarkup = (:) PangoMarkup

-- }}}

-- | There are 2 render modes:

-- | Graphs are fully Anti-aliased (default)
graphModeNormal :: GraphOptions -> GraphOptions
graphModeNormal = (:) (GraphMode "normal")

-- | No Anti-aliasing
graphModeMono :: GraphOptions -> GraphOptions
graphModeMono = (:) (GraphMode "mono")

-- | RRDtool graphs are composed of stair case curves by default. This is in line with the way RRDtool
-- calculates its data. Some people favor a more 'organic' look for their graphs even though it is not all
-- that true.
graphSlopeMode :: GraphOptions -> GraphOptions
graphSlopeMode = (:) SlopeMode

-- | Image format for the generated graph. For the vector formats you can choose among the standard Postscript
-- fonts Courier-Bold, Courier-BoldOblique, Courier-Oblique, Courier, Helvetica-Bold, Helvetica-BoldOblique,
-- Helvetica-Oblique, Helvetica, Symbol, Times-Bold, Times-BoldItalic, Times-Italic, Times-Roman, and
-- ZapfDingbats.
graphImageFormat :: String -> GraphOptions -> GraphOptions
graphImageFormat f | valid_elt f = (:) (ImageFormat f)
    where
        valid_elt = flip elem ["PNG", "SVG", "EPS", "PDF"]

-- | By default the tab-width is 40 pixels, use this option to change it.
graphTabWidth :: Int -> GraphOptions -> GraphOptions
graphTabWidth t = (:) (TabWidth t)

-- | If you are graphing memory (and NOT network traffic) this switch should be set to *1024* so that one Kb
-- is 1024 byte. For traffic measurement, 1 kb/s is *1000* b/s.
graphBase :: Int -> GraphOptions -> GraphOptions
graphBase b = (:) (GraphBase b)

-- | Adds the given string as a watermark, horizontally centered, at the bottom of the graph.
graphWatermark :: String -> GraphOptions -> GraphOptions
graphWatermark w = (:) (GraphWatermark w)

renderGraphOptions :: GraphOptions -> [String]
renderGraphOptions = concatMap renderGraphOption
    where
        renderGraphOption (HorizontalTitle t) = ["--title", show t]
        renderGraphOption (VerticalTitle t)  = ["----vertical-label", show t]
        renderGraphOption (Width w) = ["--width", show w]
        renderGraphOption (Height h) = ["--height", show h]
        renderGraphOption (FullSize) = ["--full-size-mode"]
        renderGraphOption (OnlyGraph) = ["--only-graph"]
        renderGraphOption (UpperLimit u) = ["--upper-limit", show u]
        renderGraphOption (LowerLimit l) = ["--lower-limit", show l]
        renderGraphOption (Rigid) = ["--rigid"]
        renderGraphOption (AltAutoscale) = ["--alt-autoscale"]
        renderGraphOption (AltAutoscaleMin) = ["--alt-autoscale-min"]
        renderGraphOption (AltAutoscaleMax) = ["--alt-autoscale-max"]
        renderGraphOption (NoGridFit) = ["--no-gridfit"]
        renderGraphOption (XGrid s) = ["--x-grid", s]
        renderGraphOption (YGrid s) = ["--y-grid", s]
        renderGraphOption (Logarithmic) = ["--logarithmic"]
        renderGraphOption (UnitsExponent e) = ["--units-exponent", show e]
        renderGraphOption (UnitsLength l) = ["--units-length", show l]
        renderGraphOption (UnitsSi) = ["--units=si"]
        renderGraphOption (RightAxis s h) = ["--right-axis", show s ++ ":" ++ show h]
        renderGraphOption (RightAxisLabel s) = ["--right-axis-label", s]
        renderGraphOption (RightAxisFormat f) = ["--right-axis-format", f]
        renderGraphOption (LegendPosition p) = ["--legend-position", p]
        renderGraphOption (LegendDirection d) = ["--legend-direction", d]
        renderGraphOption (GraphColor e c) = ["--color", e++c]
        renderGraphOption (GridDash o f) = ["--grid-dash", show o ++ ":" ++ show f]
        renderGraphOption (Border b) = ["--border", show b]
        renderGraphOption (DynamicLabels) = ["--dynamic-labels"]
        renderGraphOption (Zoom z) = ["--zoom", show z]
        renderGraphOption (GraphFont e s f) = ["--font", e ++ ":" ++ show s ++ ":" ++ f]
        renderGraphOption (FontMode m) = ["--font-render-mode", m]
        renderGraphOption (PangoMarkup) = ["--pango-markup"]
        renderGraphOption (GraphMode m) = ["--graph-render-mode", m]
        renderGraphOption (SlopeMode) = ["--slope-mode"]
        renderGraphOption (ImageFormat f) = ["--imgformat", f]
        renderGraphOption (TabWidth t) = ["--tabwidth", show t]
        renderGraphOption (GraphBase b) = ["--base", show b]
        renderGraphOption (GraphWatermark w) = ["--watermark", w]



-- Ignored those
-- [--daemon address]
-- [-f|--imginfo printfstr]
--
