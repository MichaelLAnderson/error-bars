module ChartWithErrorBars exposing (main)

import Browser
import Color
import Csv
import Csv.Decode
import Html exposing (Html, div, h1, node, p, text)
import Html.Attributes exposing (class)
import LineChart as LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk exposing (..)
import LineChart.Legends as Legends
import LineChart.Line as Line
import Result
import Svg exposing (Attribute, Svg, g, line, text_, tspan)
import Svg.Attributes as SvgA


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { hovering : Maybe Data
    , errorBarStyle : ErrorBarStyle
    , tooltipStyle : ToolTipStyle
    }


type alias ErrorBarStyle =
    { width : Float
    , color : Color.Color
    }


type alias ToolTipStyle =
    { offset_x : Float
    , offset_y : Float
    , boundaryOffset_x : Float
    , boundaryOffset_y : Float
    , lineSpace : Float
    , hoverBarWidth : Float
    , hoverColor : Color.Color
    }


init : Model
init =
    { hovering = Nothing
    , errorBarStyle =
        { width = 11
        , color = Color.purple
        }
    , tooltipStyle =
        { offset_x = 10
        , offset_y = -60
        , boundaryOffset_x = 80
        , boundaryOffset_y = 5
        , lineSpace = 14
        , hoverBarWidth = 11
        , hoverColor = Color.black
        }
    }



-- UPDATE


type Msg
    = Hover (Maybe Data)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Hover hovering ->
            { model | hovering = hovering }



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div
        [ class "container" ]
        [ chart model ]


chart : Model -> Html.Html Msg
chart model =
    LineChart.viewCustom
        { y = Axis.default 600 "km/s" .valueOfC
        , x = Axis.default 1000 "year" .year
        , container = Container.styled "line-chart-1" [ ( "font-family", "sans-serif" ), ( "font-size", "12px" ) ]
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.none
        , events = Events.hoverOne Hover
        , junk = Junk.custom (junk model)
        , grid = Grid.default
        , area = Area.default
        , line = Line.custom (\_ -> Line.style 0 (\x -> x)) -- makes a scatter plot
        , dots = customDotsConfig model.hovering
        }
        [ LineChart.line model.errorBarStyle.color Dots.diamond "dataset" dataset ]


junk : Model -> Coordinate.System -> Junk.Layers msg
junk { hovering, errorBarStyle, tooltipStyle } system =
    let
        maybeTooltips =
            case hovering of
                Just datum ->
                    tooltip datum system tooltipStyle

                Nothing ->
                    nullSvg
    in
    { below = [ sectionBand system, title system ]
    , above = [ maybeTooltips, errorBars system dataset errorBarStyle ]
    , html = []
    }


tooltip : Data -> Coordinate.System -> ToolTipStyle -> Svg msg
tooltip datum system tstyle =
    let
        svg_x =
            Coordinate.toSvgX system datum.year

        svg_y =
            Coordinate.toSvgY system datum.valueOfC

        offset_x =
            tstyle.offset_x

        offset_y =
            tstyle.offset_y

        bOffset_x =
            tstyle.boundaryOffset_x

        bOffset_y =
            tstyle.boundaryOffset_y

        lineSpace =
            tstyle.lineSpace

        eBarStyle =
            ErrorBarStyle tstyle.hoverBarWidth tstyle.hoverColor

        eBarHeight =
            Coordinate.scaleSvgY system datum.uncertainty

        x =
            if (svg_x + offset_x + bOffset_x) < Coordinate.toSvgX system system.x.max then
                String.fromFloat (svg_x + offset_x)

            else
                String.fromFloat (svg_x - bOffset_x)

        y =
            if (svg_y + offset_y) > Coordinate.toSvgY system system.y.max then
                String.fromFloat (svg_y + offset_y)

            else
                String.fromFloat (svg_y + bOffset_y)

        yWithLineSpace =
            String.fromFloat ((String.toFloat y |> Maybe.withDefault 0) + lineSpace)
    in
    Svg.g
        [ SvgA.style "text-anchor: start; font: 11px sans-serif;" ]
        [ Svg.text_ [ SvgA.x x, SvgA.y y ] [ Svg.text datum.observer ]
        , Svg.text_ [ SvgA.x x, SvgA.y yWithLineSpace ] [ Svg.text datum.method ]
        , errorBar svg_x svg_y eBarHeight system eBarStyle
        ]


nullSvg =
    Svg.circle [ SvgA.cx "0", SvgA.cy "0", SvgA.r "0" ] []


errorBars : Coordinate.System -> List Data -> ErrorBarStyle -> Svg msg
errorBars system data estyle =
    let
        x =
            Coordinate.toSvgX system

        y =
            Coordinate.toSvgY system

        e =
            Coordinate.scaleSvgY system
    in
    Svg.g [] (List.map (\u -> errorBar (x u.year) (y u.valueOfC) (e u.uncertainty) system estyle) data)


errorBar : Float -> Float -> Float -> Coordinate.System -> ErrorBarStyle -> Svg msg
errorBar x y barHeight system estyle =
    let
        barWidth =
            estyle.width

        color =
            Color.toCssString estyle.color

        xBarHigh =
            String.fromFloat (x + barWidth / 2)

        xBarMid =
            String.fromFloat x

        xBarLow =
            String.fromFloat (x - barWidth / 2)

        -- prevent the stem from running off the bottom of the chart
        yBarHigh =
            if (Coordinate.toSvgY system system.y.min + 1) < (y + barHeight / 2) then
                String.fromFloat (Coordinate.toSvgY system system.y.min)

            else
                String.fromFloat (y + barHeight / 2)

        -- prevent the stem from running off the top of the chart
        yBarLow =
            if (Coordinate.toSvgY system system.y.max - 1) > (y - barHeight / 2) then
                String.fromFloat (Coordinate.toSvgY system system.y.max)

            else
                String.fromFloat (y - barHeight / 2)
    in
    Svg.g []
        [ -- if the bottom bar runs off the bottom of the chart, or if bar height is zero, then do not display it
          if (y + barHeight / 2) < Coordinate.toSvgY system system.y.min && barHeight > 0 then
            line [ SvgA.stroke color, SvgA.x1 xBarHigh, SvgA.x2 xBarLow, SvgA.y1 yBarHigh, SvgA.y2 yBarHigh ] []

          else
            nullSvg

        --  vertical stem
        , line [ SvgA.stroke color, SvgA.x1 xBarMid, SvgA.x2 xBarMid, SvgA.y1 yBarHigh, SvgA.y2 yBarLow ] []

        -- if the top bar runs off the top of the chart, or if bar height is zero, then do not diplay it
        , if (y - barHeight / 2) > Coordinate.toSvgY system system.y.max && barHeight > 0 then
            line [ SvgA.stroke color, SvgA.x1 xBarHigh, SvgA.x2 xBarLow, SvgA.y1 yBarLow, SvgA.y2 yBarLow ] []

          else
            nullSvg
        ]


sectionBand : Coordinate.System -> Svg msg
sectionBand system =
    Junk.rectangle system [ SvgA.fill "#06060688" ] system.x.min system.x.max 299792 299793


title : Coordinate.System -> Svg msg
title system =
    Svg.g [ SvgA.style "text-anchor: start; font: 18px sans-serif;" ]
        [ labelAt system 1869 300600 0 0 "middle" Color.black "Historical Speed of Light Measurements"
        , Svg.g [ SvgA.style "text-anchor: start; font: 12px sans-serif;" ]
            [ labelAt system 1869 300545 0 0 "middle" Color.black "Best 57 values of c by 16 methods" ]
        , Svg.g [ SvgA.style "text-anchor: start; font: 11px sans-serif;" ]
            [ labelAt system 1790 299803 0 0 "middle" Color.black "modern accepted value = 299792.458 km/s" ]
        ]


customDotsConfig : Maybe Data -> Dots.Config Data
customDotsConfig maybeHovered =
    let
        styleLegend _ =
            Dots.disconnected 8 2

        styleIndividual datum =
            if Just datum == maybeHovered then
                Dots.empty 4 2

            else
                Dots.disconnected 8 2
    in
    Dots.customAny
        { legend = styleLegend
        , individual = styleIndividual
        }



--  CSV


type alias Data =
    { number_ : Int
    , year : Float
    , observer : String
    , method : String
    , valueOfC : Float
    , uncertainty : Float
    }



-- only strings for the first pass decode, because I couldn't figure out
-- how to make Csv.Decode.andMap ( Csv.Decode.field "valueOfC" String.toFloat )... work


type alias OnlyStrings =
    { a : String
    , b : String
    , c : String
    , d : String
    , e : String
    , f : String
    }


dataset : List Data
dataset =
    List.map (\onlystring -> transformOnlyStringsToData onlystring) (parsedStrings resultWithErrors)


transformOnlyStringsToData : OnlyStrings -> Data
transformOnlyStringsToData onlystrings =
    Data (String.toInt onlystrings.a |> Maybe.withDefault 0)
        (String.toFloat onlystrings.b |> Maybe.withDefault 0)
        onlystrings.c
        onlystrings.d
        (String.toFloat onlystrings.e |> Maybe.withDefault 0)
        (String.toFloat onlystrings.f |> Maybe.withDefault 0)



-- Throwing away the errors.  Not so robust, but expedient.


parsedStrings : Result.Result Csv.Decode.Errors (List OnlyStrings) -> List OnlyStrings
parsedStrings result =
    case result of
        Ok listOfOnlyStrings ->
            listOfOnlyStrings

        Err error ->
            [ OnlyStrings "" "" "" "" "" "" ]


resultWithErrors : Result.Result Csv.Decode.Errors (List OnlyStrings)
resultWithErrors =
    Csv.parse historicSpeedOfLightCSV |> Csv.Decode.decodeCsv decoder


decoder : Csv.Decode.Decoder (OnlyStrings -> a) a
decoder =
    Csv.Decode.map OnlyStrings
        (Csv.Decode.next Ok
            |> Csv.Decode.andMap (Csv.Decode.next Ok)
            |> Csv.Decode.andMap (Csv.Decode.next Ok)
            |> Csv.Decode.andMap (Csv.Decode.next Ok)
            |> Csv.Decode.andMap (Csv.Decode.next Ok)
            |> Csv.Decode.andMap (Csv.Decode.next Ok)
        )



-- DATA
-- "The Atomic Constants, Light, and Time," Barry Setterfield and Trevor Norman, 1987
-- best 57 values of c by 16 methods
-- missing uncertainty data represented as 0


historicSpeedOfLightCSV =
    """
NO.,DATE,OBSERVER,METHOD,VALUE OF C (Km/s), UNCERTAINTY
1,1740,Bradley,Aberration,300650,0
2,1783,Lindenau,Aberration,300460,170
3,1843,Struve,Aberration,300020,160
4,1861,Glasenapp,Jupiter Satellite,300050,
5,1874.8,Cornu (Helmert),Toothed Wheel,299990,200
6,1874.8,Cornu (Dorsey),Toothed Wheel,299900,200
7,1876.5,Harvard Observat.,Jupiter Satellite,299921,13
8,1879.5,Michelson,Rotating Mirror,299910,50
9,1882.7,Newcomb,Rotating Mirror,299860,30
10,1882.8,Michelson,Rotating Mirror,299853,60
11,1883,Nyren,Aberration,299850,90
12,1900.4,Perrotin,Toothed Wheel,299900,80
13,1902.4,Perrotin,Toothed Wheel,299860,80
14,1902.4,Perrotin/Prim,Toothed Wheel,299901,84
15,1906,Rosa and Dorsey,Electromag. Units,299803,30
16,1923,Mercier,Waves on Wires,299795,30
17,1924.6,Michelson,Polygonal Mirror,299802,30
18,1926.5,Michelson,Polygonal Mirror,299798,15
19,1928,Mittelstaedt,Kerr Cell,299786,10
20,1932.5,Pease/Pearson,Polygonal Mirror,299774,10
21,1936.8,Anderson,Kerr Cell,299771,10
22,1937,Huttel,Kerr Cell,299771,10
23,1940,Anderson,Kerr Cell,299776,10
24,1947,"Essen,Gordon-Smith",Cavity Resonator,299798,3
25,1947,"Essen,Gordon-Smith",Cavity Resonator,299792,3
26,1949,Aslakson,Radar,299792.4,2.4
27,1949,Bergstrand,Geodimeter,299796,2
28,1950,Essen,Cavity Resonator,299792.5,1
29,1950,Hansen and Bol,Cavity Resonator,299794.3,1.2
30,1950,Bergstrand,Geodimeter,299793.1,0.26
31,1951,Bergstrand,Geodimeter,299793.1,0.4
32,1951,Aslakson,Radar,299794.2,1.4
33,1951,Froome,Radio Interferom.,299792.6,0.7
34,1953,Bergstrand,Geodimeter,299792.85,0.16
35,1954,Froome,Radio Interferom.,299792.75,0.3
36,1954,Florman,Radio Interferom.,299795.1,3.1
37,1955,Scholdstrom,Geodimeter,299792.4,0.4
38,1955,Plyler et. al.,Spectral Lines,299792,6
39,1956,Wadley,Tellurometer,299792.9,2
40,1956,Wadley,Tellurometer,299792.7,2
41,1956,Rank et. al.,Spectral Lines,299791.9,2
42,1956,Edge,Geodimeter,299792.4,0.11
43,1956,Edge,Geodimeter,299792.2,0.13
44,1957,Wadley,Tellurometer,299792.6,1.2
45,1958,Froome,Radio Interferom.,299792.5,0.1
46,1960,Kolibayev,Geodimeter,299792.6,0.06
47,1966,Karolus,Modulated Light,299792.44,0.2
48,1967,Simkin et. al.,Microwave Interf.,299792.56,0.11
49,1967,Grosse,Geodimeter,299792.5,0.05
50,1972,"Bay,Luther,White",Laser,299792.462,0.018
51,1972,NBS (Boulder),Laser,299792.46,0.006
52,1973,Evenson et. al.,Laser,299792.4574,0.0011
53,1973,"NRC, NBS",Laser,299792.458,0.002
54,1974,Blaney et. al.,Laser,299792.459,0.0008
55,1978,Woods et. al.,Laser,299792.4588,0.0002
56,1979,Baird et. al.,Laser,299792.4581,0.0019
57,1983,NBS (US),Laser,299792.4586,0.0003"""
