module Css exposing (..)
import Html exposing (text)
import Html.Attributes exposing (style)
import List exposing (map)

type Display = Flex | Grid
type LengthUnit = Mm | Cm | Per | Px
type AngleUnit = Deg | Rad
type TimeUnit = Sec | Ms

type alias Angle = {size: Float, unit: AngleUnit}
type alias Length = {size: Float, unit: LengthUnit}
type alias Time = {size: Float, unit: TimeUnit}

type Keyword = Auto
             | Stretch

type CssVal a = Keyword Keyword
              | Quantity a

type Css = Display Display
         | Rotate (CssVal Angle)
         | Width (CssVal Length)
         | Height (CssVal Length)
         | AnimationDuration (CssVal Time)

cssStyle: Css -> Html.Attribute msg
cssStyle css =
    case css of
        Display display ->
            case display of
                Flex -> style "display" "flex"
                Grid -> style "display" "grid"
        Rotate val -> handleAngle "rotate" val
        Width val -> handleLength "width" val
        Height val -> handleLength "height" val
        AnimationDuration val -> handleTime "animation-duration" val

keywordName: Keyword -> String
keywordName keyword =
    case keyword of
        Auto -> "auto"
        Stretch -> "stretch"

angleUnitName: AngleUnit -> String
angleUnitName unit =
    case unit of
        Deg -> "deg"
        Rad -> "rad"

angleName: Angle -> String
angleName a = (String.fromFloat a.size) ++ (angleUnitName a.unit)

lengthUnitName: LengthUnit -> String
lengthUnitName unit =
    case unit of
        Mm -> "mm"
        Cm -> "cm"
        Per -> "%"
        Px -> "px"

lengthName: Length -> String
lengthName l = (String.fromFloat l.size) ++ (lengthUnitName l.unit)

                    
timeUnitName: TimeUnit -> String
timeUnitName unit =
    case unit of
        Sec -> "s"
        Ms -> "ms"

timeName: Time -> String
timeName t = (String.fromFloat t.size) ++ (timeUnitName t.unit)
                                 
handleAngle: String -> CssVal Angle -> Html.Attribute msg
handleAngle par val = 
    case val of
        Keyword k -> style par (keywordName k)
        Quantity l -> style par (angleName l)

handleLength: String -> CssVal Length -> Html.Attribute msg
handleLength par val =
    case val of
        Keyword k -> style par (keywordName k)
        Quantity l -> style par (lengthName l)

handleTime: String -> CssVal Time -> Html.Attribute msg
handleTime par val =
    case val of
        Keyword k -> style par (keywordName k)
        Quantity t -> style par (timeName t)

angle: Float -> AngleUnit -> CssVal Angle
angle size dim = Quantity <| Angle size dim

length: Float -> LengthUnit -> CssVal Length
length size dim = Quantity <| Length size dim

time: Float -> TimeUnit -> CssVal Time
time size dim = Quantity <| Time size dim

eg: List (Html.Attribute msg)
eg = map cssStyle [Display Flex, Rotate <| angle 90 Deg]
     
main = text "compiles"
       
