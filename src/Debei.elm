module Debei exposing (..)
import Browser
import List
import Html exposing (..)
import Html.Attributes exposing (..)

main = Browser.element
       { init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       }
    
type alias Dress = {url: String 
             ,price: Int
             ,name: String
             ,color: String
             }
    
type alias Checkout = List Dress -- Model is just list of dresses in checkout
type Msg = AddDress Dress
    
init: List Dress -> (Checkout, Cmd Msg) -- flags is original list of dresses
init dresses =
    (dresses, Cmd.none)

update: Msg -> Checkout -> (Checkout, Cmd Msg)
update msg checkout =
    case msg of
        AddDress dress -> (checkout ++ [dress], Cmd.none)

subscriptions: Checkout -> Sub Msg
subscriptions _ =
    Sub.none
        
-- ALL HTML PRODUCING FUNCTIONS
type alias Color = String
type alias BackgroundColor = Color
type Display = Flex | Grid
type alias Height = Length
type alias Width = Length

type LengthUnit = Px | Cm | Mm | Per | Em | Rem
type alias Length = {unit: LengthUnit, length: Float}
    
lengthString: Length -> String
lengthString length =
    let unitstring =
            case length.unit of
                Px -> "px"
                Cm -> "cm"
                Mm -> "mm"
                Per -> "%"
                Em -> "em"
                Rem -> "rem"
    in
        unitstring ++ " " ++ (String.fromFloat length.length)
                         
            
type Css
    = Display Display
      | Color Color
      | BackgroundColor Color
      | Width Length
      | Height Length
      
cssStyle: Css -> Html.Attribute msg
cssStyle css =
    case css of
        Display val ->
            case val of
                Flex -> style "display" "flex"
                Grid -> style "display" "grid"
        BackgroundColor val -> style "background-color" val
        Color val -> style "color" val
        Width val -> style "width" (lengthString val)
        Height val -> style "height" (lengthString val)
                
                           

dressHtml: Dress -> Html msg
dressHtml dress =
    img [src dress.url
         ,cssStyle Width 
        ]
    []

view: Checkout -> Html Msg
view checkout =
    div [cssStyle(Display Flex)] (List.map dressHtml checkout)
        
