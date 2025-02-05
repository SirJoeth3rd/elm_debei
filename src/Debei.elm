module Debei exposing (..)
import Browser
import List
import Html exposing (..)
import Html.Attributes exposing (..)
import Css exposing (..)

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

dressHtml: Dress -> Html msg
dressHtml dress =
    img [src dress.url
        ,cssStyle <| Width (length 50 Px)
        ]
    []

-- wat wil ek he?
-- Width (LengthQuantity Px 60)
-- Width (Keyword Auto)
-- Width (CssFunction Add(CssVar("--some-var")))
-- So wat van ek maak hierdie defs private bv type CssFunction_ = Add LengthQuantity Lengthquantity | Mul
-- en dan maak ek die length type soos hierdie Length = LengthQuantity_ 

view: Checkout -> Html Msg
view checkout =
    div [cssStyle(Display Flex)] (List.map dressHtml checkout)
        
