module Invoices 
    exposing 
        ( Model
        , Msg
        , view
        , update
        , default
        )


import Html exposing ( Html, div, text )
import Time exposing ( Posix, now, posixToMillis )
import Dict exposing ( Dict, insert, empty, toList )
import Task exposing ( perform )
import List exposing ( map )
import Debug exposing ( toString )
import Material.LayoutGrid as Grid exposing (layoutGrid, layoutGridInner, layoutGridCell)
import Material.Button exposing (buttonConfig, textButton, outlinedButton)

type alias Invoice =
    { number : Int
    , issue_date : String
    , supply_date : String
    , gross_value : Float
    , net_value : Float
    , remarks : String
    , items : List String }

type ViewMode = Catalog | Zoom Int

type alias Store a = Dict Int a

type alias Model =
    { docs : Store Invoice 
    , mode : ViewMode 
    }

default : Model
default = 
    { docs = empty
    , mode = Catalog }

type Msg
    = AddNewClicked 
    | AddNew Posix

newInvoice : Int -> Invoice
newInvoice timestamp = 
    { number = timestamp
    , issue_date = ""
    , supply_date = ""
    , gross_value = 0
    , net_value = 0
    , remarks = ""
    , items = [] 
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model
    = case msg of
        AddNewClicked -> ( model, perform AddNew now )
        AddNew time -> 
            let timestamp = posixToMillis time
                newDoc = newInvoice timestamp
            in ( { model | docs = insert timestamp newDoc model.docs } , Cmd.none)
            
catalog : Store Invoice -> Html Msg
catalog docs = 
    div [] 
        [ layoutGrid []
            [ layoutGridInner []
                [ layoutGridCell [ Grid.span12 ] [ addNewButton ]
                , layoutGridCell [] [ listOfDocs docs ]
                ]
            ]
        ]

listOfDocs : Store Invoice -> Html Msg
listOfDocs store =
    let 
        listOfPairs = toList store
        docs = map ( \(_, v) -> v ) listOfPairs
    in div [] ( map docToHtml docs )

docToHtml : Invoice -> Html Msg
docToHtml doc =
    div [] [ text (toString doc.number) ]

addNewButton : Html Msg
addNewButton = outlinedButton 
    { buttonConfig | onClick = Just AddNewClicked }
    "Nowa faktura"

view : Model -> Html Msg
view model
    = case model.mode of
        Catalog -> catalog model.docs
        Zoom key -> text "ala"
            
    