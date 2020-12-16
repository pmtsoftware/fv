module Entity exposing (..)

import Element exposing
    ( Element 
    , column 
    , el 
    , width 
    , px
    , spacing
    , fill
    )
import Inputs exposing 
    ( input 
    )
import Styles exposing 
    ( space 
    )

type alias Model = 
    { name : String 
    , vatin : String {- NIP in Poland -}
    , address1 : String
    , address2 : String
    }

type Msg 
    = NameChanged String
    | VatinChanged String 
    | Address1Changed String 
    | Address2Changed String 

default : Model 
default =
    { name = "" 
    , vatin = "" 
    , address1 = "" 
    , address2 = "" 
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChanged name ->
            ( { model | name = name }, Cmd.none )
        VatinChanged vatin ->
            ( { model | vatin = vatin }, Cmd.none )
        Address1Changed address1 ->
            ( { model | address1 = address1 }, Cmd.none )
        Address2Changed address2 ->
            ( { model | address2 = address2 }, Cmd.none )
            
label : String -> Element msg 
label caption =
    el 
        [ width <| px 80
        , Element.centerY
        ] 
        <| Element.text caption

nameField : Model -> Element Msg
nameField model =
    let l = label "Nazwa" 
    in input l model.name NameChanged

vatinField : Model -> Element Msg 
vatinField model =
    let l = label "VAT"
    in input l model.vatin VatinChanged

address1Field : Model -> Element Msg 
address1Field model = 
    let l = label "Adres 1"
    in input l model.address1 Address1Changed

address2Field : Model -> Element Msg 
address2Field model = 
    let l = label "Adres 2"
    in input l model.address2 Address2Changed

view : Model -> Element Msg
view model =
    column 
        [ spacing space.small
        , width fill 
        ]
        [ nameField model 
        , vatinField model 
        , address1Field model 
        , address2Field model 
        ]
