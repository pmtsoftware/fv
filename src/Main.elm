module Main exposing ( main )

import Browser
import Html exposing ( Html )
import Element exposing 
    ( Element
    , layout
    , el
    , width 
    , fill 
    , column
    )
import Element.Font as Font
import Settings as Settings exposing ( Model, Msg, default, view, update )
import Invoices as Inv exposing ( Model, Msg, view, update, default, setCatalogMode )
import Nav as Nav exposing ( Model, Msg, view, update )

type alias Model = 
    { page : Nav.Model
    , settings : Settings.Model
    , invData : Inv.Model
    }

-- UNICODE polish characters
-- #104	A
-- #106 Ć
-- #118 Ę
-- #141 Ł
-- #143 Ń
-- #D3  Ó
-- #15A Ś
-- #179 Ź
-- #17B Ż
-- #105 ą
-- #107 ć
-- #119 ę
-- #142 ł
-- #144 ń
-- #F3  ó
-- #15B ś
-- #17A ź
-- #17C ż

type Msg 
    = NavLinkClicked Nav.Msg 
    | SettingsChanged Settings.Msg
    | InvoiceChanged Inv.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        NavLinkClicked inner -> 
            ( { model | page = Nav.update inner model.page, invData = Inv.setCatalogMode model.invData }
            , Cmd.none 
            )
        SettingsChanged settingsMsg -> ( { model | settings = Settings.update settingsMsg model.settings }, Cmd.none )
        InvoiceChanged invMsg ->
            let ( newData, innerMsg ) = Inv.update invMsg model.invData
            in ( { model | invData = newData } , Cmd.map InvoiceChanged innerMsg )

embed : (msg -> Msg) -> Element msg -> Element Msg
embed = Element.map 

topBar : Nav.Model -> Element Msg
topBar page = el [ width fill ] <| embed NavLinkClicked ( Nav.view page )

tabContent : Model -> Element Msg
tabContent model = 
    let tab = model.page
        in case tab of
            Nav.Invoices ->  embed InvoiceChanged <| Inv.view model.invData
            Nav.Clients -> Element.text "Customers"
            Nav.Settings -> embed SettingsChanged <| Settings.view model.settings

view : Model -> Html Msg
view model = 
    column 
        [ width fill 
        ] 
        [ topBar model.page
        , tabContent model
        ] 
        |> el 
            [ Font.family 
                [ Font.typeface "Open Sans"
                , Font.sansSerif
                ] 
            , Font.size 14
            , width fill
            ]
        |> layout []

init : () -> ( Model, Cmd Msg )
init _ = let 
            model =
                { page = Nav.Invoices
                , settings = Settings.default
                , invData = Inv.default
                }
            in ( model, Cmd.none )

subs : Model -> Sub Msg
subs _ = Sub.none

main : Program () Model Msg
main =
    Browser.element 
        { init = init 
        , update = update
        , view = view 
        , subscriptions  = subs
        }