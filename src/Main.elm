module Main exposing ( main )

import Browser
import Html exposing ( Html )
import Element exposing 
    ( Element
    , layout
    , el
    , column
    )
import Settings as Settings exposing ( Model, Msg, default, view, update )
import Invoices as Inv exposing ( Model, Msg, view, update, default )
import Nav as Nav exposing ( Model, Msg, view, update )

type alias Model = 
    { counter : Int 
    , page : Nav.Model
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

type Tab = Invoices | Customers | Settings

type Msg 
    = NavLinkClicked Nav.Msg 
    | SettingsChanged Settings.Msg
    | InvoiceChanged Inv.Msg
    | Inc 
    | Dec

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        Inc -> ( {model | counter = model.counter + 11}, Cmd.none )
        Dec -> ( {model | counter = model.counter - 1}, Cmd.none )
        NavLinkClicked inner -> ( { model | page = Nav.update inner model.page }, Cmd.none )
        SettingsChanged settingsMsg -> ( { model | settings = Settings.update settingsMsg model.settings }, Cmd.none )
        InvoiceChanged invMsg ->
            let ( newData, innerMsg ) = Inv.update invMsg model.invData
            in ( { model | invData = newData } , Cmd.map InvoiceChanged innerMsg )

embed : (msg -> Msg) -> Element msg -> Element Msg
embed f m = Element.map f m

topBar : Nav.Model -> Element Msg
topBar page = el [] <| embed NavLinkClicked ( Nav.view page )

tabContent : Model -> Element Msg
tabContent model = 
    let tab = model.page
        in case tab of
            Nav.Invoices ->  embed InvoiceChanged <| Inv.view model.invData
            Nav.Clients -> Element.text "Customers"
            Nav.Settings -> embed SettingsChanged <| Settings.view model.settings

view : Model -> Html Msg
view model = 
    column [] 
        [ topBar model.page
        , tabContent model
        ] |> el [] |> layout []

init : () -> ( Model, Cmd Msg )
init _ = let 
            model =
                { counter = 0
                , page = Nav.Invoices
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