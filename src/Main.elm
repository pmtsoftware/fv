module Main exposing ( main )

import Browser
import Html exposing ( Html, div, text )
import Material.Typography as Typography
import Material.TabBar exposing ( tab, tabBar, tabBarConfig, tabConfig )
import Settings as Settings exposing ( Model, Msg, default, view, update )
import Invoices as Inv exposing ( Model, Msg, view, update, default )

type alias Model = 
    { counter : Int 
    , activeTab : Tab
    , settings : Settings.Model
    , invData : Inv.Model
    }

type Tab = Invoices | Customers | Settings

type Msg 
    = TabChanged Tab 
    | SettingsChanged Settings.Msg
    | InvoiceChanged Inv.Msg
    | Inc 
    | Dec

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        Inc -> ( {model | counter = model.counter + 11}, Cmd.none )
        Dec -> ( {model | counter = model.counter - 1}, Cmd.none )
        TabChanged tab -> ( { model | activeTab = tab }, Cmd.none )
        SettingsChanged settingsMsg -> ( { model | settings = Settings.update settingsMsg model.settings }, Cmd.none )
        InvoiceChanged invMsg ->
            let ( newData, innerMsg ) = Inv.update invMsg model.invData
            in ( { model | invData = newData } , Cmd.map InvoiceChanged innerMsg )

topBar : Tab -> Html Msg
topBar active = tabBar tabBarConfig
            [ tab
                { tabConfig
                    | active = active == Invoices
                    , onClick = Just (TabChanged Invoices)
                }
                { label = "Faktury", icon = Nothing }
            , tab 
                { tabConfig
                    | active = active == Customers
                    , onClick = Just (TabChanged Customers)
                }
                { label = "Klienci", icon = Nothing }
            , tab
                { tabConfig 
                    | active = active == Settings
                    , onClick = Just (TabChanged Settings)
                }
                { label =  "Ustawienia", icon = Nothing }
            ]

tabContent : Model -> Html Msg
tabContent model = 
    let tab = model.activeTab
        in case tab of
            Invoices ->  Html.map ( \msg -> InvoiceChanged msg ) ( Inv.view model.invData )
            Customers -> text "Customers"
            Settings -> Html.map ( \msg -> SettingsChanged msg ) ( Settings.view model.settings ) 

view : Model -> Html Msg
view model = 
    div [ Typography.typography ]
        [ topBar model.activeTab
        , tabContent model
        ]

init : () -> ( Model, Cmd Msg )
init _ = let 
            model =
                { counter = 0
                , activeTab = Invoices
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