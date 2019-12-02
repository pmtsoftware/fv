module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Material.Typography as Typography
import Material.TabBar exposing (tab, tabBar, tabBarConfig, tabConfig)
import Settings as Settings exposing (Model, Msg, default, view, update )

type alias Model = 
    { counter : Int 
    , activeTab : Tab
    , settings : Settings.Model
    }

type Tab = Invoices | Customers | Settings

type Msg 
    = TabChanged Tab 
    | SettingsChanged Settings.Msg
    | Inc 
    | Dec

update : Msg -> Model -> Model
update msg model = 
    case msg of 
        Inc -> {model | counter = model.counter + 11}
        Dec -> {model | counter = model.counter - 1}
        TabChanged tab -> { model | activeTab = tab }
        SettingsChanged settingsMsg -> { model | settings = Settings.update settingsMsg model.settings }

topBar : Html Msg
topBar = tabBar tabBarConfig
            [ tab
                { tabConfig
                    | active = True
                    , onClick = Just (TabChanged Invoices)
                }
                { label = "Faktury", icon = Nothing }
            , tab 
                { tabConfig
                    | active = False
                    , onClick = Just (TabChanged Customers)
                }
                { label = "Klienci", icon = Nothing }
            , tab
                { tabConfig 
                    | active = False
                    , onClick = Just (TabChanged Settings)
                }
                { label =  "Ustawienia", icon = Nothing }
            ]

tabContent : Model -> Html Msg
tabContent model = 
    let tab = model.activeTab
        in case tab of
            Invoices -> text "Invoices"
            Customers -> text "Customers"
            Settings -> Html.map (\msg -> SettingsChanged msg) (Settings.view model.settings) 

view : Model -> Html Msg
view model = 
    div [ Typography.typography ]
        [ topBar
        , tabContent model
        ]

init : Model
init = { counter = 0
       , activeTab = Invoices
       , settings = Settings.default
       }

main : Program () Model Msg
main =
    Browser.sandbox 
        { init = init 
        , update = update
        , view = view }