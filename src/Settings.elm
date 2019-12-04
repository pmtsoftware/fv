module Settings 
    exposing 
        ( Model
        , Msg
        , default
        , view
        , update
        )

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Material.TextField exposing (textField, textFieldConfig)
import Material.LayoutGrid as Grid exposing (layoutGrid, layoutGridInner, layoutGridCell)
import Material.HelperText exposing ( helperLine, characterCounter )

type alias Model =
    { name : String
    , street : String
    , zip_code : String
    , city : String
    , account : String 
    , vatin : String
    }

default : Model
default = 
    { name = ""
    , street = ""
    , zip_code = ""
    , city = ""
    , account = ""
    , vatin = ""
    }

type Msg 
    = NameChanged String 
    | StreetChanged String 
    | ZipCodeChanged String 
    | CityChanged String
    | AccountChanged String 
    | VatinChanged String

update : Msg -> Model -> Model
update msg settings 
    = case msg of
        NameChanged val -> { settings | name = val }
        StreetChanged val -> { settings | street = val }
        ZipCodeChanged val -> { settings | zip_code = val }
        CityChanged val -> { settings | city = val }
        AccountChanged val -> { settings | account = val }
        VatinChanged val -> { settings | vatin = val }

inputText : String -> String -> (String -> Msg) -> Maybe Int -> Html Msg
inputText label val toMsg max =
    textField
        { textFieldConfig 
            | label = Just label 
            , value = val
            , outlined = True
            , onInput = Just toMsg 
            , maxLength = max
            , additionalAttributes = [ style "width" "100%" ]
        }

nameField : Model -> Html Msg
nameField  model = 
    inputText "Nazwa" model.name NameChanged Nothing

streetField : Model -> Html Msg
streetField model = 
    inputText "Ulica" model.street StreetChanged Nothing

zipCodeField : Model -> Html Msg
zipCodeField model = 
    inputText "Kod pocztowy" model.zip_code ZipCodeChanged Nothing

cityField : Model -> Html Msg
cityField model = 
    inputText "Miejscowosc" model.city CityChanged Nothing

accountField : Model -> Html Msg
accountField model = 
    inputText "Numer konta bankowego" model.account AccountChanged (Just 26)

vatinField : Model -> Html Msg
vatinField model = 
    inputText "VAT" model.vatin VatinChanged Nothing

view : Model -> Html Msg
view settings = 
    div []
        [ layoutGrid []
            [ layoutGridInner []
                [ layoutGridCell [ Grid.span12 ] []

                , layoutGridCell [ Grid.span4 ] []
                , layoutGridCell [ Grid.span4 ] [ nameField settings ]
                , layoutGridCell [ Grid.span4 ] []

                , layoutGridCell [ Grid.span4 ] []
                , layoutGridCell [ Grid.span4 ] [ streetField settings ]
                , layoutGridCell [ Grid.span4 ] []

                , layoutGridCell [ Grid.span4 ] []
                , layoutGridCell [ Grid.span4 ] [ zipCodeField settings ]
                , layoutGridCell [ Grid.span4 ] []

                , layoutGridCell [ Grid.span4 ] []
                , layoutGridCell [ Grid.span4 ] [ cityField settings ]
                , layoutGridCell [ Grid.span4 ] []

                , layoutGridCell [ Grid.span4 ] []
                , layoutGridCell [ Grid.span4 ] [ vatinField settings ]
                , layoutGridCell [ Grid.span4 ] []

                , layoutGridCell [ Grid.span4 ] []
                , layoutGridCell [ Grid.span4 ] [ accountField settings, helperLine [] [ characterCounter [] ] ]
                , layoutGridCell [ Grid.span4 ] []
                ]
            ]
        ]