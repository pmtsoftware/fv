module Nav exposing 
    ( Model (..)
    , Msg
    , view
    , update
    )

import Element exposing
    ( Element 
    , row 
    , padding 
    , spacing
    , text
    )
import Element.Input exposing
    ( button 
    )

type Model = Invoices | Clients | Settings

type Msg = NavLinkClicked Model

update : Msg -> Model -> Model
update (NavLinkClicked page) _ = page

invoices : Element Msg
invoices =
    button []
        { onPress = Just <| NavLinkClicked Invoices
        , label  = text "Faktury"
        } 

settings : Element Msg
settings =
    button []
        { onPress = Just <| NavLinkClicked Settings
        , label  = text "Ustawienia"
        } 

clients : Element Msg
clients =
    button []
        { onPress = Just <| NavLinkClicked Clients
        , label  = text "Klienci"
        } 

view : Model -> Element Msg
view page =
    row [ padding 16, spacing 16 ] 
        [ invoices
        , clients
        , settings
        ] 