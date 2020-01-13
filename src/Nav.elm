module Nav exposing 
    ( Page (..)
    , Model
    , Msg
    , view
    , update
    )

import Styles exposing 
    ( theme 
    , space 
    , font 
    )
import List exposing ( append )
import Element exposing
    ( Element 
    , Attribute
    , row 
    , padding 
    , spacing
    , width
    , height
    , fill
    , text
    , el 
    , centerX 
    , px
    )
import Element.Font as Font
import Element.Background as Bg
import Element.Border as Border
import Element.Input exposing
    ( button 
    )

type Page = Invoices | Clients | Settings

type alias Model = Page

type Msg = NavLinkClicked Model

update : Msg -> Model -> Model
update (NavLinkClicked page) _ = page

alpha : Maybe a -> List a
alpha x =
    case x of
        Just y -> [y]
        Nothing -> []

currentItemBgColor : Model -> Page ->  Maybe ( Element.Attribute Msg )
currentItemBgColor currentPage page =
  if currentPage == page
  then
    Just <| Bg.color theme.primary_dark
  else
    Nothing

noFocus : Attribute msg 
noFocus =
    Element.focused 
        [ Border.glow theme.primary_dark 2
        ]
            
link : Model -> Page -> String -> Element Msg
link model page label =
    let bgColor = currentItemBgColor model page
        staticAttrs = [ height fill, padding space.small, noFocus ]
        styles = append staticAttrs <| alpha bgColor
    in el styles 
        <| button [] 
            { onPress = Just <| NavLinkClicked page
            , label = text label
            } 

view : Model -> Element Msg
view page =
    let linkWithModel = link page
    in el 
        [ width fill
        , Bg.color theme.primary 
        , Font.color <| font.light
        ] <| el [ centerX, width <| px 960 ] 
            <| row 
                [ padding 0
                , spacing 0 
                , width fill
                ] 
                [ linkWithModel Invoices "Faktury"
                , linkWithModel Clients "Klienci"
                , linkWithModel Settings "Ustawienia"
                ] 
