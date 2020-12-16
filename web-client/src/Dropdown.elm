module Dropdown exposing 
    ( Model
    )

import Element as Element exposing 
    ( Element
    )

type alias Model a = 
    { items : List a
    , selected : Maybe a
    }

init : List a -> Maybe a -> Model a
init xs selected = 
    { items = xs
    , selected = selected
    }

select : Model a -> a -> Model a
select model val = { model | selected = Just val }

type alias ItemView a msg  = 
    { view : a -> Element msg 
    , onClick : a -> msg
    }

view : Model a -> ItemView a msg -> Element msg
view model itemView = Element.none