module EditableItem exposing (..)

import Data exposing (DocItem)

type Deletable a  
    = Pure a 
    | AskedForDelete a 
    | Deleted 

type alias Model = Deletable DocItem

model : Model 
model = Deleted

