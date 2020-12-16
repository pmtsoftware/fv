module EditableInvoice exposing (..)

import Platform.Cmd as Cmd
import Data exposing (Document, DocItemId)

type alias Model = 
    { data : Document 
    , itemAskedForDelete : Maybe DocItemId
    }

type Msg 
    = NumberFieldChanged String
    | RemarksFieldChanged String
    | BuyerNameChanged String
    | BuyerAddress1Changed String 
    | BuyerAddress2Changed String 
    | BuyerVatinChanged String 
    | SellerNameChanged String
    | SellerAddress1Changed String 
    | SellerAddress2Changed String 
    | SellerVatinChanged String 
    | ItemQtyChanged DocItemId String
    | ItemNameChanged DocItemId String
    | ItemVatRateChanged DocItemId String
    | ItemNetPriceChanged DocItemId String
    | ItemUnitChanged DocItemId String 
    | Nop


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)