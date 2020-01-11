module Styles exposing (..)

import Element exposing
    ( Color
    , rgb255 
    )

type alias Theme = 
    { primary : Color
    , primary_dark : Color 
    , primary_light_1 : Color 
    , primary_light_2 : Color 
    , primary_light_3 : Color 
    , primary_light_4 : Color 
    , primary_light_5 : Color 
    , primary_light_6 : Color 
    , primary_light_7 : Color 
    , primary_light_8 : Color 
    , secondary : Color 
    }

type alias Font =
    { light : Color 
    , dark : Color 
    }

type alias Space = 
    { xxSmall : Int
    , xSmall : Int
    , small : Int
    , normal : Int
    , large : Int
    , xLarge : Int
    , xxLarge : Int
    }

theme : Theme
theme =
    { primary = rgb255 67 66 69
    , primary_dark = rgb255 34 33 36
    , primary_light_1 = rgb255 99 97 101
    , primary_light_2 = rgb255 119 117 121
    , primary_light_3 = rgb255 160 158 162
    , primary_light_4 = rgb255 191 189 193
    , primary_light_5 = rgb255 226 224 228
    , primary_light_6 = rgb255 239 238 242
    , primary_light_7 = rgb255 246 245 249
    , primary_light_8 = rgb255 251 249 254
    , secondary = rgb255 23 113 229
    }

font : Font
font = 
    { dark = rgb255 34 33 36 
    , light = rgb255 251 249 254 }

space : Space
space = 
    { xxSmall = 2
    , xSmall = 4
    , small = 8
    , normal = 16
    , large = 32
    , xLarge = 64
    , xxLarge = 128
    }
