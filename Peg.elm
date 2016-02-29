module Peg where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import PegGame exposing (..)


-- MODEL

type alias Model = PegGame.Peg


-- UPDATE

type Action = Choose


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case (model.pegged, model.highlighted) of
    (True, False) ->
      div [peggedStyle, onClick address Choose] []

    (True, True) ->
      div [highlightedStyle, onClick address Choose] []

    (False, _) ->
      div [unpeggedStyle, onClick address Choose] []


peggedStyle : Attribute
peggedStyle =
  style
    [ ("border-radius", "50%")
    , ("background-color", "red")
    , ("border", "1px solid red")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("height", "50px")
    ]

highlightedStyle : Attribute
highlightedStyle =
  style
    [ ("border-radius", "50%")
    , ("background-color", "red")
    , ("border", "1px solid red")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("height", "50px")
    , ("box-shadow", "0px 0px 5px rgba(0, 0, 0, 0.8)")
    ]

unpeggedStyle : Attribute
unpeggedStyle =
  style
    [ ("border-radius", "50%")
    , ("background-color", "transparent")
    , ("border", "1px solid grey")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("height", "50px")
    ]
