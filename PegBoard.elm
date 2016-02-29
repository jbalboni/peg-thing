module PegBoard where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Peg exposing (..)
import PegGame exposing (isValidMove, makeMove)


-- MODEL

type alias Model = PegGame.Board
type alias ID = PegGame.ID


-- UPDATE

type Action = Choose ID Peg.Action

update : Action -> Model -> Model
update action model =
  case action of
    Choose id pegAction ->
      let
        highlightedPegs = List.filter (\(_, pegModel, _) -> pegModel.highlighted == True) model.positions
        selectPeg (pegID, pegModel, connections) =
          if pegID == id && pegModel.pegged == True
            then (pegID, {pegModel | highlighted = True}, connections)
            else (pegID, {pegModel | highlighted = False}, connections)
        removeHighlight (id, peg, connections) =
          (id, {peg | highlighted = False}, connections)
      in
        case highlightedPegs of
          [] ->
            {model | positions = List.map selectPeg model.positions}
          highlighted::_ ->
            let
              (highlightedID, _, _) = highlighted
            in
              if isValidMove highlighted id model.positions
                then { model | positions = List.map (makeMove highlighted id) model.positions}
              else if highlightedID == id
                then {model | positions = List.map removeHighlight model.positions}
              else
                model


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [] (viewPegRow 1 address model.positions)

viewPegRow : Int -> Signal.Address Action -> PegGame.PegPositions -> List Html
viewPegRow rowSize address model =
  if List.isEmpty model
    then []
    else [div [rowStyle] (List.map (viewPeg address) (List.take rowSize model))] ++
        (viewPegRow (rowSize + 1) address (List.drop rowSize model))

viewPeg : Signal.Address Action -> PegGame.Position -> Html
viewPeg address (id, model, _) =
  Peg.view (Signal.forwardTo address (Choose id)) model


rowStyle : Attribute
rowStyle =
  style
    [ ("display", "flex")
    , ("width", "500px")
    , ("justify-content", "center")
    ]
