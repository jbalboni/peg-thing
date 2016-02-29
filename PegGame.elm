module PegGame where
import Triangles exposing (..)


type alias ID = Int
type alias Peg = {pegged : Bool, highlighted : Bool}
type alias Connection = {to : Int, over : Int}
type alias Position = (ID, Peg, List Connection)
type alias PegPositions = List Position
type alias Board = { positions : PegPositions, rows : Int }


createBoard : Int -> Board
createBoard rows =
  let
    size = (rowTriangle rows)
  in
    { rows = rows, positions =
      (generatePositions size)
        |> (connect rows size)
        |> (removeStartPeg size rows)
    }

removeStartPeg : Int -> Int -> PegPositions -> PegPositions
removeStartPeg size rows positions =
  let
    removePeg position =
      let
        (id,peg,connections) = position
        removeID = size - (rows // 2)
      in
        if id == removeID
          then (id, {peg | pegged = False}, connections)
          else position
  in
    List.map removePeg positions

generatePositions : Int -> PegPositions
generatePositions size =
  case size of
    1 ->
      [(1, {pegged = True, highlighted = False}, [])]
    _ ->
      (generatePositions (size - 1)) ++
        [(size, {pegged = True, highlighted = False}, [])]

connectPos : Int -> Int -> Int -> PegPositions -> PegPositions
connectPos from to over positions =
  let
    addConnection position =
      let
        (id, peg, connections) = position
      in
        if from == id
          then (id, peg, {to = to, over = over} :: connections)
        else
          position
  in
    List.map addConnection positions

connectRight : ID -> PegPositions -> PegPositions
connectRight current positions =
  let
    neighbor = current + 1
    destination = neighbor + 1
  in
    if not (isTriangular neighbor || isTriangular current)
      then (connectPos current destination neighbor positions)
        |> (connectPos destination current neighbor)
    else
      positions

connectDownLeft : ID -> Int -> PegPositions -> PegPositions
connectDownLeft current size positions =
  let
    row = rowNum current
    neighbor = current + row
    destination = row + neighbor + 1
  in
    if destination <= size
      then (connectPos current destination neighbor positions)
        |> (connectPos destination current neighbor)
      else positions

connectDownRight : ID -> Int -> PegPositions -> PegPositions
connectDownRight current size positions =
  let
    row = rowNum current
    neighbor = current + row + 1
    destination = row + neighbor + 2
  in
    if destination <= size
      then (connectPos current destination neighbor positions)
        |> (connectPos destination current neighbor)
      else positions

connect : Int -> Int -> PegPositions -> PegPositions
connect rows size positions =
  let
    setConnections (current, _, _) currentPositions =
      connectRight current currentPositions
        |> (connectDownRight current size)
        |> (connectDownLeft current size)
  in
    List.foldl setConnections positions positions

isValidMove : Position -> ID -> PegPositions -> Bool
isValidMove (_,_,connections) toID positions =
  case List.filter (\x -> x.to == toID) connections of
    [] ->
      False
    connection::_ ->
      (case List.filter (\(id,_,_) -> id == connection.over) positions of
        [] ->
          False
        (_,peg,_)::_ ->
          peg.pegged)
      &&
      (case List.filter (\(id,_,_) -> id == toID) positions of
        [] ->
          False
        (_,peg,_)::_ ->
          not peg.pegged)

jumpedPeg : List Connection -> ID -> ID
jumpedPeg connections toID =
  case List.filter (\x -> x.to == toID) connections of
    [] ->
      0
    connection::_ ->
      connection.over

makeMove : Position -> ID -> Position -> Position
makeMove (fromID, _, fromConnections) id (pegID, pegModel, connections) =
  if fromID == pegID
    then (pegID, {pegModel | pegged = False, highlighted = False}, connections)
  else if pegID == id
    then (pegID, {pegModel | pegged = True, highlighted = False}, connections)
  else if (jumpedPeg fromConnections id) == pegID
    then (pegID, {pegModel | pegged = False}, connections)
  else (pegID, {pegModel | highlighted = False}, connections)
