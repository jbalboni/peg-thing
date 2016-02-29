
import PegBoard exposing (update, view)
import PegGame exposing (createBoard)
import StartApp.Simple exposing (start)


main =
  start
    { model = createBoard 5
    , update = update
    , view = view
    }
