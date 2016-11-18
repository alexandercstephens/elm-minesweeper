import List.Extra exposing (unique)
import Html exposing (Html, button, div, text, span)
import Html.App as App
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Json
import Random
import Time exposing (Time, second)


main =
  App.program 
    { init = init
    , view = view
    , update = update 
    , subscriptions = subscriptions
    }

(?) b (x, y) = if b then x else y


-- MODEL

boardSize : Int
boardSize = 10

numBombs : Int
numBombs = 12

squareSize : Int
squareSize = 25

type alias TileLocation = (Int, Int)
type alias Model = 
  { time: Time
  , bombs: List TileLocation
  , explored: List TileLocation
  , flagged: List TileLocation
  }

generateBomb : Int -> TileLocation
generateBomb n = (n // boardSize, n % boardSize)

init : (Model, Cmd Msg)
init =
  ( { time = 0
    , bombs = []
    , explored = []
    , flagged = []
    }
  , Random.generate InitBoard 
      (Random.list numBombs 
        (Random.map generateBomb 
          (Random.int 1 (boardSize*boardSize)))))


-- UPDATE

type Msg =
  GenerateBoard
  | InitBoard (List TileLocation)
  | Explore TileLocation
  | ToggleFlag TileLocation
  | Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateBoard ->
      init
      
    InitBoard n ->
      ( { model | bombs = unique n }
      , Cmd.none
      )

    Explore location ->
      ( if (location `List.member` model.explored) then
          model
        else
          { model |
            explored = model.explored ++
              (spacesToExplore [location] model.bombs model.explored)
          }
      , Cmd.none
      )

    ToggleFlag location ->
      ( if (location `List.member` model.flagged) then
          { model | flagged = List.filter ((/=) location) model.flagged }
        else
          { model | flagged = model.flagged ++ [location] }
      , Cmd.none
      )

    Tick ->
      ( { model | time = model.time + 1 }
      , Cmd.none
      )

-- input: tuple
-- output: list of tuples surrounding that tuple, within the board
surroundingSpaces : TileLocation -> List TileLocation
surroundingSpaces (x, y) =
  List.filter (\(x, y) -> (x >= 0) && (x < boardSize) && (y >= 0) && (y < boardSize))
    [ (x - 1, y - 1)
    , (x - 1, y    )
    , (x - 1, y + 1)
    , (x    , y - 1)
    , (x    , y + 1)
    , (x + 1, y - 1)
    , (x + 1, y    )
    , (x + 1, y + 1)
    ]

-- returns the number of bombs surrounding this coordinate
dangerFactor : TileLocation-> List TileLocation -> Int
dangerFactor location bombs =
  List.length
    (List.filter
      (\location -> location `List.member` bombs)
      (surroundingSpaces location)
    )

spacesToExplore : List TileLocation -> List TileLocation -> List TileLocation -> List TileLocation
spacesToExplore toExplore bombs explored =
  case toExplore of
    [] -> []
    (location::locations) ->
      --if already explored, ignore
      if location `List.member` explored then
          spacesToExplore locations bombs explored

      --if no surrounding bombs, explore and explore surrounding spaces too
      else if dangerFactor location bombs == 0 then
          location::(spacesToExplore (locations ++ surroundingSpaces location) bombs (location::explored))

      --otherwise, just explore this space
      else
          location::(spacesToExplore locations bombs (location::explored))


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second (\_ -> Tick)


-- VIEW

onRightClick message =
  onWithOptions
    "contextmenu"
    { stopPropagation = True
    , preventDefault = True
    }
    (Json.succeed message)

remainingBombCount : Model -> Int
remainingBombCount model =
  List.length model.bombs - List.length model.flagged

view : Model -> Html Msg
view model =
  let
    lost = List.any (\location -> location `List.member` model.bombs) model.explored
  in
    div []
      [ div
          [ style [("position", "absolute"), ("left", px 45), ("line-height", px 28)] ]
          [ text (toString (remainingBombCount model)) ]
      , div
          [ style [("position", "absolute"), ("left", px (squareSize * boardSize - 20)), ("line-height", px 28)] ]
          [ text (toString model.time) ]
      , div
          [ onClick GenerateBoard ]
          [ button [ style resetButtonStyle, class (lost ? ("fa fa-frown-o", "fa fa-smile-o"))] [] ]
      , div
          [ style [("position", "absolute"), ("left", px 20), ("top", px 40), ("overflow", "hidden")] ]
          (List.map (rowView model lost) [0..(boardSize - 1)])
      ]

rowView : Model -> Bool -> Int -> Html Msg
rowView model lost x =
  div
    [ style [("height", px squareSize)] ]
    (List.indexedMap (tileView model lost) (List.repeat boardSize x))
  
tileView : Model -> Bool -> Int -> Int -> Html Msg
tileView model lost x y =
  if ((x, y) `List.member` model.explored) then
    if (x, y) `List.member` model.bombs then
      exploredBombView True
    else
      exploredTileView (dangerFactor (x, y) model.bombs)
  else
    if (x, y) `List.member` model.bombs && lost then
      exploredBombView False
    else if (x, y) `List.member` model.flagged then
      flaggedTileView lost (x, y)
    else
      unexploredTileView lost (x, y)

exploredTileView : Int -> Html Msg
exploredTileView df =
  div
    [ style
      ( tileStyle
      ++ exploredStyle
      )
    ]
    [ if df == 0 then
        text ""
      else
        text (toString (df))
    ]

exploredBombView : Bool -> Html Msg
exploredBombView exploded =
  div
    [ style
      ( tileStyle
      ++ exploredStyle
      ++ if exploded then
          [("background-color", "red")]
        else
          []
      )
    ]
    [ text "*" ]

flaggedTileView : Bool -> TileLocation -> Html Msg
flaggedTileView lost location =
  div
    ( [ style
        ( tileStyle
        ++ unexploredStyle
        )
      , class "fa fa-flag"
      ]
    ++ if not lost then
        [ onRightClick (ToggleFlag location) ]
      else
        []
    )
    [ ]

unexploredTileView : Bool -> TileLocation -> Html Msg
unexploredTileView lost location =
  div
    ( [ style
        ( tileStyle
        ++ unexploredStyle
        )
      ]
    ++ if not lost then
        [ onClick (Explore location)
        , onRightClick (ToggleFlag location)
        ]
      else
        []
    )
    [ ]

tileStyle : List (String, String)
tileStyle =
  List.concat
    [ [ ("display", "inline-block")
      , ("background-color", "gray")
      , ("vertical-align", "top")
      , ("text-align", "center")
      , ("line-height", px squareSize)
      ]
    , square squareSize
    ]

unexploredStyle : List (String, String)
unexploredStyle =
  [ ("background-color", "gray")
  , ("box-shadow", "inset -2px -2px #333, inset 2px 2px #ccc")
  ]
exploredStyle : List (String, String)
exploredStyle =
  [ ("background-color", "white")
  , ("box-shadow", "inset 0 0 0 1px #eee")
  ]

resetButtonStyle : List (String, String)
resetButtonStyle =
  [ ("background-color", "lightgray")
  , ("padding", "4px")
  , ("border-radius", "2px")
  , ("position", "absolute")
  , ("left", px (6 + squareSize * boardSize // 2))
  ]

px : Int -> String
px x = (toString x) ++ "px"

square : Int -> List (String, String)
square x = [("width", px x), ("height", px x)]
