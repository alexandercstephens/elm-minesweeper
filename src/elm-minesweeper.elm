import Html exposing (Html, button, div, text, span)
import Html.App as App
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Random


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
numBombs = 20

squareSize : Int
squareSize = 25

type alias Model = 
  { bombs: List { x : Int, y : Int }
  , explored: List { x : Int, y : Int }
  }

generateBomb : Int -> { x : Int, y : Int }
generateBomb n = { x = n // boardSize, y = n % boardSize }

init : (Model, Cmd Msg)
init =
  ( { bombs = []
    , explored = []
    }
  , Random.generate InitBoard 
      (Random.list numBombs 
        (Random.map generateBomb 
          (Random.int 1 (boardSize*boardSize)))))


-- UPDATE

type Msg = GenerateBoard | InitBoard (List { x : Int, y : Int }) | Explore Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateBoard ->
      init
      
    InitBoard n ->
      ( { model | bombs = n }
      , Cmd.none)

    Explore x' y' ->
      ( if (inCoordinateList x' y' model.explored) then
          model
        else
          { model |
            explored = model.explored ++
              List.map
              (\(x, y) -> {x=x, y=y})
              (spacesToExplore [(x', y')] model.bombs model.explored)
          }
      , if (inCoordinateList x' y' model.bombs) then
          Cmd.none -- TODO lose
        else
          Cmd.none)


inCoordinateList : Int -> Int -> List { a | x : Int, y : Int } -> Bool
inCoordinateList x' y' coordinateList =
  List.any (\{x, y} -> x==x' && y==y') coordinateList

-- input: tuple
-- output: list of tuples surrounding that tuple, within the board
surroundingSpaces : (Int, Int) -> List (Int, Int)
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
dangerFactor : Int -> Int -> List { a | x: Int, y: Int } -> Int
dangerFactor x y bombs =
  List.length
    (List.filter
      (\(x, y) -> inCoordinateList x y bombs)
      (surroundingSpaces (x, y))
    )

spacesToExplore : List (Int, Int) -> List { x: Int, y: Int } -> List { x: Int, y: Int } -> List (Int, Int)
spacesToExplore toExplore bombs explored =
  case toExplore of
    [] -> []
    ((x, y)::xys) ->
      --if already explored, ignore
      if inCoordinateList x y explored then
          spacesToExplore xys bombs explored

      --if no surrounding bombs, explore and explore surrounding spaces too
      else if dangerFactor x y bombs == 0 then
          (x, y)::(spacesToExplore (xys ++ surroundingSpaces (x, y)) bombs ({x=x, y=y}::explored))

      --otherwise, just explore this space
      else
          (x, y)::(spacesToExplore xys bombs ({x=x, y=y}::explored))


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  let
    lost = List.any (\{x, y} -> inCoordinateList x y model.bombs) model.explored
  in
    div []
      [ div
          [ onClick GenerateBoard ]
          [ button [ style resetButtonStyle, class (lost ? ("fa fa-frown-o", "fa fa-smile-o"))] [] ]
      , div
          [ style [("position", "absolute"), ("left", px 20), ("top", px 40)] ]
          (List.map (rowView model lost) [0..(boardSize - 1)])
      ]

rowView : Model -> Bool -> Int -> Html Msg
rowView model lost x =
  div
    [ style [("height", px squareSize)] ]
    (List.indexedMap (tileView model lost) (List.repeat boardSize x))
  
tileView : Model -> Bool -> Int -> Int -> Html Msg
tileView model lost x y =
  if (inCoordinateList x y model.explored) then
    if inCoordinateList x y model.bombs then
      exploredBombView True
    else
      exploredTileView (dangerFactor x y model.bombs)
  else
    if inCoordinateList x y model.bombs && lost then
      exploredBombView False
    else
      unexploredTileView lost x y

exploredTileView: Int -> Html Msg
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

exploredBombView: Bool -> Html Msg
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

unexploredTileView: Bool -> Int -> Int -> Html Msg
unexploredTileView lost x y =
  div
    ( [ style
        ( tileStyle
        ++ unexploredStyle
        )
      ]
    ++ if not lost then
        [onClick (Explore x y)]
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
  , ("left", px (9 + squareSize * boardSize // 2))
  ]

px : Int -> String
px x = (toString x) ++ "px"

square : Int -> List (String, String)
square x = [("width", px x), ("height", px x)]
