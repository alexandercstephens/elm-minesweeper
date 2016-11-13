import Html exposing (Html, button, div, text, span)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random


main =
  App.program 
    { init = init
    , view = view
    , update = update 
    , subscriptions = subscriptions
    }


-- MODEL

boardSize : Int
boardSize = 10

numBombs : Int
numBombs = 20

squareSize : Int
squareSize = 20

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
  case (List.filter (\{x, y} -> x==x' && y==y') coordinateList) of
    [] -> False
    otherwise -> True

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
  div [ style [("position", "absolute"), ("left", px 20), ("top", px 10)] ] (List.map (bombRowView model) [0..(boardSize - 1)])
  
bombRowView : Model -> Int -> Html Msg
bombRowView model x =
  div [ style [("height", px squareSize), ("margin", (px 2) ++ " 0")] ] (List.indexedMap (bombView model) (List.repeat boardSize x))
  
bombView : Model -> Int -> Int -> Html Msg
bombView model x y = 
  if (inCoordinateList x y model.explored) then 
    div 
    [ style (List.concat 
      [ bombStyle
      , explored
      ])
    , onClick (Explore x y)] 
    [ if (inCoordinateList x y model.bombs) then 
        text "*" 
      else 
        let
          df = dangerFactor x y model.bombs
        in
          if df == 0 then
            text ""
          else
            text (toString (df))
    ] 
  else 
    div 
    [ style (List.concat 
      [ bombStyle
      , unexplored
      ])
    , onClick (Explore x y)] 
    [ ]
  
    
bombStyle : List (String, String)
bombStyle = 
  List.concat
    [ [ ("display", "inline-block")
      , ("border", "1px solid black")
      , ("background-color", "gray")
      , ("vertical-align", "top")
      , ("text-align", "center")
      ]
    , square squareSize
    ]

unexplored : List (String, String)
unexplored = 
  [ ("background-color", "gray")
  , ("box-shadow", "inset -1px -1px #666")
  ]
explored : List (String, String)
explored = 
  [ ("background-color", "white")
  , ("box-shadow", "inset 1px 1px #ccc")
  ]

bomb : List (String, String)
bomb =
  [ ("background-color", "blue")
  ]

px : Int -> String
px x = (toString x) ++ "px"

square : Int -> List (String, String)
square x = [("width", px x), ("height", px x)]
