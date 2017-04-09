module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (Position)
import Collage exposing (..)
import Element exposing (toHtml)
import Color exposing (..)
import Number.Bounded as Bounded exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { down : Bool
    , paths : List (List { x : Bounded Int, y : Bounded Int })
    }


init : ( Model, Cmd Msg )
init =
    ( Model False [], Cmd.none )


boundingBox : ( Float, Float, Float, Float )
boundingBox =
    ( 200, 200, 400, 300 )


boundedX : Bounded Int
boundedX =
    let
        ( x, y, w, h ) =
            boundingBox
    in
        between (round x) (round x + round w)


boundedY : Bounded Int
boundedY =
    let
        ( x, y, w, h ) =
            boundingBox
    in
        between (round y) (round y + round h)



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({ paths } as model) =
    case msg of
        DragStart { x, y } ->
            Model True <| [ { x = set x boundedX, y = set y boundedY } ] :: paths

        DragAt { x, y } ->
            Model True <|
                case paths of
                    h :: t ->
                        (h ++ [ { x = set x boundedX, y = set y boundedY } ]) :: t

                    _ ->
                        [ [ { x = set x boundedX, y = set y boundedX } ] ]

        DragEnd _ ->
            Model False paths



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.down of
        False ->
            Mouse.downs DragStart

        True ->
            Sub.batch
                [ Mouse.moves DragAt
                , Mouse.ups DragEnd
                ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        toPaths positions =
            path
                (List.map
                    (\{ x, y } ->
                        ( toFloat <| Bounded.value x
                        , toFloat <| negate <| Bounded.value y
                        )
                    )
                    positions
                )
                |> traced
                    { defaultLine
                        | width = 10
                        , color = blue
                        , cap = Round
                        , join = Smooth
                    }
                |> move ( -400, 400 )

        renderBoundingBox =
            let
                ( x, y, w, h ) =
                    boundingBox
            in
                rect w h
                    |> outlined defaultLine
                    |> move ( -400 + w / 2 + x, 400 - h / 2 - y )
    in
        div
            [ style
                [ ( "cursor", "pointer" )
                , ( "border", "2px solid black" )
                , ( "width", "800px" )
                , ( "height", "800px" )
                ]
            ]
            [ collage
                800
                800
                (renderBoundingBox :: List.map toPaths model.paths)
                |> toHtml
            ]
