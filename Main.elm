module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (Position)
import Collage exposing (..)
import Element exposing (toHtml)
import Color exposing (..)


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
    , paths : List (List Position)
    }


init : ( Model, Cmd Msg )
init =
    ( Model False [], Cmd.none )



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
        DragStart xy ->
            Model True <| [ xy ] :: paths

        DragAt xy ->
            Model True <|
                case paths of
                    h :: t ->
                        (h ++ [ xy ]) :: t

                    _ ->
                        [ [ xy ] ]

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
                        ( toFloat <| x
                        , toFloat <| -y
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
                (List.map toPaths model.paths)
                |> toHtml
            ]
