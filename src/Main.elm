port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, header, li, section, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Level



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { gameState : State
    , colorSwatches : ( Level.Color, Level.Color, Level.Color )
    , highestLevel : Int
    }


type State
    = Loading
    | Playing Level.Level LevelOutcome
    | LevelSelect
    | Credits


type LevelOutcome
    = CurrentlyPlaying
    | Win
    | Loss


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = Loading
      , colorSwatches = Level.initColorSwatch
      , highestLevel = 1
      }
    , sendMessage <| encodeJSMsg GetHighestLevel 0
    )



-- UPDATE


type Msg
    = NoOp
    | SendToJs JSMsg Int
    | RecvFromJs Int
    | PlayLevel Level.Level
    | SelectLevel
    | SelectColorSwatch Level.Color Level.Level
    | MixColors Int Level.Color Level.Level
    | ResetLevel Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        SendToJs jsMsg newLevel ->
            ( model, sendMessage <| encodeJSMsg jsMsg newLevel )

        RecvFromJs highestLevel ->
            ( { model | highestLevel = highestLevel, gameState = LevelSelect }, Cmd.none )

        PlayLevel level ->
            ( { model | gameState = Playing level CurrentlyPlaying }, Cmd.none )

        SelectLevel ->
            ( { model | gameState = LevelSelect }, Cmd.none )

        SelectColorSwatch color level ->
            ( { model
                | gameState =
                    Playing (Level.updateBrushColor level color) CurrentlyPlaying
                , colorSwatches =
                    Level.updateColorSwatch model.colorSwatches color
              }
            , Cmd.none
            )

        MixColors artboardNumber artboardColor level ->
            let
                newColor =
                    Level.mixColors artboardColor level.brushColor

                updatedLevel =
                    newColor
                        |> Level.updateBrushColor level
                        |> Level.updateArtboards newColor artboardNumber
            in
            if updatedLevel.colorsToMatch == updatedLevel.artboards then
                let
                    newLevel =
                        Level.allLevels
                            |> List.filter (\a -> a.levelNumber == level.levelNumber + 1)
                            |> List.head
                            |> Maybe.withDefault Level.defaultLevel
                in
                ( { model
                    | colorSwatches = Level.initColorSwatch
                    , gameState = Playing newLevel CurrentlyPlaying
                  }
                , Cmd.none
                )
                -- if they do update gameoutcome to Win, disable the buttons
                -- update highest level in model and localstorage

            else
                ( { model | gameState = Playing updatedLevel CurrentlyPlaying }, Cmd.none )

        ResetLevel levelNumber ->
            let
                resetLevel =
                    Level.allLevels
                        |> List.filter (\a -> a.levelNumber == levelNumber)
                        |> List.head
                        |> Maybe.withDefault Level.defaultLevel
            in
            ( { model
                | colorSwatches = Level.initColorSwatch
                , gameState = Playing resetLevel CurrentlyPlaying
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver RecvFromJs



-- VIEW


view : Model -> Html Msg
view model =
    case model.gameState of
        Loading ->
            div [] [ text "loading" ]

        Playing level levelOutcome ->
            div []
                [ gameHeader
                , playingView level levelOutcome model.colorSwatches
                ]

        LevelSelect ->
            div []
                [ gameHeader
                , levelSelectView model.highestLevel
                ]

        Credits ->
            div []
                [ gameHeader
                , text "credits"
                ]


gameHeader : Html Msg
gameHeader =
    header [ onClick SelectLevel ]
        [ text "Coloros" ]


playingView : Level.Level -> LevelOutcome -> ( Level.Color, Level.Color, Level.Color ) -> Html Msg
playingView level levelOutcome colorSwatches =
    let
        ( colorOneToMatch, colorTwoToMatch, colorThreeToMatch ) =
            level.colorsToMatch

        ( artboardOne, artboardTwo, artboardThree ) =
            level.artboards

        ( redSwatch, yellowSwatch, blueSwatch ) =
            colorSwatches
    in
    div []
        [ section [ class "artboards" ]
            [ div [ class "color-one-set" ]
                [ div [ class "color-one" ] [ text <| Level.colorToString colorOneToMatch ]
                , div [ class "color-one-to-match" ]
                    [ button
                        [ class <| Level.colorToString artboardOne
                        , onClick <| MixColors 1 artboardOne level
                        ]
                        [ text <| Level.colorToString artboardOne ]
                    ]
                ]
            , div [ class "color-two-set" ]
                [ div [ class "color-two" ] [ text <| Level.colorToString colorTwoToMatch ]
                , div [ class "color-two-to-match" ]
                    [ button
                        [ class <| Level.colorToString artboardTwo
                        , onClick <| MixColors 2 artboardTwo level
                        ]
                        [ text <| Level.colorToString artboardTwo ]
                    ]
                ]
            , div [ class "color-three-set" ]
                [ div [ class "color-three" ] [ text <| Level.colorToString colorThreeToMatch ]
                , div [ class "color-three-to-match" ]
                    [ button
                        [ class <| Level.colorToString artboardThree
                        , onClick <| MixColors 3 artboardThree level
                        ]
                        [ text <| Level.colorToString artboardThree ]
                    ]
                ]
            ]
        , section [ class "colors-swatches" ]
            [ ul []
                [ li []
                    [ button
                        [ class <| Level.colorToString redSwatch
                        , onClick <| SelectColorSwatch redSwatch level
                        ]
                        [ text <| Level.colorToString redSwatch ]
                    ]
                , li []
                    [ button
                        [ class <| Level.colorToString yellowSwatch
                        , onClick <| SelectColorSwatch yellowSwatch level
                        ]
                        [ text <| Level.colorToString yellowSwatch ]
                    ]
                , li []
                    [ button
                        [ class <| Level.colorToString blueSwatch
                        , onClick <| SelectColorSwatch blueSwatch level
                        ]
                        [ text <| Level.colorToString blueSwatch ]
                    ]
                ]
            ]
        , section [ class "current-color" ]
            [ div [] [ text <| Level.colorToString level.brushColor ]
            , div [] [ button [ onClick <| ResetLevel level.levelNumber ] [ text "reset" ] ]
            ]
        ]


levelSelectView : Int -> Html Msg
levelSelectView highestLevel =
    ul [] (levelsToSelect Level.allLevels highestLevel)


levelsToSelect : List Level.Level -> Int -> List (Html Msg)
levelsToSelect levels highestLevel =
    List.map (levelSelectItem highestLevel) levels


levelSelectItem : Int -> Level.Level -> Html Msg
levelSelectItem highestLevel level =
    let
        ( colorOne, colorTwo, colorThree ) =
            level.colorsToMatch
    in
    li
        [ if level.levelNumber == highestLevel then
            class "currentLevel"

          else
            class ""
        , onClick <| PlayLevel level
        ]
        [ text <| String.fromInt level.levelNumber ++ Level.colorToString colorOne ++ Level.colorToString colorTwo ++ Level.colorToString colorThree ]



-- Ports


port sendMessage : Encode.Value -> Cmd msg


port messageReceiver : (Int -> msg) -> Sub msg


type JSMsg
    = GetHighestLevel
    | SaveHighestLevel
    | ResetHighestLevel


jsmToString : JSMsg -> String
jsmToString jsm =
    case jsm of
        GetHighestLevel ->
            "getHighestLevel"

        SaveHighestLevel ->
            "setHighestLevel"

        ResetHighestLevel ->
            "resetHighestLevel"


encodeJSMsg : JSMsg -> Int -> Encode.Value
encodeJSMsg jsm newLevel =
    Encode.object
        [ ( "jsMsg", Encode.string <| jsmToString jsm )
        , ( "valaue", Encode.int newLevel )
        ]
