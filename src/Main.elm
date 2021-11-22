port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, br, button, div, h1, header, img, li, section, span, text, ul)
import Html.Attributes exposing (class, disabled, src)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Level
import Logo
import Process
import Task



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
    , highestLevel : Int
    }


type State
    = Loading
    | Playing Level.Level Level.LevelOutcome
    | LevelSelect
    | Credits


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = Loading
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
    | ViewCredits
    | ResetLevelProgress
    | ResetLevelOutcome Level.Level Level.LevelOutcome


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
            ( { model
                | highestLevel = highestLevel
                , gameState = LevelSelect
              }
            , Cmd.none
            )

        PlayLevel level ->
            ( { model | gameState = Playing level Level.CurrentlyPlaying }, Cmd.none )

        SelectLevel ->
            ( { model | gameState = LevelSelect }, Cmd.none )

        SelectColorSwatch color level ->
            ( { model
                | gameState =
                    Playing
                        (level
                            |> Level.updateBrushColor color
                            |> Level.updateColorSwatch color
                        )
                        Level.CurrentlyPlaying
              }
            , Cmd.none
            )

        MixColors artboardNumber artboardColor level ->
            let
                newColor =
                    Level.mixColors artboardColor level.brushColor

                updatedLevel =
                    level
                        |> Level.updateBrushColor newColor
                        |> Level.updateArtboards newColor artboardNumber
            in
            case Level.checkLevelOutcome updatedLevel.colorsToMatch updatedLevel.artboards of
                Level.CurrentlyPlaying ->
                    ( { model | gameState = Playing updatedLevel Level.CurrentlyPlaying }, Cmd.none )

                Level.Win ->
                    ( { model
                        | gameState = Playing updatedLevel Level.Win
                      }
                    , transitionLevelOutcome (ResetLevelOutcome updatedLevel Level.Win)
                    )

                Level.Loss ->
                    ( { model | gameState = Playing updatedLevel Level.Loss }
                    , transitionLevelOutcome (ResetLevelOutcome updatedLevel Level.Loss)
                    )

        ResetLevelOutcome currentLevel levelOutcome ->
            case levelOutcome of
                Level.Loss ->
                    let
                        resetLevel =
                            Level.allLevels
                                |> List.filter (\a -> a.levelNumber == currentLevel.levelNumber)
                                |> List.head
                                |> Maybe.withDefault Level.defaultLevel
                    in
                    ( { model
                        | gameState = Playing resetLevel Level.CurrentlyPlaying
                      }
                    , Cmd.none
                    )

                Level.Win ->
                    let
                        newLevel =
                            Level.allLevels
                                |> List.filter (\a -> a.levelNumber == currentLevel.levelNumber + 1)
                                |> List.head
                                |> Maybe.withDefault Level.defaultLevel

                        ( cmd, hl ) =
                            if currentLevel.levelNumber < model.highestLevel then
                                ( Cmd.none, model.highestLevel )

                            else
                                ( sendMessage <| encodeJSMsg SetHighestLevel (currentLevel.levelNumber + 1)
                                , currentLevel.levelNumber + 1
                                )

                        newGameState =
                            if currentLevel.levelNumber == List.length Level.allLevels then
                                -- allow the highestLevel to increase higher than the levels list in case i add more levels later
                                -- and so that it doesn't display a current level in level select that has already been beaten
                                Credits

                            else
                                Playing newLevel Level.CurrentlyPlaying
                    in
                    ( { model
                        | highestLevel = hl
                        , gameState = newGameState
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ResetLevel levelNumber ->
            let
                resetLevel =
                    Level.allLevels
                        |> List.filter (\a -> a.levelNumber == levelNumber)
                        |> List.head
                        |> Maybe.withDefault Level.defaultLevel
            in
            ( { model
                | gameState = Playing resetLevel Level.CurrentlyPlaying
              }
            , Cmd.none
            )

        ViewCredits ->
            ( { model | gameState = Credits }, Cmd.none )

        ResetLevelProgress ->
            ( { model | highestLevel = 1 }, sendMessage <| encodeJSMsg ResetHighestLevel 1 )



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
                , playingView level levelOutcome
                ]

        LevelSelect ->
            div []
                [ gameHeader
                , levelSelectView model.highestLevel
                , div [ class "main-screen-button-wrapper" ]
                    [ button [ onClick ViewCredits, class "credits-button" ] [ text "Credits" ]
                    , button [ onClick ResetLevelProgress, class "reset-button" ] [ text "Reset Game to Level 1" ]
                    ]
                ]

        Credits ->
            div []
                [ gameHeader
                , h1 [ class "credits" ]
                    [ span [ class "created-by" ] [ text "created by" ]
                    , br [] []
                    , span [ class "my-name" ] [ text "Rob Bethencourt" ]
                    ]
                ]


gameHeader : Html Msg
gameHeader =
    header [ onClick SelectLevel ]
        [ Logo.logo ]


playingView : Level.Level -> Level.LevelOutcome -> Html Msg
playingView level levelOutcome =
    let
        ( colorOneToMatch, colorTwoToMatch, colorThreeToMatch ) =
            level.colorsToMatch

        ( artboardOne, artboardTwo, artboardThree ) =
            level.artboards

        ( redSwatch, yellowSwatch, blueSwatch ) =
            level.colorSwatches

        isDisabled =
            case levelOutcome of
                Level.CurrentlyPlaying ->
                    False

                _ ->
                    True
    in
    div []
        [ section [ class "artboards" ]
            [ div [ class "color-one-set" ]
                [ div [ class <| "color-one " ++ Level.colorToString colorOneToMatch ] []
                , div [ class "color-one-to-match" ]
                    [ button
                        [ class <| Level.colorToString artboardOne
                        , onClick <| MixColors 1 artboardOne level
                        , disabled isDisabled
                        ]
                        []
                    ]
                ]
            , div [ class "color-two-set" ]
                [ div [ class <| "color-two " ++ Level.colorToString colorTwoToMatch ] []
                , div [ class "color-two-to-match" ]
                    [ button
                        [ class <| Level.colorToString artboardTwo
                        , onClick <| MixColors 2 artboardTwo level
                        , disabled isDisabled
                        ]
                        []
                    ]
                ]
            , div [ class "color-three-set" ]
                [ div [ class <| "color-three " ++ Level.colorToString colorThreeToMatch ] []
                , div [ class "color-three-to-match" ]
                    [ button
                        [ class <| Level.colorToString artboardThree
                        , onClick <| MixColors 3 artboardThree level
                        , disabled isDisabled
                        ]
                        []
                    ]
                ]
            ]
        , section [ class "colors-swatches" ]
            [ ul []
                [ li []
                    [ button
                        [ class <| Level.colorToString redSwatch
                        , onClick <| SelectColorSwatch redSwatch level
                        , disabled isDisabled
                        ]
                        []
                    ]
                , li []
                    [ button
                        [ class <| Level.colorToString yellowSwatch
                        , onClick <| SelectColorSwatch yellowSwatch level
                        , disabled isDisabled
                        ]
                        []
                    ]
                , li []
                    [ button
                        [ class <| Level.colorToString blueSwatch
                        , onClick <| SelectColorSwatch blueSwatch level
                        , disabled isDisabled
                        ]
                        []
                    ]
                ]
            ]
        , section [ class "current-color" ]
            [ div [ class <| "brush-color " ++ Level.colorToString level.brushColor ] []
            , div [ class "reset-wrapper" ] [ button [ onClick <| ResetLevel level.levelNumber, class "red" ] [ text "reset" ] ]
            ]
        ]


levelSelectView : Int -> Html Msg
levelSelectView highestLevel =
    ul [ class "level-select-wrapper" ] (levelsToSelect Level.allLevels highestLevel)


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
        ]
        [ button
            [ onClick <| PlayLevel level
            , if level.levelNumber <= highestLevel then
                disabled False

              else
                disabled True
            ]
            [ div [ class "level-select-color-wrapper" ]
                [ div [ class <| "level-select-color " ++ Level.colorToString colorOne ] []
                , div [ class <| "level-select-color " ++ Level.colorToString colorTwo ] []
                , div [ class <| "level-select-color " ++ Level.colorToString colorThree ] []
                ]
            ]
        ]



-- Ports


port sendMessage : Encode.Value -> Cmd msg


port messageReceiver : (Int -> msg) -> Sub msg


type JSMsg
    = GetHighestLevel
    | SetHighestLevel
    | ResetHighestLevel


jsmToString : JSMsg -> String
jsmToString jsm =
    case jsm of
        GetHighestLevel ->
            "getHighestLevel"

        SetHighestLevel ->
            "setHighestLevel"

        ResetHighestLevel ->
            "resetHighestLevel"


encodeJSMsg : JSMsg -> Int -> Encode.Value
encodeJSMsg jsm newLevel =
    Encode.object
        [ ( "jsMsg", Encode.string <| jsmToString jsm )
        , ( "value", Encode.int newLevel )
        ]



-- tasks


transitionLevelOutcome : Msg -> Cmd Msg
transitionLevelOutcome msg =
    Process.sleep 2000
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
