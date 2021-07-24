port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, header, li, section, text, ul)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Level exposing (Level)



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
    | ViewCredits
    | ResetLevelProgress


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
            ( { model | gameState = Playing level CurrentlyPlaying }, Cmd.none )

        SelectLevel ->
            ( { model | gameState = LevelSelect, colorSwatches = Level.initColorSwatch }, Cmd.none )

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

                    ( cmd, hl ) =
                        if level.levelNumber < model.highestLevel then
                            ( Cmd.none, model.highestLevel )

                        else
                            ( sendMessage <| encodeJSMsg SetHighestLevel (level.levelNumber + 1), level.levelNumber + 1 )

                    newGameState =
                        if level.levelNumber == List.length Level.allLevels then
                            -- allow the highestLevel to increase higher than the levels list
                            -- in case i add more levels later
                            -- and so that it doesn't display a current level in level select that has already been beaten
                            Credits

                        else
                            Playing newLevel CurrentlyPlaying
                in
                ( { model
                    | colorSwatches = Level.initColorSwatch
                    , highestLevel = hl
                    , gameState = newGameState
                  }
                , cmd
                )
                -- if they do update gameoutcome to Win, disable the buttons that choose colors and artboards
                -- will this change any of the above if statements?

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
                , playingView level levelOutcome model.colorSwatches
                ]

        LevelSelect ->
            div []
                [ gameHeader
                , levelSelectView model.highestLevel
                , div []
                    [ button [ onClick ViewCredits ] [ text "Credits" ] ]
                , div []
                    [ button [ onClick ResetLevelProgress ] [ text "Reset Highest Level" ] ]
                ]

        Credits ->
            div []
                [ gameHeader
                , text "created by Rob Bethencourt"
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
        ]
        [ button
            [ onClick <| PlayLevel level
            , if level.levelNumber <= highestLevel then
                disabled False

              else
                disabled True
            ]
            [ text <| String.fromInt level.levelNumber ++ Level.colorToString colorOne ++ Level.colorToString colorTwo ++ Level.colorToString colorThree ]
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
