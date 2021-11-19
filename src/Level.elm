module Level exposing
    ( Color
    , Level
    , LevelOutcome(..)
    , allLevels
    , checkLevelOutcome
    , colorToHex
    , defaultLevel
    , initColorSwatches
    , mixColors
    , stringToColor
    , updateArtboards
    , updateBrushColor
    , updateColorSwatch
    )


type alias Level =
    { levelNumber : Int
    , colorsToMatch : ( Color, Color, Color )
    , artboards : ( Color, Color, Color )
    , colorSwatches : ( Color, Color, Color )
    , brushColor : Color
    }


type LevelOutcome
    = CurrentlyPlaying
    | Win
    | Loss


allLevels : List Level
allLevels =
    [ Level 1 ( Red, Yellow, Blue ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 2 ( Red, Yellow, Green ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 3 ( Red, Orange, Blue ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 4 ( Purple, Yellow, Blue ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 5 ( Blue, Blue, Yellow ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 6 ( Green, Green, Yellow ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 7 ( Orange, Yellow, Purple ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 8 ( Purple, Purple, Yellow ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 9 ( Purple, Purple, Green ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 10 ( Orange, Purple, Purple ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 11 ( RedOrange, Orange, Purple ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 12 ( BlueGreen, Green, Orange ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 13 ( YellowOrange, Orange, Purple ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 14 ( Orange, YellowGreen, Green ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 15 ( Purple, Green, BluePurple ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    , Level 16 ( Orange, RedPurple, Purple ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor
    ]


defaultLevel : Level
defaultLevel =
    Level 1 ( Red, Yellow, Blue ) ( NoColor, NoColor, NoColor ) initColorSwatches NoColor


type Color
    = NoColor
    | Red
    | Yellow
    | Blue
    | Orange
    | Green
    | Purple
    | RedOrange
    | YellowOrange
    | YellowGreen
    | BlueGreen
    | BluePurple
    | RedPurple
    | Brown



-- color swatch


initColorSwatches : ( Color, Color, Color )
initColorSwatches =
    ( Red, Yellow, Blue )



-- mixing colors


mixColors : Color -> Color -> Color
mixColors artboardColor brushColor =
    case artboardColor of
        NoColor ->
            brushColor

        Red ->
            mixWithRed brushColor

        Yellow ->
            mixWithYellow brushColor

        Blue ->
            mixWithBlue brushColor

        Orange ->
            mixWithOrange brushColor

        Green ->
            mixWithGreen brushColor

        Purple ->
            mixWithPurple brushColor

        _ ->
            Brown


mixWithRed : Color -> Color
mixWithRed brushColor =
    case brushColor of
        Red ->
            Red

        Yellow ->
            Orange

        Blue ->
            Purple

        Orange ->
            RedOrange

        Purple ->
            RedPurple

        _ ->
            Brown


mixWithYellow : Color -> Color
mixWithYellow brushColor =
    case brushColor of
        Red ->
            Orange

        Yellow ->
            Yellow

        Blue ->
            Green

        Orange ->
            YellowOrange

        Green ->
            YellowGreen

        _ ->
            Brown


mixWithBlue : Color -> Color
mixWithBlue brushColor =
    case brushColor of
        Red ->
            Purple

        Yellow ->
            Green

        Blue ->
            Blue

        Green ->
            BlueGreen

        Purple ->
            BluePurple

        _ ->
            Brown


mixWithOrange : Color -> Color
mixWithOrange brushColor =
    case brushColor of
        Orange ->
            Orange

        Red ->
            RedOrange

        Yellow ->
            YellowOrange

        _ ->
            Brown


mixWithGreen : Color -> Color
mixWithGreen brushColor =
    case brushColor of
        Green ->
            Green

        Yellow ->
            YellowGreen

        Blue ->
            BlueGreen

        _ ->
            Brown


mixWithPurple : Color -> Color
mixWithPurple brushColor =
    case brushColor of
        Purple ->
            Purple

        Blue ->
            BluePurple

        Red ->
            RedPurple

        _ ->
            Brown



-- helper functions


colorToHex : Color -> String
colorToHex color =
    case color of
        NoColor ->
            "#F1F2F9"

        Red ->
            "#FF1400"

        Yellow ->
            "#FFEA00"

        Blue ->
            "#0055FF"

        Orange ->
            "#FF5F0F"

        Green ->
            "#00BF33"

        Purple ->
            "#8600CF"

        RedOrange ->
            "#FF4203"

        YellowOrange ->
            "#FFAD29"

        YellowGreen ->
            "#95EB00"

        BlueGreen ->
            "#00EDD2"

        BluePurple ->
            "#4D17D4"

        RedPurple ->
            "#D41782"

        Brown ->
            "#4F240F"


stringToColor : String -> Color
stringToColor str =
    case str of
        "red" ->
            Red

        "yellow" ->
            Yellow

        "blue" ->
            Blue

        "orange" ->
            Orange

        "green" ->
            Green

        "purple" ->
            Purple

        "red-orange" ->
            RedOrange

        "yellow-orange" ->
            YellowOrange

        "yellow-green" ->
            YellowGreen

        "blue-green" ->
            BlueGreen

        "blue-purple" ->
            BluePurple

        "red-purple" ->
            RedPurple

        "brown" ->
            Brown

        _ ->
            NoColor



-- updating Level record


updateBrushColor : Color -> Level -> Level
updateBrushColor newColor level =
    { level | brushColor = newColor }


updateColorSwatch : Color -> Level -> Level
updateColorSwatch color level =
    let
        ( redSwatch, yellowSwatch, blueSwatch ) =
            level.colorSwatches
    in
    case color of
        Red ->
            { level | colorSwatches = ( NoColor, yellowSwatch, blueSwatch ) }

        Yellow ->
            { level | colorSwatches = ( redSwatch, NoColor, blueSwatch ) }

        Blue ->
            { level | colorSwatches = ( redSwatch, yellowSwatch, NoColor ) }

        _ ->
            level


updateArtboards : Color -> Int -> Level -> Level
updateArtboards newColor artboardNumber level =
    let
        ( artboardOne, artboardTwo, artboardThree ) =
            level.artboards
    in
    case artboardNumber of
        1 ->
            { level | artboards = ( newColor, artboardTwo, artboardThree ) }

        2 ->
            { level | artboards = ( artboardOne, newColor, artboardThree ) }

        _ ->
            { level | artboards = ( artboardOne, artboardTwo, newColor ) }


checkLevelOutcome : ( Color, Color, Color ) -> ( Color, Color, Color ) -> LevelOutcome
checkLevelOutcome colorsToMatch artboards =
    if colorsToMatch == artboards then
        Win

    else
        let
            ( csw1, csw2, csw3 ) =
                artboards
        in
        if csw1 == Brown || csw2 == Brown || csw3 == Brown then
            Loss

        else
            CurrentlyPlaying
