module Level exposing
    ( Color
    , Level
    , allLevels
    , colorToString
    , initColorSwatch
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
    , brushColor : Color
    }


allLevels : List Level
allLevels =
    -- i should rethink if i need to keep the current color and artboards in the level data as thi may be better suited elsewhere as they are not unique to each level
    [ Level 1 ( Red, Yellow, Blue ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 2 ( Red, Yellow, Green ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 3 ( Blue, Blue, Yellow ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 4 ( Green, Green, Yellow ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 5 ( Orange, Yellow, Blue ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 6 ( Purple, Purple, Yellow ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 7 ( Orange, Purple, Yellow ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 8 ( Purple, Purple, Green ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 9 ( Orange, Purple, Purple ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 10 ( RedOrange, Blue, Green ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 11 ( Red, YellowOrange, Purple ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 12 ( BlueGreen, Green, Orange ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 13 ( RedOrange, Orange, Green ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 14 ( BlueGreen, RedOrange, Green ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 15 ( RedOrange, YellowGreen, Green ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 16 ( Purple, Green, BluePurple ) ( NoColor, NoColor, NoColor ) NoColor
    , Level 17 ( YellowOrange, RedPurple, Purple ) ( NoColor, NoColor, NoColor ) NoColor
    ]


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


initColorSwatch : ( Color, Color, Color )
initColorSwatch =
    ( Red, Yellow, Blue )


updateColorSwatch : ( Color, Color, Color ) -> Color -> ( Color, Color, Color )
updateColorSwatch currentColorSwatches color =
    let
        ( redSwatch, yellowSwatch, blueSwatch ) =
            currentColorSwatches
    in
    case color of
        Red ->
            ( NoColor, yellowSwatch, blueSwatch )

        Yellow ->
            ( redSwatch, NoColor, blueSwatch )

        Blue ->
            ( redSwatch, yellowSwatch, NoColor )

        _ ->
            currentColorSwatches



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


colorToString : Color -> String
colorToString color =
    case color of
        NoColor ->
            "white"

        Red ->
            "red"

        Yellow ->
            "yellow"

        Blue ->
            "blue"

        Orange ->
            "orange"

        Green ->
            "green"

        Purple ->
            "purple"

        RedOrange ->
            "red-orange"

        YellowOrange ->
            "yellow-orange"

        YellowGreen ->
            "yellow-green"

        BlueGreen ->
            "blue-green"

        BluePurple ->
            "blue-purple"

        RedPurple ->
            "red-purple"

        Brown ->
            "brown"


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


updateBrushColor : Level -> Color -> Level
updateBrushColor level newColor =
    { level | brushColor = newColor }


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
