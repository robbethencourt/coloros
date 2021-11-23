module Artbrush exposing (artbrush)

import Svg exposing (Svg, svg)
import Svg.Attributes exposing (class, d, strokeLinejoin, strokeWidth, viewBox)


artbrush : String -> Svg msg
artbrush color =
    svg [ viewBox "30 0 500 250", class <| "artbrush svg-" ++ color ]
        [ Svg.path
            [ d "M373.88 112.89A97.5 97.5 0 0 0 179 108c-.5 41.49-49.56 115.22-152.88 75.55 0 0 57 47 139.26 33.33 0 0-7.26 12.67-62.26 15.67 0 0 133.63 14.13 210.38-29.06a98.28 98.28 0 0 0 60.38-90.6Z"
            , strokeLinejoin "round"
            , strokeWidth "3"
            ]
            []
        ]
