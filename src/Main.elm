module Main exposing (main)

import Playground exposing (..)


main =
    animation view


view time =
    [ giraffe (wave 10 70 3 time)
        |> moveRight (wave 0 100 10 time)
        |> moveUp (wave -50 150 4 time)
    ]


giraffe nod =
    group
        [ head nod
        , spots
            [ ( 2, 0 )
            , ( 18, 28 )
            , ( 2, 50 )
            , ( 15, 78 )
            , ( 18, 90 )
            ]
            |> move -30 -150
        ]


head nod =
    group
        [ headNeck yellow 40 10
        , face
            |> rotate nod
        ]


face =
    group
        [ circle black 15
            |> move 10 10
        , circle white 10
            |> move 25 -25
        , headNeck orange 10 3
            |> moveUp 60
            |> moveLeft 10
        ]


headNeck color size neckLength =
    group
        [ circle color size
        , rectangle color size (size * neckLength)
            |> moveDown (size * (neckLength / 2))
            |> moveLeft (size / 2)
        ]


spot =
    circle brown 12


spots range =
    group <|
        List.map
            (\( x, y ) ->
                move x y spot
            )
            range
