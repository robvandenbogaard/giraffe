module Main exposing (gezicht, giraf, kop, kopnek, main, view, vlek, vlekken)

import Playground exposing (..)


main =
    animation view


view time =
    [ giraf (wave 10 70 3 time)
        |> moveRight (wave 0 100 10 time)
        |> moveUp (wave -50 150 4 time)
    ]


giraf kopknik =
    group
        [ kop kopknik
        , vlekken
            [ ( 2, 0 )
            , ( 18, 28 )
            , ( 2, 50 )
            , ( 15, 78 )
            , ( 18, 90 )
            ]
            |> move -30 -150
        ]


kop kopknik =
    group
        [ kopnek yellow 40 10
        , gezicht
            |> rotate kopknik
        ]


gezicht =
    group
        [ circle black 15
            |> move 10 10
        , circle white 10
            |> move 25 -25
        , kopnek orange 10 3
            |> moveUp 60
            |> moveLeft 10
        ]


kopnek kleur grootte neklengte =
    group
        [ circle kleur grootte
        , rectangle kleur grootte (grootte * neklengte)
            |> moveDown (grootte * (neklengte / 2))
            |> moveLeft (grootte / 2)
        ]


vlek =
    circle brown 12


vlekken range =
    group <|
        List.map
            (\( x, y ) ->
                move x y vlek
            )
            range
