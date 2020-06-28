module Main exposing (main)

import Playground exposing (..)


main =
    let
        memory =
            { balls =
                [ { color = green, size = 12, x = 100, y = 50 }
                ]
            , eaten = 0
            , spots =
                [ ( 2, 0 )
                , ( 18, 28 )
                , ( 2, 50 )
                , ( 15, 78 )
                , ( 18, 90 )
                ]
            , x = 0
            , y = 0
            , aim = 0
            , sunlight = rgb 250 245 255
            }
    in
    game view update memory


update computer memory =
    let
        -- aim 0 means zero degrees rotation of the face; in this case the eye
        -- and mouth are aiming 45 degrees downwards relative to the horizon
        -- we would like the giraffe to aim in the direction of the mouse cursor
        -- tan aim = (c.y - y) / (c.x - x)
        -- atan (tan aim) = aim in radians minus the 45 degree offset
        -- atan2 is to help sort out the various negative value cases
        newAim =
            45
                + (180 / pi)
                * atan2 (computer.mouse.y - memory.y) (abs (computer.mouse.x - memory.x))
    in
    { memory | aim = newAim }


view computer memory =
    [ background memory.sunlight
    , giraffe memory.sunlight memory.spots memory.aim
        |> move memory.x memory.y
    , balls memory.balls
    ]


background sunlight =
    group
        [ rectangle sunlight 2000 2000
        , rectangle lightBrown 2000 400
            |> moveDown 400
        ]


giraffe sunlight listOfSpots nod =
    group
        [ head sunlight nod
        , spots listOfSpots
            |> move -30 -150
        ]


head sunlight nod =
    group
        [ headNeck yellow 40 10
        , face sunlight
            |> rotate nod
        ]


face sunlight =
    group
        [ circle black 15
            |> move 10 10
        , circle sunlight 10
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


balls =
    group << List.map ball


ball { color, size, x, y } =
    move x y (circle color size)
