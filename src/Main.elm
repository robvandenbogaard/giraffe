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
            , velocity = ( 0, 0 )
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

        pull =
            ( computer.mouse.x - memory.x - 50, computer.mouse.y - memory.y )

        ( ( newX, newY ), newVelocity ) =
            moveGiraffe ( memory.x, memory.y ) newAim memory.velocity pull

        movedBalls =
            List.map (moveBall computer.time) memory.balls

        ( eaten, afterEatingBalls ) =
            List.foldl
                (maybeEatBall memory.x memory.y memory.aim)
                ( memory.eaten, [] )
                movedBalls
    in
    { memory
        | aim = newAim
        , x = newX
        , y = newY
        , velocity = newVelocity
        , balls = afterEatingBalls
    }


moveGiraffe ( x, y ) aim ( vx, vy ) ( pullX, pullY ) =
    let
        drag =
            2

        vx_ =
            (vx + pullX / 100) / drag

        vy_ =
            (vy + pullY / 100) / drag

        x_ =
            clamp -1000 1000 (x + vx_)

        y_ =
            clamp -150 200 (y + vy_)
    in
    ( ( x_, y_ ), ( vx_, vy_ ) )


moveBall time b =
    let
        x_ =
            b.x - 2
    in
    { b
        | size = b.size + wave -0.1 0.1 1 time
        , x =
            if x_ > -1000 then
                x_

            else
                1000
        , y = b.y + wave -0.2 0.2 20 time
    }


maybeEatBall x y aim b ( eaten, ballsSoFar ) =
    -- if the ball is behind the giraffe it can't be eaten; in that case we just
    -- return the current "eaten" score and add the ball unchanged to the list
    -- of balls we checked so far
    -- we compare with x + 5, just to the right of the position of the giraffe
    -- because its mouth is on the right side of its head, and to avoid division
    -- by zero in further calculations
    if b.x < x + 5 then
        ( eaten, b :: ballsSoFar )

    else
        let
            distanceToBall =
                sqrt ((b.x - x) ^ 2 + (b.y - y) ^ 2)

            aimToBall =
                45 + (180 / pi) * atan2 (b.y - y) (b.x - x)
        in
        if distanceToBall > 50 || distanceToBall < 20 then
            -- this ball is too far away or to close by to get eaten; just
            -- return the score so far and add the ball unchanged to the
            -- list of balls already checked
            ( eaten, b :: ballsSoFar )

        else if abs (aimToBall - aim) > 10 then
            -- the ball is near but the giraffe is not aiming its mouth in its
            -- direction, so it won't get eaten
            ( eaten, b :: ballsSoFar )

        else
            -- yesss! we can eat the ball; increment the "eaten" score and move
            -- the ball out of sight to the right so it'll reappear as a new one
            ( eaten + 1, { b | x = 1000 } :: ballsSoFar )


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
        , legs yellow 40 4
            |> move 0 (-40 * 8)
        , spots listOfSpots
            |> move -30 -150
        ]


head sunlight nod =
    group
        [ headNeck yellow 40 8
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


legs color size legLength =
    let
        leg =
            rectangle color (size * 2 / 3) (size * legLength)
                |> moveLeft (size / 3)
                |> moveDown (size * legLength / 2)
    in
    group
        [ moveLeft (size * 4 / 3) leg
        , leg
        , circle color size
            |> moveLeft size
            |> moveDown (size / 6)
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
