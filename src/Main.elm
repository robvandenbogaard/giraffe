module Main exposing (main)

import Playground exposing (..)


type Health
    = NeedVitamins Int
    | Dead


main =
    let
        memory =
            { balls =
                [ { color = green, size = 12, x = 100, y = 50 }
                , { color = blue, size = 10, x = 900, y = -50 }
                ]
            , spots =
                []
            , x = 0
            , y = 0
            , aim = 0
            , velocity = ( 0, 0 )
            , sunlight = rgb 250 245 255
            , health = NeedVitamins 0
            }
    in
    game view updateWhenHealthy memory


updateWhenHealthy computer memory =
    case memory.health of
        NeedVitamins _ ->
            update computer memory

        Dead ->
            memory


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

        ( ballsEaten, ballsAfterEating ) =
            List.foldl
                (maybeEatBall memory.x memory.y memory.aim)
                ( 0, [] )
                movedBalls

        spotsAfterEating =
            if ballsEaten > 0 then
                let
                    random1 =
                        cos (spin 1 computer.time)

                    random2 =
                        sin (spin 1 computer.time)

                    newSpots =
                        ( random1, random2 ) :: List.reverse memory.spots
                in
                List.reverse newSpots

            else
                memory.spots

        newHealth =
            case ( memory.health, ballsEaten ) of
                ( NeedVitamins amount, 0 ) ->
                    if amount < 1000 then
                        NeedVitamins (amount + 1)

                    else
                        Dead

                ( NeedVitamins _, _ ) ->
                    NeedVitamins 0

                ( Dead, _ ) ->
                    Dead
    in
    { memory
        | aim = newAim
        , x = newX
        , y = newY
        , velocity = newVelocity
        , balls = ballsAfterEating
        , spots = spotsAfterEating
        , health = newHealth
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
    -- return the amount eaten so far and add the ball unchanged to the list
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
            -- return the number of eaten balls so far and add the ball
            -- unchanged to the list of balls already checked
            ( eaten, b :: ballsSoFar )

        else if abs (aimToBall - aim) > 10 then
            -- the ball is near but the giraffe is not aiming its mouth in its
            -- direction, so it won't get eaten
            ( eaten, b :: ballsSoFar )

        else
            -- yesss! we can eat the ball; increase the "eaten" score and move
            -- the ball out of sight to the right so it'll reappear as a new one
            ( eaten + 1, { b | x = 1000 } :: ballsSoFar )


view computer memory =
    [ background memory.sunlight
    , giraffe memory.sunlight memory.health memory.spots memory.aim
        |> move memory.x memory.y
    , balls memory.balls
    ]


background sunlight =
    group
        [ rectangle sunlight 2000 2000
        , rectangle lightBrown 2000 400
            |> moveDown 400
        ]


giraffe sunlight health listOfSpots nod =
    let
        color =
            case health of
                NeedVitamins amount ->
                    if amount < 500 then
                        yellow

                    else
                        lightYellow

                Dead ->
                    gray
    in
    group
        [ head sunlight color nod
        , legs color 40 4
            |> move 0 (-40 * 8)
        , spots health listOfSpots
            |> move -20 -60
        ]


head sunlight color nod =
    group
        [ headNeck color 40 8
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


spot health i ( x, y ) =
    let
        color =
            case health of
                NeedVitamins amount ->
                    if amount < 500 then
                        brown

                    else
                        lightBrown

                Dead ->
                    darkGray
    in
    circle color 12
        |> move (x * 8) -(toFloat i * 25 + y * 5)


spots health =
    group << List.indexedMap (spot health)


balls =
    group << List.map ball


ball { color, size, x, y } =
    move x y (circle color size)
