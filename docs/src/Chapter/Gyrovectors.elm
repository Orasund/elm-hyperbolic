module Chapter.Gyrovectors exposing (..)

import ElmBook.Chapter exposing (Chapter)
import Html exposing (Html)
import Hyperbolic
import Layout
import View.Canvas


regularTiling vertices polygonsAroundAPoint amount =
    let
        radius =
            Hyperbolic.discFillingPolygon
                { vertices = vertices
                , polygonsAroundAPoint = polygonsAroundAPoint
                }

        init =
            List.range 0 polygonsAroundAPoint
                |> List.map
                    (\i ->
                        ( Hyperbolic.fromPolarCoords
                            { radius = radius
                            , angle =
                                2
                                    * pi
                                    * (1 + i |> modBy polygonsAroundAPoint |> toFloat)
                                    / toFloat polygonsAroundAPoint
                            }
                            |> Hyperbolic.toPoincareVector
                        , Hyperbolic.origin
                            |> Hyperbolic.toPoincareVector
                        )
                    )

        iterate ( p, fromP ) =
            List.range 1 (polygonsAroundAPoint - 1)
                |> List.map toFloat
                |> List.map (\i -> i * 2 * pi / toFloat polygonsAroundAPoint)
                |> List.map
                    (\angle ->
                        Hyperbolic.add
                            (fromP
                                |> Hyperbolic.vectorTo p
                                |> Hyperbolic.rotateClockwise -angle
                            )
                            p
                    )
                |> List.map
                    (\newP ->
                        ( newP
                        , p
                        )
                    )
    in
    List.range 0 amount
        |> List.foldl
            (\_ ( list, output ) ->
                ( list |> List.concatMap iterate, list ++ output )
            )
            ( init, [] )
        |> (\( a, b ) -> a ++ b)
        |> List.map (Tuple.mapBoth Hyperbolic.fromPoincareVector Hyperbolic.fromPoincareVector)
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


addition : Html msg
addition =
    let
        p1 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 0
                }
                |> Hyperbolic.toPoincareVector

        p2 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 2 * pi / 4
                }
                |> Hyperbolic.toPoincareVector

        origin =
            Hyperbolic.origin |> Hyperbolic.toPoincareVector
    in
    [ ( origin, p1 )
    , ( origin, p2 )
    , ( p1, p1 |> Hyperbolic.add p2 )
    , ( p2, p2 |> Hyperbolic.add p1 )
    ]
        |> List.map
            (Tuple.mapBoth Hyperbolic.fromPoincareVector
                Hyperbolic.fromPoincareVector
            )
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


inCircles : Float -> Html msg
inCircles radius =
    let
        moveRight p from =
            Hyperbolic.add
                (from
                    |> Hyperbolic.vectorTo p
                    |> Hyperbolic.rotateClockwise (2 * pi / 4)
                )
                p

        p0 =
            Hyperbolic.fromPolarCoords
                { radius = radius
                , angle = 0
                }
                |> Hyperbolic.toPoincareVector

        origin =
            Hyperbolic.origin
                |> Hyperbolic.toPoincareVector

        p1 =
            moveRight p0 origin

        p2 =
            moveRight p1 p0

        p3 =
            moveRight p2 p1

        p4 =
            moveRight p3 p2

        p5 =
            moveRight p4 p3
    in
    [ ( origin, p0 )
    , ( p0, p1 )
    , ( p1, p2 )
    , ( p2, p3 )
    , ( p3, p4 )
    , ( p4, p5 )
    ]
        |> List.map
            (Tuple.mapBoth Hyperbolic.fromPoincareVector
                Hyperbolic.fromPoincareVector
            )
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


turningRight : Html msg
turningRight =
    [ inCircles 0.75
    , inCircles (Hyperbolic.discFillingPolygon { vertices = 5, polygonsAroundAPoint = 4 })
    , inCircles 1.2
    , inCircles (Hyperbolic.discFillingPolygon { vertices = 6, polygonsAroundAPoint = 4 })
    ]
        |> Layout.row [ Layout.spaceBetween ]


additionMeeting : Html msg
additionMeeting =
    let
        polygonsAroundAPoint =
            5

        length =
            Hyperbolic.discFillingPolygon
                { vertices = 4, polygonsAroundAPoint = polygonsAroundAPoint }

        moveLeft p from =
            Hyperbolic.add
                (p
                    |> Hyperbolic.vectorTo from
                    |> Hyperbolic.rotateClockwise -(2 * pi / polygonsAroundAPoint)
                )
                p

        moveRight p from =
            Hyperbolic.add
                (p
                    |> Hyperbolic.vectorTo from
                    |> Hyperbolic.rotateClockwise (2 * pi / polygonsAroundAPoint)
                )
                p

        p0 =
            Hyperbolic.fromPolarCoords
                { radius = length
                , angle = 0
                }
                |> Hyperbolic.toPoincareVector

        origin =
            Hyperbolic.origin
                |> Hyperbolic.toPoincareVector

        p1 =
            moveLeft p0 origin

        p2 =
            moveRight p0 origin
    in
    [ ( origin, p0 )
    , ( p0, p1 )
    , ( p0, p2 )
    , ( p1, moveRight p1 p0 )
    , ( p2, moveLeft p2 p0 )
    ]
        |> List.map
            (Tuple.mapBoth Hyperbolic.fromPoincareVector
                Hyperbolic.fromPoincareVector
            )
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc
        |> View.Canvas.hyperbolic


tiling : Html msg
tiling =
    regularTiling 3 7 0


alternativeTilings : Html msg
alternativeTilings =
    Layout.row [ Layout.spaceBetween ]
        [ regularTiling 3 9 0
        , regularTiling 4 5 0
        , regularTiling 5 4 1
        ]


chapter : Chapter state
chapter =
    ElmBook.Chapter.chapter "Gyrovectors and angles"
        |> ElmBook.Chapter.withComponentList
            [ ( "Regular tiling of triangles", tiling )
            , ( "Alternative tilings", alternativeTilings )
            , ( "Adding to Gyrovectors", addition )
            , ( "Turning left", turningRight )
            , ( "Rectangle in Hyperbolic space", additionMeeting )
            ]
        |> ElmBook.Chapter.render
            """
In the last chapter we noticed, that it's not that easy to create a regular tiling of the hyperbolic disc.

Up until this point we shied away from using angles. But now we will introduce a secret weapon that will not only allow us to use angles, but also will create any regular tiling (where the origin is a vertex): Gyrovectors.

We will use Gyrovectors for the Poincaré Hyperbolic Disc Model (as it preserves angles). We could also use gyrovectors for the Beltrami-Klein Disc Model and the elm package also includes the required addition to do so. 


    let
        p1 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 0
                }
                |> Hyperbolic.toPoincareVector

        p2 =
            Hyperbolic.fromPolarCoords
                { radius = 1
                , angle = 2 * pi / 4
                }
                |> Hyperbolic.toPoincareVector

        origin =
            Hyperbolic.origin |> Hyperbolic.toPoincareVector
    in
    [ ( origin, p1 )
    , ( origin, p2 )
    , ( p1, p1 |> Hyperbolic.add p2 )
    , ( p2, p2 |> Hyperbolic.add p1 )
    ]
        |> List.map
            (Tuple.mapBoth Hyperbolic.fromPoincareVector
                Hyperbolic.fromPoincareVector
            )
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc


<component with-label="Adding to Gyrovectors"/>

Gyrovectors are like regular vectors with the catch that the order in which you add vectors matters.
In the example above you see that we move right and down in different orders.
Normally, you would expect that the resulting point matches, but here it does not.

<component with-label="Turning left"/>

Similarly, turning left four times will not end up in the same spot as you started. However, if we choose the length correctly we end up with a pentagon or actually any regular polygon with >4 sides.

Luckily, we have a function that can calculate the exact size that we need. If you look up tilings, you will usually find a Schläfli symbol {p,q}. In that case p = vertices, q = polygonsAroundAPoint.

    let
        vertices =
            3

        polygonsAroundAPoint =
            7

        radius =
            Hyperbolic.discFillingPolygon
                { vertices = vertices
                , polygonsAroundAPoint = polygonsAroundAPoint
                }

        init =
            List.range 0 polygonsAroundAPoint
                |> List.map
                    (\\i ->
                        ( Hyperbolic.fromPolarCoords
                            { radius = radius
                            , angle =
                                2
                                    * pi
                                    * (1 + i |> modBy polygonsAroundAPoint |> toFloat)
                                    / toFloat polygonsAroundAPoint
                            }
                            |> Hyperbolic.toPoincareVector
                        , Hyperbolic.origin
                            |> Hyperbolic.toPoincareVector
                        )
                    )

        iterate ( p, fromP ) =
            List.range 1 (polygonsAroundAPoint - 1)
                |> List.map toFloat
                |> List.map (\\i -> i * 2 * pi / toFloat polygonsAroundAPoint)
                |> List.map
                    (\u{0007}ngle ->
                        Hyperbolic.add
                            (p
                                |> Hyperbolic.negate
                                |> Hyperbolic.add fromP
                                |> Hyperbolic.rotateClockwise -angle
                            )
                            p
                    )
                |> List.map
                    (\\newP ->
                        ( newP
                        , p
                        )
                    )
    in
    List.range 0 0
        |> List.foldl
            (\\_ ( list, output ) ->
                ( list |> List.concatMap iterate, list ++ output )
            )
            ( init, [] )
        |> (\\( a, b ) -> a ++ b)
        |> List.map (Tuple.mapBoth Hyperbolic.fromPoincareVector Hyperbolic.fromPoincareVector)
        |> List.concatMap (Hyperbolic.pointsAlongLineSegment 100)
        |> List.map Hyperbolic.projectOntoPoincareDisc

<component with-label="Regular tiling of triangles"/>

Sadly i do not have the time to explain exactly what is happening here, but just note, that this construction **only works if we center the initial polygons around the origin.**

<component with-label="Alternative tilings"/>
"""
