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
                            (p
                                |> Hyperbolic.negate
                                |> Hyperbolic.add fromP
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
    ElmBook.Chapter.chapter "Working with Gyrovectors"
        |> ElmBook.Chapter.withComponentList
            [ ( "Regular tiling of triangles", tiling )
            , ( "Alternative tilings", alternativeTilings )
            ]
        |> ElmBook.Chapter.render
            """
In the last chapter we noticed, that it's not that easy to create a regular tiling of the hyperbolic disc.

Up until this point we shied away from using angles. But now we will introduce a secret weapon that will not only allow us to use angle, but also will create any regular tiling (where the origin is a vertex).

The idea is to construct triangles by rotating a vector around a point. But of course we can't just regular vectors (that would be too easy). Instead we will use gyrovectors.

You don't have to know much about gyrovectors, but if you want to go ahead and do you own research, gyrovectors is probably the thing you want to investigate.

For now we only need to know that we use Gyrovectors for the Poincaré Hyperbolic Disc Model (as it preserves angles). We could also use gyrovectors for the Beltrami-Klein Disc Model and the elm package also includes the required addition to do so. 

Also another thing to keep in mind is that depending on the size of the triangles, we can fit different amount of the around a point.

That's why we need to calculate the length of a size depending on the amount of vertices (3 for a triangle) and on how many polygons should fit around the point.

If you look up tilings, you will usually find a Schläfli symbol {p,q}. In that case p = vertices, q = polygonsAroundAPoint.
```
tiling =
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
```
<component with-label="Regular tiling of triangles"/>

Sadly i do not have the time to explain exactly what is happening here, but just note, that this construction **only works if we center the initial polygons around the origin.**

<component with-label="Alternative tilings"/>
"""
