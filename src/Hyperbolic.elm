module Hyperbolic exposing
    ( Point, origin, pointAtInfinity, pointsAreEqual, distanceTo, distanceToOrigin, discFillingPolygon
    , Line, IdealPoint, HyperIdealPoint, LineSegment, poleOfLine, lineFromIdealPoints, lineSectionTo, lineFromPoints, lineFromLineSegment, midpointOfLine, midpointOfLineSegment, lineFromHyperIdealPointThrough, perpendicularLineThrough, intersectLines, bisectorThrough, reflectBy
    , Gyrovector, vectorTo, negate, add, length, einsteinVelocityAddition, rotateClockwise, rotatePointClockwise, scaleBy
    , pointsAlongLine, pointsAlongLineSegment, fromPolarCoords, projectFromEuclideanSpace, projectOntoBeltramiKleinDisc, projectOntoPoincareDisc
    , unsafeFromAxialCoord, unsafeFromIdealPoint, unsafeFromRecord, unsafeHyperIdealPointToRecord, unsafePoincareVectorFromRecord, unsafePoincareVectorToRecord, unsafeToAxialCoord, unsafeToRecord
    )

{-| Explore the hyperbolic space using this module.
Checkout [the doc for examples](https://orasund.github.io/elm-hyperbolic).


# Points

@docs Point, origin, pointAtInfinity, pointsAreEqual, distanceTo, distanceToOrigin, discFillingPolygon


# Lines And Line Segments

@docs Line, IdealPoint, HyperIdealPoint, LineSegment, poleOfLine, lineFromIdealPoints, lineSectionTo, lineFromPoints, lineFromLineSegment, midpointOfLine, midpointOfLineSegment, lineFromHyperIdealPointThrough, perpendicularLineThrough, intersectLines, bisectorThrough, reflectBy


# Gyrovectors

@docs Gyrovector, vectorTo, negate, add, length, einsteinVelocityAddition, rotateClockwise, rotatePointClockwise, scaleBy


# Convertion

@docs pointsAlongLine, pointsAlongLineSegment, fromPolarCoords, projectFromEuclideanSpace, projectOntoBeltramiKleinDisc, projectOntoPoincareDisc


# Advanced

@docs unsafeFromAxialCoord, unsafeFromIdealPoint, unsafeFromRecord, unsafeHyperIdealPointToRecord, unsafePoincareVectorFromRecord, unsafePoincareVectorToRecord, unsafeToAxialCoord, unsafeToRecord

-}

import Internal exposing (artanh, normalVector, tanh)


{-| Module for working with hyperbolic geometry. Uses the Beltrami–Klein model.

Ideal points have Beltrami coordinates with length 1.

Hyper ideal points have Beltrami coordinates with length > 1.

-}
type Point
    = BeltramiCoord ( Float, Float )


{-| An [ideal point](https://en.wikipedia.org/wiki/Ideal_point) is a point at infinity and can be represented by an angle
-}
type IdealPoint
    = IdealPoint Float


{-| A hyper ideal point is a point that only exists in the euclidean projection of the hyperbolic plane into the Beltrami–Klein model.
-}
type HyperIdealPoint
    = HyperIdealPoint ( Float, Float )


{-| We define a line by two [ideal points](https://en.wikipedia.org/wiki/Ideal_point).
-}
type alias Line =
    ( IdealPoint, IdealPoint )


{-| A line segment is defined by two points
-}
type alias LineSegment =
    ( Point, Point )


{-| Gyrovector in Poincare-Model
-}
type Gyrovector
    = PoincareVector ( Float, Float )


{-| An ideal point is a point at infinity, uniquely defined by an angle.
-}
pointAtInfinity : Float -> IdealPoint
pointAtInfinity angle =
    IdealPoint angle


{-| The origin is the point in the center.
-}
origin : Point
origin =
    BeltramiCoord ( 0, 0 )


{-| Returns if two point can be considered equal.
-}
pointsAreEqual : Point -> Point -> Bool
pointsAreEqual (BeltramiCoord ( x1, y1 )) (BeltramiCoord ( x2, y2 )) =
    Internal.equal x1 x2 && Internal.equal y1 y2


{-| Get the distance between two points
-}
distanceTo : Point -> Point -> Float
distanceTo (BeltramiCoord u) (BeltramiCoord v) =
    --https://en.wikipedia.org/wiki/Beltrami%E2%80%93Klein_model
    case
        Internal.lineToGeneralForm ( u, v )
            |> Internal.intersectLineWithUnitCircle
    of
        Just ( tempA, tempB ) ->
            let
                ( a, b ) =
                    if Internal.distance tempA u < Internal.distance tempA u then
                        ( tempA, tempB )

                    else
                        ( tempB, tempA )
            in
            logBase e
                ((Internal.distance v a * Internal.distance b u)
                    / (Internal.distance u a * Internal.distance b v)
                )
                / 2

        Nothing ->
            0


{-| Distance to the origin
-}
distanceToOrigin : Point -> Float
distanceToOrigin =
    distanceTo origin


{-| If you want to construct a tiling in the hyperbolic space, then the length of the polygon specifies how many polygons fit around a point.

If you look up tilings, you will usually find a Schläfli symbol {p,q}. In that case p = vertices, q = polygonsAroundAPoint.

The resulting length is for BeltramiCoords

-}
discFillingPolygon : { vertices : Int, polygonsAroundAPoint : Int } -> Float
discFillingPolygon args =
    --https://math.stackexchange.com/questions/1331199/edge-length-of-hyperbolic-tesselations
    (cos (pi / toFloat args.vertices)
        / sin (pi / toFloat args.polygonsAroundAPoint)
    )
        |> Internal.arcosh
        |> (*) 2



------------------------------------------------------------------------------------------
-- Line And Line Section
------------------------------------------------------------------------------------------


{-| Constructs a line from two ideal points

    lineFromIdealPoints : IdealPoint -> IdealPoint -> Line
    lineFromIdealPoints =
        Tuple.pair

-}
lineFromIdealPoints : IdealPoint -> IdealPoint -> Line
lineFromIdealPoints =
    Tuple.pair


{-| Constructs a line segment to a point

    lineSectionTo : BeltramiPoint -> BeltramiPoint -> LineSegment
    lineSectionTo =
        Tuple.pair

-}
lineSectionTo : Point -> Point -> LineSegment
lineSectionTo =
    Tuple.pair


{-| constructs a line from two points
-}
lineFromPoints : Point -> Point -> Maybe Line
lineFromPoints (BeltramiCoord b1) (BeltramiCoord b2) =
    Internal.lineToGeneralForm ( b1, b2 )
        |> Internal.intersectLineWithUnitCircle
        |> Maybe.map (Tuple.mapBoth toPolar toPolar)
        |> Maybe.map
            (\( ( _, i1 ), ( _, i2 ) ) ->
                lineFromIdealPoints (IdealPoint i1) (IdealPoint i2)
            )


{-| Convert a line segment into a line
-}
lineFromLineSegment : LineSegment -> Maybe Line
lineFromLineSegment ( p1, p2 ) =
    lineFromPoints p1 p2


{-| Construct a line using an hyper ideal point
-}
lineFromHyperIdealPointThrough : Point -> HyperIdealPoint -> Maybe Line
lineFromHyperIdealPointThrough p (HyperIdealPoint h) =
    lineFromPoints p (BeltramiCoord h)


{-| Any line except lines going through the origin, can be associated by a unique hyper ideal point.
-}
poleOfLine : Line -> Maybe HyperIdealPoint
poleOfLine ( i1, i2 ) =
    let
        (BeltramiCoord p1) =
            unsafeFromIdealPoint i1

        (BeltramiCoord p2) =
            unsafeFromIdealPoint i2

        v1 =
            normalVector ( p1, ( 0, 0 ) )

        v2 =
            normalVector ( p2, ( 0, 0 ) )
    in
    Internal.lineToGeneralForm ( p2, Internal.plus p2 v2 )
        |> Internal.lineIntersection (Internal.lineToGeneralForm ( p1, Internal.plus p1 v1 ))
        |> Maybe.map HyperIdealPoint


{-| reflect a point by a line
-}
reflectBy : Line -> Point -> Point
reflectBy line p0 =
    perpendicularLineThrough p0 line
        |> Maybe.andThen (intersectLines line)
        |> Maybe.map
            (\p1 ->
                toPoincareVector p1
                    |> add (vectorTo (toPoincareVector p0) (toPoincareVector p1))
                    |> fromPoincareVector
            )
        |> Maybe.withDefault p0


{-| Point as to lie on the line
-}
perpendicularLineThrough : Point -> Line -> Maybe Line
perpendicularLineThrough (BeltramiCoord p) line =
    case poleOfLine line of
        Just (HyperIdealPoint pole) ->
            lineFromPoints (BeltramiCoord pole) (BeltramiCoord p)

        Nothing ->
            --line through origin
            let
                ( BeltramiCoord p1, BeltramiCoord p2 ) =
                    line
                        |> Tuple.mapBoth unsafeFromIdealPoint unsafeFromIdealPoint
            in
            lineFromPoints
                (Internal.normalVector ( p1, p2 )
                    |> Internal.plus p
                    |> BeltramiCoord
                )
                (BeltramiCoord p)


{-| return the nearest ideal point of a line from a given point
-}
nearestIdealPointOf : Line -> Point -> IdealPoint
nearestIdealPointOf ( i1, i2 ) (BeltramiCoord p) =
    let
        (BeltramiCoord p1) =
            unsafeFromIdealPoint i1

        (BeltramiCoord p2) =
            unsafeFromIdealPoint i2
    in
    if Internal.distance p1 p < Internal.distance p2 p then
        i1

    else
        i2


{-| Construct a bisector through a point
-}
bisectorThrough : Point -> ( Point, Point ) -> Maybe Line
bisectorThrough c ( a, b ) =
    Maybe.map2
        (\i1 i2 ->
            poleOfLine ( i1, i2 )
        )
        (lineFromPoints c a
            |> Maybe.map (\line -> nearestIdealPointOf line a)
        )
        (lineFromPoints c b
            |> Maybe.map (\line -> nearestIdealPointOf line b)
        )
        |> Maybe.andThen identity
        |> Maybe.andThen (lineFromHyperIdealPointThrough c)


{-| Construct the midpoint of a line
-}
midpointOfLine : Line -> Point
midpointOfLine ( i1, i2 ) =
    let
        (BeltramiCoord ( x1, y1 )) =
            unsafeFromIdealPoint i1

        (BeltramiCoord ( x2, y2 )) =
            unsafeFromIdealPoint i2
    in
    ( (x1 - x2) / 2 + x2, (y1 - y2) / 2 + y2 )
        |> BeltramiCoord


{-| Get the midpoint of a line segment
-}
midpointOfLineSegment : LineSegment -> Maybe Point
midpointOfLineSegment ( p1, p2 ) =
    lineFromPoints p1 p2
        |> Maybe.andThen
            (\line ->
                Maybe.map2
                    (\( i1, _ ) ( _, i4 ) ->
                        intersectLines ( i1, i4 ) line
                    )
                    (perpendicularLineThrough p1 line)
                    (perpendicularLineThrough p2 line)
                    |> Maybe.andThen identity
            )


{-| Two lines may intersect at exactly one point
-}
intersectLines : Line -> Line -> Maybe Point
intersectLines l1 l2 =
    let
        ( BeltramiCoord p1, BeltramiCoord p2 ) =
            l1 |> Tuple.mapBoth unsafeFromIdealPoint unsafeFromIdealPoint

        ( BeltramiCoord p3, BeltramiCoord p4 ) =
            l2 |> Tuple.mapBoth unsafeFromIdealPoint unsafeFromIdealPoint
    in
    Internal.lineIntersection (Internal.lineToGeneralForm ( p1, p2 )) (Internal.lineToGeneralForm ( p3, p4 ))
        |> Maybe.map BeltramiCoord


{-| Constructs a given amount of point along a line segment
-}
pointsAlongLineSegment : Int -> LineSegment -> List Point
pointsAlongLineSegment n ( BeltramiCoord ( x1, y1 ), BeltramiCoord ( x2, y2 ) ) =
    let
        vecX =
            x2 - x1

        vecY =
            y2 - y1
    in
    List.range 0 n
        |> List.map (\i -> toFloat i / toFloat n)
        |> List.map (\amount -> ( x1 + amount * vecX, y1 + amount * vecY ))
        |> List.map BeltramiCoord


{-| Constructs a given amount of point along a line
-}
pointsAlongLine : Int -> Line -> List Point
pointsAlongLine n ( i1, i2 ) =
    pointsAlongLineSegment n ( unsafeFromIdealPoint i1, unsafeFromIdealPoint i2 )



------------------------------------------------------------------------------------------
-- Vectors
------------------------------------------------------------------------------------------


{-| Creates a vector from two points. The length of the vector
-}
vectorTo : Gyrovector -> Gyrovector -> Gyrovector
vectorTo p2 p1 =
    p2 |> negate |> add p1


{-| scale a vector by a factor
-}
scaleBy : Float -> Gyrovector -> Gyrovector
scaleBy a (PoincareVector x) =
    --https://andbloch.github.io/K-Stereographic-Model/
    --kappa = -1
    Internal.artanh (Internal.length x)
        |> Maybe.map ((*) a)
        |> Maybe.map Internal.tanh
        |> Maybe.map (\v -> v / Internal.length x)
        |> Maybe.map (\v -> Internal.scaleBy v x)
        |> Maybe.withDefault x
        |> PoincareVector


{-| Rotate the vector clockwise
-}
rotateClockwise : Float -> Gyrovector -> Gyrovector
rotateClockwise amount (PoincareVector ( x, y )) =
    ( x * cos amount - y * sin amount
    , x * sin amount + y * cos amount
    )
        |> PoincareVector


{-| Rotate a point clockwise around the origin
-}
rotatePointClockwise : Float -> Point -> Point
rotatePointClockwise amount (BeltramiCoord ( x, y )) =
    --https://en.wikipedia.org/wiki/Rotation_matrix#:~:text=To%20perform%20the%20rotation%20on,the%20trigonometric%20summation%20angle%20formulae.
    ( x * cos amount - y * sin amount
    , x * sin amount + y * cos amount
    )
        |> BeltramiCoord


{-| Negate a vector
-}
negate : Gyrovector -> Gyrovector
negate (PoincareVector v) =
    v
        |> Internal.negate
        |> PoincareVector


{-| Addition for Gyrovectors in the Beltrami Disc Model.
-}
einsteinVelocityAddition : Point -> Point -> Point
einsteinVelocityAddition (BeltramiCoord b) (BeltramiCoord a) =
    --https://en.wikipedia.org/wiki/Gyrovector_space
    let
        v =
            b

        u =
            a

        cSquared =
            1

        gamma =
            1 / sqrt (1 - Internal.innerProduct u u / cSquared)
    in
    u
        |> Internal.plus (v |> Internal.scaleBy (1 / gamma))
        |> Internal.plus (u |> Internal.scaleBy (gamma * Internal.innerProduct u v / (cSquared * (1 + gamma))))
        |> Internal.scaleBy (1 / (1 + Internal.innerProduct u v / cSquared))
        |> BeltramiCoord


{-| Using the Möbius Addition to add two vectors in the poincare disc
-}
add : Gyrovector -> Gyrovector -> Gyrovector
add (PoincareVector v) (PoincareVector u) =
    --https://en.wikipedia.org/wiki/Gyrovector_space#Beltrami%E2%80%93Klein_disc/ball_model_and_Einstein_addition
    --s = 1
    let
        normSquared x =
            Internal.innerProduct x x
    in
    u
        |> Internal.scaleBy
            (1
                + (2 * Internal.innerProduct u v)
                + Internal.innerProduct v v
            )
        |> Internal.plus (v |> Internal.scaleBy (1 - normSquared u))
        |> Internal.scaleBy
            (1
                / (1
                    + (2 * Internal.innerProduct u v)
                    + (normSquared v * normSquared u)
                  )
            )
        |> PoincareVector


{-| Return the length of a vector
-}
length : Gyrovector -> Float
length (PoincareVector v) =
    v
        |> Internal.length
        |> Internal.artanh
        |> Maybe.withDefault 0
        |> (*) 2



------------------------------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------------------------------


{-| Constructs a point using polar coordinates.
-}
fromPolarCoords : { radius : Float, angle : Float } -> Point
fromPolarCoords args =
    --https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane
    Tuple.pair
        (tanh args.radius * cos args.angle)
        (tanh args.radius * sin args.angle)
        |> BeltramiCoord


{-| Projects points from euclidean space into hyperbolic space.
However, you can not expect the proportions to stay the same.

Points further out experience more distortion.

-}
projectFromEuclideanSpace : ( Float, Float ) -> Point
projectFromEuclideanSpace ( x, y ) =
    toPolar ( x, y )
        |> (\( radius, angle ) -> fromPolarCoords { radius = radius, angle = angle })


{-| Converts Beltrami Coordinates into [Axial coordinates](https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane#Axial_coordinates).
-}
unsafeFromAxialCoord : ( Float, Float ) -> Point
unsafeFromAxialCoord ( x, y ) =
    BeltramiCoord ( tanh x, tanh y )


{-| Converts [Axial coordinates](https://en.wikipedia.org/wiki/Coordinate_systems_for_the_hyperbolic_plane#Axial_coordinates) into Beltrami Coordinates.
-}
unsafeToAxialCoord : Point -> Maybe ( Float, Float )
unsafeToAxialCoord (BeltramiCoord ( x, y )) =
    Maybe.map2 Tuple.pair
        (artanh x)
        (artanh y)


{-| Project Hyperbolic points to the Beltrami klein disc
-}
projectOntoBeltramiKleinDisc : Point -> ( Float, Float )
projectOntoBeltramiKleinDisc (BeltramiCoord p) =
    p


{-| convert a point into a vector
-}
toPoincareVector : Point -> Gyrovector
toPoincareVector (BeltramiCoord s) =
    --https://en.wikipedia.org/wiki/Beltrami%E2%80%93Klein_model
    let
        l =
            1 + sqrt (1 - Internal.innerProduct s s)
    in
    s
        |> Internal.scaleBy (1 / l)
        |> PoincareVector


{-| convert a vector into a point
-}
fromPoincareVector : Gyrovector -> Point
fromPoincareVector (PoincareVector u) =
    --https://en.wikipedia.org/wiki/Beltrami%E2%80%93Klein_model
    u
        |> Internal.scaleBy (2 / (1 + Internal.innerProduct u u))
        |> BeltramiCoord


{-| Project Hyperbolic points to the Poincare disc
-}
projectOntoPoincareDisc : Point -> ( Float, Float )
projectOntoPoincareDisc p =
    p
        |> toPoincareVector
        |> (\(PoincareVector s) -> s)


{-| -}
unsafeHyperIdealPointToRecord : HyperIdealPoint -> { x : Float, y : Float }
unsafeHyperIdealPointToRecord (HyperIdealPoint ( x, y )) =
    { x = x, y = y }


{-| -}
unsafeFromIdealPoint : IdealPoint -> Point
unsafeFromIdealPoint (IdealPoint angle1) =
    ( 1, angle1 ) |> fromPolar |> BeltramiCoord


{-| -}
unsafeFromRecord : { x : Float, y : Float } -> Point
unsafeFromRecord { x, y } =
    BeltramiCoord ( x, y )


{-| -}
unsafePoincareVectorFromRecord : { x : Float, y : Float } -> Gyrovector
unsafePoincareVectorFromRecord { x, y } =
    PoincareVector ( x, y )


{-| -}
unsafeToRecord : Point -> { x : Float, y : Float }
unsafeToRecord (BeltramiCoord ( x, y )) =
    { x = x, y = y }


{-| -}
unsafePoincareVectorToRecord : Gyrovector -> { x : Float, y : Float }
unsafePoincareVectorToRecord (PoincareVector ( x, y )) =
    { x = x, y = y }
