module Main exposing
    ( Circle
    , Complex
    , buildInitialConfig
    , cAbs
    , cSub
    , descartesCurvature
    , descartesCenter
    , generateGasket
    , inverseCircle
    , symmetricConfig
    , main
    )

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Svg exposing (circle, svg)
import Svg.Attributes as SA


-- TYPES


type alias Complex =
    { re : Float, im : Float }


type alias Circle =
    { k : Float
    , z : Complex
    , depth : Int
    }


type alias Model =
    { circles : List Circle
    , seed : Random.Seed
    , generating : Bool
    }


type Msg
    = Regenerate
    | GotSeed Random.Seed


-- COMPLEX ARITHMETIC


cAdd : Complex -> Complex -> Complex
cAdd a b =
    { re = a.re + b.re, im = a.im + b.im }


cSub : Complex -> Complex -> Complex
cSub a b =
    { re = a.re - b.re, im = a.im - b.im }


cMul : Complex -> Complex -> Complex
cMul a b =
    { re = a.re * b.re - a.im * b.im
    , im = a.re * b.im + a.im * b.re
    }


cScale : Float -> Complex -> Complex
cScale s c =
    { re = s * c.re, im = s * c.im }


cAbs : Complex -> Float
cAbs c =
    sqrt (c.re * c.re + c.im * c.im)


-- Complex sqrt: principal branch
cSqrt : Complex -> Complex
cSqrt c =
    let
        r = cAbs c
        re = sqrt ((r + c.re) / 2)
        im = sqrt ((r - c.re) / 2) * (if c.im >= 0 then 1 else -1)
    in
    { re = re, im = im }


-- DESCARTES CIRCLE THEOREM


-- Given 3 curvatures, find the 4th (both solutions)
-- Returns (k4a, k4b) where k4a = k1+k2+k3 + 2√(k1k2+k2k3+k3k1)
descartesCurvature : Float -> Float -> Float -> ( Float, Float )
descartesCurvature k1 k2 k3 =
    let
        s = k1 + k2 + k3
        disc = k1 * k2 + k2 * k3 + k3 * k1
        sq = 2 * sqrt (max 0 disc)
    in
    ( s + sq, s - sq )


-- Complex Descartes for center, given 4 curvatures and 3 centers
-- k4*z4 = k1z1 + k2z2 + k3z3 ± 2√(k1k2z1z2 + k2k3z2z3 + k3k1z3z1)
descartesCenter : Circle -> Circle -> Circle -> Float -> ( Complex, Complex )
descartesCenter c1 c2 c3 k4 =
    let
        t1 = cScale c1.k c1.z
        t2 = cScale c2.k c2.z
        t3 = cScale c3.k c3.z
        sumKZ = t1 |> cAdd t2 |> cAdd t3

        p12 = cScale (c1.k * c2.k) (cMul c1.z c2.z)
        p23 = cScale (c2.k * c3.k) (cMul c2.z c3.z)
        p31 = cScale (c3.k * c1.k) (cMul c3.z c1.z)
        underRadical = p12 |> cAdd p23 |> cAdd p31
        sq = cSqrt underRadical
        sq2 = cScale 2 sq

        z4plus = cScale (1 / k4) (cAdd sumKZ sq2)
        z4minus = cScale (1 / k4) (cSub sumKZ sq2)
    in
    ( z4plus, z4minus )


-- Apollonian inversion: given 4 mutually tangent circles (c1,c2,c3,c4),
-- find the other circle tangent to c1,c2,c3 (the Descartes partner of c4).
-- k_new = 2(k1+k2+k3) - k4
-- k_new * z_new = 2(k1z1+k2z2+k3z3) - k4*z4
inverseCircle : Circle -> Circle -> Circle -> Circle -> Maybe Circle
inverseCircle c1 c2 c3 c4 =
    let
        kNew = 2 * (c1.k + c2.k + c3.k) - c4.k
        t1 = cScale c1.k c1.z
        t2 = cScale c2.k c2.z
        t3 = cScale c3.k c3.z
        sumKZ = t1 |> cAdd t2 |> cAdd t3
        numerator = cSub (cScale 2 sumKZ) (cScale c4.k c4.z)
        zNew = cScale (1 / kNew) numerator
        minRadius = 0.0005
    in
    if kNew > 0 && (1 / kNew) > minRadius then
        Just { k = kNew, z = zNew, depth = 1 + (List.minimum [ c1.depth, c2.depth, c3.depth ] |> Maybe.withDefault 0) }
    else
        Nothing


-- Circle key for deduplication
circleKey : Circle -> String
circleKey c =
    let
        x = round (c.z.re * 1000)
        y = round (c.z.im * 1000)
        k = round (c.k * 1000)
    in
    String.fromInt k ++ "," ++ String.fromInt x ++ "," ++ String.fromInt y


-- GASKET GENERATION


type alias QueueItem =
    { c1 : Circle, c2 : Circle, c3 : Circle, c4 : Circle }


generateGasket : Circle -> Circle -> Circle -> Circle -> List Circle
generateGasket outer c1 c2 c3 =
    let
        initSeen =
            Dict.fromList
                [ ( circleKey outer, True )
                , ( circleKey c1, True )
                , ( circleKey c2, True )
                , ( circleKey c3, True )
                ]

        initQueue =
            [ { c1 = c1, c2 = c2, c3 = c3, c4 = outer }
            , { c1 = c1, c2 = c2, c3 = outer, c4 = c3 }
            , { c1 = c1, c2 = c3, c3 = outer, c4 = c2 }
            , { c1 = c2, c2 = c3, c3 = outer, c4 = c1 }
            ]

        initCircles =
            [ outer, c1, c2, c3 ]
    in
    processQueue initQueue initSeen initCircles 0


processQueue : List QueueItem -> Dict String Bool -> List Circle -> Int -> List Circle
processQueue queue seen circles count =
    if count > 20000 || List.isEmpty queue then
        circles
    else
        case queue of
            [] ->
                circles

            item :: rest ->
                case inverseCircle item.c1 item.c2 item.c3 item.c4 of
                    Nothing ->
                        processQueue rest seen circles (count + 1)

                    Just newC ->
                        let
                            key = circleKey newC
                        in
                        if Dict.member key seen then
                            processQueue rest seen circles (count + 1)
                        else
                            let
                                newSeen = Dict.insert key True seen
                                newQueue =
                                    rest
                                        ++ [ { c1 = item.c1, c2 = item.c2, c3 = newC, c4 = item.c3 }
                                           , { c1 = item.c1, c2 = item.c3, c3 = newC, c4 = item.c2 }
                                           , { c1 = item.c2, c2 = item.c3, c3 = newC, c4 = item.c1 }
                                           ]
                            in
                            processQueue newQueue newSeen (newC :: circles) (count + 1)


-- INITIAL CONFIGURATION GENERATION


type alias Quadruple =
    { outer : Circle, c1 : Circle, c2 : Circle, c3 : Circle }


randomRadii : Generator ( Float, Float, Float )
randomRadii =
    Random.map3
        (\a b c ->
            let
                r1 = 0.15 + a * 0.2
                r2 = 0.15 + b * 0.2
                r3 = 0.15 + c * 0.2
            in
            ( r1, r2, r3 )
        )
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)


buildInitialConfig : ( Float, Float, Float ) -> Maybe Quadruple
buildInitialConfig ( r1, r2, r3 ) =
    let
        k1 = 1 / r1
        k2 = 1 / r2
        k3 = 1 / r3

        z1 = { re = 0, im = 0 }
        z2 = { re = r1 + r2, im = 0 }

        d12 = r1 + r2
        d13 = r1 + r3
        d23 = r2 + r3

        x3 = (d13 * d13 - d23 * d23 + d12 * d12) / (2 * d12)
        y3sq = d13 * d13 - x3 * x3
    in
    if y3sq < 0 then
        Nothing
    else
        let
            y3 = sqrt y3sq
            z3 = { re = x3, im = y3 }

            c1 = Circle k1 z1 0
            c2 = Circle k2 z2 0
            c3 = Circle k3 z3 0

            ( _, kOuterNeg ) = descartesCurvature k1 k2 k3
            kOuter = kOuterNeg

            ( zOuterA, zOuterB ) = descartesCenter c1 c2 c3 kOuter

            centroid =
                { re = (z1.re + z2.re + z3.re) / 3
                , im = (z1.im + z2.im + z3.im) / 3
                }

            zOuter =
                if cAbs (cSub zOuterA centroid) < cAbs (cSub zOuterB centroid) then
                    zOuterA
                else
                    zOuterB

            -- rOuter = 1/|kOuter|; normalize so outer radius = 1
            -- Scaling by 1/rOuter: coordinates divide by rOuter, curvatures multiply by rOuter
            rOuter = abs (1 / kOuter)
            cx = zOuter.re
            cy = zOuter.im

            translate z = { re = (z.re - cx) / rOuter, im = (z.im - cy) / rOuter }

            normalizedOuter = { outer | z = { re = 0, im = 0 }, k = -1 }
            normalized1 = { c1 | z = translate z1, k = k1 * rOuter }
            normalized2 = { c2 | z = translate z2, k = k2 * rOuter }
            normalized3 = { c3 | z = translate z3, k = k3 * rOuter }

            outer = Circle kOuter zOuter 0
        in
        Just { outer = normalizedOuter, c1 = normalized1, c2 = normalized2, c3 = normalized3 }


-- Fallback: symmetric config with three equal circles inscribed in unit disk
-- r = 2√3 - 3 ≈ 0.4641, centers at distance 1-r from origin
symmetricConfig : Quadruple
symmetricConfig =
    let
        r = 2 * sqrt 3 - 3
        k = 1 / r
        d = 1 - r

        c1 = Circle k { re = d, im = 0 } 0
        c2 = Circle k { re = d * cos (2 * pi / 3), im = d * sin (2 * pi / 3) } 0
        c3 = Circle k { re = d * cos (4 * pi / 3), im = d * sin (4 * pi / 3) } 0
        outer = Circle -1 { re = 0, im = 0 } 0
    in
    { outer = outer, c1 = c1, c2 = c2, c3 = c3 }


buildCircles : ( Float, Float, Float ) -> List Circle
buildCircles radii =
    let
        q = buildInitialConfig radii |> Maybe.withDefault symmetricConfig
    in
    generateGasket q.outer q.c1 q.c2 q.c3


-- SVG RENDERING


viewWidth : Float
viewWidth = 700


viewHeight : Float
viewHeight = 700


viewRadius : Float
viewRadius = 320


toSvgX : Float -> Float
toSvgX x =
    viewWidth / 2 + x * viewRadius


toSvgY : Float -> Float
toSvgY y =
    viewHeight / 2 - y * viewRadius


renderCircle : Circle -> Svg.Svg Msg
renderCircle c =
    let
        r = abs (1 / c.k)
        cx = toSvgX c.z.re
        cy = toSvgY c.z.im
        svgR = r * viewRadius
    in
    if svgR < 0.3 then
        Svg.text_ [] []
    else
        circle
            [ SA.cx (String.fromFloat cx)
            , SA.cy (String.fromFloat cy)
            , SA.r (String.fromFloat svgR)
            , SA.fill "white"
            , SA.stroke "black"
            , SA.strokeWidth (String.fromFloat (max 0.3 (svgR * 0.015)))
            ]
            []


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "Georgia, serif"
        , style "background" "white"
        , style "min-height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "padding" "24px"
        , style "color" "#111"
        ]
        [ h1
            [ style "font-size" "2rem"
            , style "margin" "0 0 4px 0"
            , style "letter-spacing" "0.08em"
            ]
            [ text "Apollonian Gasket" ]
        , p
            [ style "margin" "0 0 16px 0"
            , style "font-size" "0.9rem"
            , style "color" "#555"
            , style "font-style" "italic"
            ]
            [ text (String.fromInt (List.length model.circles) ++ " circles · fractal of tangent circles") ]
        , svg
            [ SA.width (String.fromFloat viewWidth)
            , SA.height (String.fromFloat viewHeight)
            , SA.viewBox ("0 0 " ++ String.fromFloat viewWidth ++ " " ++ String.fromFloat viewHeight)
            , style "border" "1px solid #ccc"
            , style "border-radius" "4px"
            , style "background" "white"
            ]
            -- Sort by descending radius so large circles (including outer) render first
            (List.sortBy (\c -> 1 / abs c.k) model.circles
                |> List.reverse
                |> List.map renderCircle
            )
        , button
            [ onClick Regenerate
            , style "margin-top" "20px"
            , style "padding" "10px 32px"
            , style "font-size" "1rem"
            , style "font-family" "Georgia, serif"
            , style "background" "white"
            , style "color" "#111"
            , style "border" "1px solid #999"
            , style "border-radius" "6px"
            , style "cursor" "pointer"
            , style "letter-spacing" "0.06em"
            ]
            [ text "Regenerate" ]
        ]


-- MAIN


init : () -> ( Model, Cmd Msg )
init _ =
    let
        seed = Random.initialSeed 42
        ( radii, nextSeed ) = Random.step randomRadii seed
        circles = buildCircles radii
    in
    ( { circles = circles, seed = nextSeed, generating = False }
    , Random.generate GotSeed Random.independentSeed
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            let
                ( radii, nextSeed ) = Random.step randomRadii seed
                circles = buildCircles radii
            in
            ( { model | circles = circles, seed = nextSeed }
            , Cmd.none
            )

        Regenerate ->
            let
                ( radii, nextSeed ) = Random.step randomRadii model.seed
                circles = buildCircles radii
            in
            ( { model | circles = circles, seed = nextSeed }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
