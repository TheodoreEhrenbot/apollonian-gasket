module GasketTest exposing (..)

import Expect
import Main
    exposing
        ( Circle
        , Complex
        , buildInitialConfig
        , cAbs
        , cSub
        , descartesCenter
        , descartesCurvature
        , generateGasket
        , inverseCircle
        , symmetricConfig
        )
import Test exposing (..)


epsilon : Float
epsilon =
    1.0e-6


-- Check external tangency: |dist(c1,c2) - (r1+r2)| < epsilon
isExternallyTangent : Circle -> Circle -> Bool
isExternallyTangent c1 c2 =
    let
        dist = cAbs (cSub c1.z c2.z)
        r1 = abs (1 / c1.k)
        r2 = abs (1 / c2.k)
    in
    abs (dist - (r1 + r2)) < epsilon


-- Check internal tangency: |dist - |r1 - r2|| < epsilon
isInternallyTangent : Circle -> Circle -> Bool
isInternallyTangent c1 c2 =
    let
        dist = cAbs (cSub c1.z c2.z)
        r1 = abs (1 / c1.k)
        r2 = abs (1 / c2.k)
    in
    abs (dist - abs (r1 - r2)) < epsilon


suite : Test
suite =
    describe "Apollonian Gasket"
        [ describe "Descartes curvature theorem"
            [ test "(1,1,1) → positive solution ≈ 6.464" <|
                \_ ->
                    let
                        ( k4pos, _ ) = descartesCurvature 1 1 1
                        expected = 3 + 2 * sqrt 3
                    in
                    Expect.within (Expect.Absolute 0.001) expected k4pos

            , test "(1,1,1) → negative solution ≈ -0.464" <|
                \_ ->
                    let
                        ( _, k4neg ) = descartesCurvature 1 1 1
                        expected = 3 - 2 * sqrt 3
                    in
                    Expect.within (Expect.Absolute 0.001) expected k4neg

            , test "symmetric config outer k is a Descartes solution" <|
                \_ ->
                    let
                        cfg = symmetricConfig
                        ( k4pos, k4neg ) = descartesCurvature cfg.c1.k cfg.c2.k cfg.c3.k
                        kOuter = cfg.outer.k
                        isMatch = abs (kOuter - k4pos) < epsilon || abs (kOuter - k4neg) < epsilon
                    in
                    Expect.equal True isMatch
            ]

        , describe "Symmetric config tangency"
            [ test "c1 and c2 are externally tangent" <|
                \_ ->
                    let cfg = symmetricConfig
                    in Expect.equal True (isExternallyTangent cfg.c1 cfg.c2)

            , test "c1 and c3 are externally tangent" <|
                \_ ->
                    let cfg = symmetricConfig
                    in Expect.equal True (isExternallyTangent cfg.c1 cfg.c3)

            , test "c2 and c3 are externally tangent" <|
                \_ ->
                    let cfg = symmetricConfig
                    in Expect.equal True (isExternallyTangent cfg.c2 cfg.c3)

            , test "outer and c1 are internally tangent" <|
                \_ ->
                    let cfg = symmetricConfig
                    in Expect.equal True (isInternallyTangent cfg.outer cfg.c1)

            , test "outer and c2 are internally tangent" <|
                \_ ->
                    let cfg = symmetricConfig
                    in Expect.equal True (isInternallyTangent cfg.outer cfg.c2)

            , test "outer and c3 are internally tangent" <|
                \_ ->
                    let cfg = symmetricConfig
                    in Expect.equal True (isInternallyTangent cfg.outer cfg.c3)
            ]

        , describe "buildInitialConfig normalization"
            [ test "equal radii produces same curvature as symmetric config" <|
                \_ ->
                    let
                        r = 0.25
                        result = buildInitialConfig ( r, r, r )
                        expectedK = symmetricConfig.c1.k
                    in
                    case result of
                        Nothing ->
                            Expect.fail "buildInitialConfig returned Nothing"

                        Just cfg ->
                            Expect.within (Expect.Absolute 0.001) expectedK cfg.c1.k

            , test "c1 and c2 are externally tangent" <|
                \_ ->
                    case buildInitialConfig ( 0.2, 0.25, 0.3 ) of
                        Nothing ->
                            Expect.fail "buildInitialConfig returned Nothing"

                        Just cfg ->
                            Expect.equal True (isExternallyTangent cfg.c1 cfg.c2)

            , test "c1 and c3 are externally tangent" <|
                \_ ->
                    case buildInitialConfig ( 0.2, 0.25, 0.3 ) of
                        Nothing ->
                            Expect.fail "buildInitialConfig returned Nothing"

                        Just cfg ->
                            Expect.equal True (isExternallyTangent cfg.c1 cfg.c3)

            , test "c2 and c3 are externally tangent" <|
                \_ ->
                    case buildInitialConfig ( 0.2, 0.25, 0.3 ) of
                        Nothing ->
                            Expect.fail "buildInitialConfig returned Nothing"

                        Just cfg ->
                            Expect.equal True (isExternallyTangent cfg.c2 cfg.c3)

            , test "outer and c1 are internally tangent" <|
                \_ ->
                    case buildInitialConfig ( 0.2, 0.25, 0.3 ) of
                        Nothing ->
                            Expect.fail "buildInitialConfig returned Nothing"

                        Just cfg ->
                            Expect.equal True (isInternallyTangent cfg.outer cfg.c1)

            , test "outer has radius 1 (k = -1)" <|
                \_ ->
                    case buildInitialConfig ( 0.2, 0.25, 0.3 ) of
                        Nothing ->
                            Expect.fail "buildInitialConfig returned Nothing"

                        Just cfg ->
                            Expect.within (Expect.Absolute epsilon) -1.0 cfg.outer.k
            ]

        , describe "inverseCircle tangency"
            [ test "central circle is tangent to all three inner parents" <|
                \_ ->
                    let
                        cfg = symmetricConfig
                        result = inverseCircle cfg.c1 cfg.c2 cfg.c3 cfg.outer
                    in
                    case result of
                        Nothing ->
                            Expect.fail "inverseCircle returned Nothing"

                        Just newC ->
                            Expect.all
                                [ \_ -> Expect.equal True (isExternallyTangent newC cfg.c1)
                                , \_ -> Expect.equal True (isExternallyTangent newC cfg.c2)
                                , \_ -> Expect.equal True (isExternallyTangent newC cfg.c3)
                                ]
                                ()

            , test "interstice circle is tangent to two inner and outer" <|
                \_ ->
                    let
                        cfg = symmetricConfig
                        result = inverseCircle cfg.c1 cfg.c2 cfg.outer cfg.c3
                    in
                    case result of
                        Nothing ->
                            Expect.fail "inverseCircle returned Nothing"

                        Just newC ->
                            Expect.all
                                [ \_ -> Expect.equal True (isExternallyTangent newC cfg.c1)
                                , \_ -> Expect.equal True (isExternallyTangent newC cfg.c2)
                                , \_ -> Expect.equal True (isInternallyTangent newC cfg.outer)
                                ]
                                ()
            ]

        , describe "Depth-2 gasket"
            [ test "all depth-1 circles are tangent to their three parents" <|
                \_ ->
                    let
                        cfg = symmetricConfig

                        checkTangencies maybeC parents =
                            case maybeC of
                                Nothing -> False
                                Just c ->
                                    List.all
                                        (\( parent, isInternal ) ->
                                            if isInternal then isInternallyTangent c parent
                                            else isExternallyTangent c parent
                                        )
                                        parents
                    in
                    Expect.all
                        [ \_ ->
                            Expect.equal True
                                (checkTangencies (inverseCircle cfg.c1 cfg.c2 cfg.c3 cfg.outer)
                                    [ ( cfg.c1, False ), ( cfg.c2, False ), ( cfg.c3, False ) ])
                        , \_ ->
                            Expect.equal True
                                (checkTangencies (inverseCircle cfg.c1 cfg.c2 cfg.outer cfg.c3)
                                    [ ( cfg.c1, False ), ( cfg.c2, False ), ( cfg.outer, True ) ])
                        , \_ ->
                            Expect.equal True
                                (checkTangencies (inverseCircle cfg.c1 cfg.c3 cfg.outer cfg.c2)
                                    [ ( cfg.c1, False ), ( cfg.c3, False ), ( cfg.outer, True ) ])
                        , \_ ->
                            Expect.equal True
                                (checkTangencies (inverseCircle cfg.c2 cfg.c3 cfg.outer cfg.c1)
                                    [ ( cfg.c2, False ), ( cfg.c3, False ), ( cfg.outer, True ) ])
                        ]
                        ()

            , test "gasket generates more than 1000 circles" <|
                \_ ->
                    let
                        cfg = symmetricConfig
                        circles = generateGasket cfg.outer cfg.c1 cfg.c2 cfg.c3
                    in
                    Expect.greaterThan 1000 (List.length circles)
            ]
        ]
