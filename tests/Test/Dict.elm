module Test.Dict exposing (tests)

import Avl exposing (..)
import Avl.Dict as Dict exposing (Dict)

import Test exposing (..)
import Expect exposing (Expectation)

-- Adapted tests from elm-core: https://github.com/elm-lang/core/blob/master/tests/Test/Dict.elm

strCmp : Cmp String
strCmp = compare

strEq : Eq String
strEq = (==)

animals : Dict String String
animals =
    Dict.fromList strCmp [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]

expectEq : Dict String String -> Dict String String -> Expectation
expectEq x y = Expect.true "expected the dictionaries to be equal" (Dict.eq strEq strEq x y)

tests : Test
tests =
    let
        buildTests =
            describe "build Tests"
                [ test "empty" <| \() -> expectEq (Dict.fromList strCmp []) (Dict.empty)
                , test "singleton" <| \() -> expectEq (Dict.fromList strCmp [ ( "k", "v" ) ]) (Dict.singleton "k" "v")
                , test "insert" <| \() -> expectEq (Dict.fromList strCmp [ ( "k", "v" ) ]) (Dict.insert strCmp "k" "v" Dict.empty)
                , test "insert replace" <| \() -> expectEq (Dict.fromList strCmp [ ( "k", "vv" ) ]) (Dict.insert strCmp "k" "vv" (Dict.singleton "k" "v"))
                , test "update" <| \() -> expectEq (Dict.fromList strCmp [ ( "k", "vv" ) ]) (Dict.update strCmp "k" (\v -> Just "vv") (Dict.singleton "k" "v"))
                , test "update Nothing" <| \() -> expectEq Dict.empty (Dict.update strCmp "k" (\v -> Nothing) (Dict.singleton "k" "v"))
                , test "remove" <| \() -> expectEq Dict.empty (Dict.remove strCmp "k" (Dict.singleton "k" "v"))
                , test "remove not found" <| \() -> expectEq (Dict.singleton "k" "v") (Dict.remove strCmp "kk" (Dict.singleton "k" "v"))
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" <| \() -> Expect.equal True (Dict.member strCmp "Tom" animals)
                , test "member 2" <| \() -> Expect.equal False (Dict.member strCmp "Spike" animals)
                , test "get 1" <| \() -> Expect.equal (Just "cat") (Dict.get strCmp "Tom" animals)
                , test "get 2" <| \() -> Expect.equal Nothing (Dict.get strCmp "Spike" animals)
                , test "size of empty dictionary" <| \() -> Expect.equal 0 (Dict.size Dict.empty)
                , test "size of example dictionary" <| \() -> Expect.equal 2 (Dict.size animals)
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <| \() -> Expect.equal (Dict.singleton "Tom" "cat") (Dict.filter strCmp (\k v -> k == "Tom") animals)
                ]

    in
        describe "Dict Tests"
            [ buildTests
            , queryTests
            , transformTests
            ]
