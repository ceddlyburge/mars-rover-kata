module RoverTests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, string)

import Rover exposing (..)


tests : Test
tests =
    describe "rove"
        [ test "sample data should return sample outputs" <|
            \() ->
                rove """5 3
1 1 E
RFRFRFRF
3 2 N
FRRFLLFFRRFLL
0 3 W
LLFFFLFLFL"""
                |> Expect.equal """1 1 E

3 3 N LOST
2 3 S """
            
        ]
