module RoverTests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, string)

import Rover exposing (..)


-- ## instructions

-- 2- 3 hours
-- kiss
-- use the sample data
-- do the hard things first
-- commit frequently

-- need to keep last known positions for lost robots (for the output)
-- lost robots leave a scent at their last valid position (maybe the above could meet this demand)
-- north is y + 1
-- east is x + 1
-- The first robot finish all its movement before the second robot starts and so on

-- ## tests
-- slimed: sample data for test (input to output)
-- update robot position existing position and instruction (ignore scents)
-- parse size of grid in to type (0,0) is assumed
-- parse initial position of robot in to type
-- parse robot instructions in to type
-- update robot position existing position and instruction (ignore scents), becoming lost (must update scents)
-- update robot position and scent from existing position, existing scents and instruction
-- update robot position and scent from existing position, existing scents and instruction, becoming lost, make sure last known position is recorded
-- update robot position and scent from lost position, becoming lost, make sure last known position is retained
-- Aplly list of instructions to a robot, from an initial position
-- output of known position of robot (for final output)
-- output of lost position of robot, including last know position (for final output)


tests : Test
tests =
    describe "rove"
        [ test "rotate left instruction should rotate orienation by -90 degrees (anticlockwise)" <|
            \() ->
                updateKnownRobotPosition (Location 1 2) North RotateLeft
                |> Expect.equal (RobotPosition (Location 1 2) West)
            
        , test "sample data should return sample outputs" <|
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
