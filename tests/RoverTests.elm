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
-- done (could test / fuzz more cases): update robot position existing position and instruction (ignore scents)
-- done (could test / fuzz more cases): update robot position existing position and instruction (ignore scents), becoming lost (must update scents)
-- done: update robot position and scent from lost position, becoming lost, make sure last known position is retained
-- update robot position and scent from existing position, existing scents and instruction
-- update robot position and scent from existing position, existing scents and instruction, becoming lost, make sure last known position is recorded
-- Aplly list of instructions to a robot, from an initial position
-- parse size of grid in to type (0,0) is assumed
-- parse initial position of robot in to type
-- parse robot instructions in to type
-- output of known position of robot (for final output)
-- output of lost position of robot, including last know position (for final output)

tests : Test
tests =
    describe "rove"
        [ test "rotate left instruction should rotate orienation by -90 degrees (anticlockwise)" <|
            \() ->
                updateKnownRobotPosition bigMars smallLocation North RotateLeft noScents
                |> Expect.equal (Known smallLocation West)
        
        , test "rotate right instruction should rotate orienation by 90 degrees (clockwise)" <|
            \() ->
                updateKnownRobotPosition bigMars smallLocation East RotateRight noScents
                |> Expect.equal (Known smallLocation South)
        
        , test "forward instruction with North should increase y" <|
            \() ->
                updateKnownRobotPosition bigMars smallLocation North Forward noScents
                |> Expect.equal (Known { smallLocation | y = smallLocation.y + 1 } North)
        
        , test "moving to y < 0 results in robot becoming lost, with last known position retained" <|
            \() ->
                updateKnownRobotPosition bigMars (Location 1 0) South Forward noScents
                |> Expect.equal (Lost <| Location 1 0)

        , test "moving to x > mars right results in robot becoming lost, with last known position retained" <|
            \() ->
                updateKnownRobotPosition (Mars 1 3) (Location 1 2) East Forward noScents
                |> Expect.equal (Lost <| Location 1 2)

        , test "lost robot remains lost, with last known position retained" <|
            \() ->
                updateRobotPosition anyMars (Lost anyLocation) anyRobotInstruction noScents
                |> Expect.equal (Lost anyLocation)

        , test "scent prevents robot moving to y < 0 lost position" <|
            \() ->
                updateKnownRobotPosition bigMars (Location 1 0) South Forward [ Location 1 0 ]
                |> Expect.equal (Known (Location 1 0) South)

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
            

bigMars: Mars
bigMars = 
    Mars 100 100
        
anyMars: Mars
anyMars = 
    Mars 1 1

smallLocation: Location
smallLocation = 
    Location 0 0

anyLocation: Location
anyLocation = 
    Location 0 0


anyOrientation: Orientation
anyOrientation = 
    West

anyRobotInstruction: RobotInstruction
anyRobotInstruction = 
    Forward

noScents: List Location
noScents =
    []