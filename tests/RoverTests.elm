module RoverTests exposing (tests)

import Parser

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, string)

import Rover exposing (..)


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
                |> Expect.equal (Lost (Location 1 0) South)

        , test "moving to x > mars right results in robot becoming lost, with last known position retained" <|
            \() ->
                updateKnownRobotPosition (Mars 1 3) (Location 1 2) East Forward noScents
                |> Expect.equal (Lost (Location 1 2) East)

        , test "lost robot remains lost, with last known position retained" <|
            \() ->
                updateRobotPosition anyMars noScents anyRobotInstruction (Lost anyLocation anyOrientation)
                |> Expect.equal (Lost anyLocation anyOrientation)

        , test "scent prevents robot moving to y < 0 lost position" <|
            \() ->
                updateKnownRobotPosition bigMars (Location 1 0) South Forward [ Location 1 0 ]
                |> Expect.equal (Known (Location 1 0) South)

        , test "robot 1 sample case (remains known)" <|
            \() ->
                updateRobot 
                    sampleMars 
                    noScents
                    firstSampleRobotInitialPosition 
                    firstSampleRobotInstructions
                |> Expect.equal firstSampleRobotFinalPosition

        , test "robot 2 sample case (becomes lost)" <|
            \() ->
                updateRobot 
                    sampleMars 
                    noScents
                    secondSampleRobotInitialPosition 
                    secondSampleRobotInstructions
                |> Expect.equal secondSampleRobotFinalPosition

        , test "robot 3 sample case (prevented from becoming lost by a scent)" <|
            \() ->
                updateRobot 
                    sampleMars 
                    [ Location 3 3 ]
                    thirdSampleRobotInitialPosition 
                    thirdSampleRobotInstructions
                |> Expect.equal thirdSampleRobotFinalPosition
        , test "robot 3 sample case (prevented from becoming lost by a scent from sample robot 2)" <|
            \() ->
                updateRobots 
                    sampleMars 
                    [ Robot secondSampleRobotInitialPosition secondSampleRobotInstructions
                    , Robot thirdSampleRobotInitialPosition thirdSampleRobotInstructions
                    ]
                |> Expect.equal 
                    [ secondSampleRobotFinalPosition
                    , thirdSampleRobotFinalPosition
                    ]

        , test "sample case" <|
            \() ->
                updateRobots 
                    sampleMars 
                    [ Robot firstSampleRobotInitialPosition firstSampleRobotInstructions
                    , Robot secondSampleRobotInitialPosition secondSampleRobotInstructions
                    , Robot thirdSampleRobotInitialPosition thirdSampleRobotInstructions
                    ]
                |> Expect.equal 
                    [ firstSampleRobotFinalPosition
                    , secondSampleRobotFinalPosition
                    , thirdSampleRobotFinalPosition
                    ]

        , test "parse Mars" <|
            \() ->
                Parser.run marsParser "5 3"
                |> Expect.equal 
                    (Ok <| Mars 5 3)

        , test "parse initial RobotPosition" <|
            \() ->
                Parser.run robotPositionParser "1 1 E"
                |> Expect.equal 
                    (Ok ( Known (Location 1 1) East) )

        , test "parse RobotInstructions" <|
            \() ->
                Parser.run robotInstructionsParser "LRF"
                |> Expect.equal 
                    (Ok [ RotateLeft, RotateRight, Forward ])

        , test "parse Robot" <|
            \() ->
                Parser.run robotParser """1 1 E
R"""
                |> Expect.equal 
                    (Ok ( Robot (Known (Location 1 1) East) [ RotateRight ] ))

        , test "parse Robots" <|
            \() ->
                Parser.run robotsParser """1 1 E
R
3 2 N
F"""
                |> Expect.equal 
                    (Ok 
                        [ Robot (Known (Location 1 1) East) [ RotateRight ] 
                        , Robot (Known (Location 3 2) North) [ Forward ] 
                        ]
                    )

        , test "output known robot final position" <|
            \() ->
                outputRobotPosition (Known (Location 1 2) North) 
                |> Expect.equal 
                    "1 2 N"

        , test "output lost robot final position" <|
            \() ->
                outputRobotPosition (Lost (Location 3 3) North) 
                |> Expect.equal 
                    "3 3 N LOST"

        , test "parse mars rover inputs" <|
            \() ->
                Parser.run inputsParser """5 3
1 1 E
RFRFRFRF
3 2 N
FRRFLLFFRRFLL
0 3 W
LLFFFLFLFL"""
                |> Expect.equal 
                    (Ok 
                        (
                            Inputs
                                sampleMars
                                [ Robot firstSampleRobotInitialPosition firstSampleRobotInstructions
                                , Robot secondSampleRobotInitialPosition secondSampleRobotInstructions
                                , Robot thirdSampleRobotInitialPosition thirdSampleRobotInstructions
                                ]

                        )
                    )

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
2 3 S"""
        ]
            
sampleMars: Mars
sampleMars = 
    Mars 5 3

bigMars: Mars
bigMars = 
    Mars 100 100
        
anyMars: Mars
anyMars = 
    Mars 1 1

firstSampleRobotInitialPosition : RobotPosition 
firstSampleRobotInitialPosition =
    Known (Location 1 1) East

secondSampleRobotInitialPosition : RobotPosition 
secondSampleRobotInitialPosition =
    Known (Location 3 2) North

thirdSampleRobotInitialPosition : RobotPosition 
thirdSampleRobotInitialPosition =
    Known (Location 0 3) West

firstSampleRobotInstructions : List RobotInstruction
firstSampleRobotInstructions = 
    [ RotateRight
    , Forward
    , RotateRight
    , Forward
    , RotateRight
    , Forward
    , RotateRight
    , Forward
    ]

secondSampleRobotInstructions : List RobotInstruction
secondSampleRobotInstructions = 
    [ Forward
    , RotateRight
    , RotateRight
    , Forward
    , RotateLeft
    , RotateLeft
    , Forward
    , Forward
    , RotateRight
    , RotateRight
    , Forward
    , RotateLeft
    , RotateLeft
    ]



thirdSampleRobotInstructions : List RobotInstruction
thirdSampleRobotInstructions = 
    [ RotateLeft
    , RotateLeft
    , Forward
    , Forward
    , Forward
    , RotateLeft
    , Forward
    , RotateLeft
    , Forward
    , RotateLeft
    ]


firstSampleRobotFinalPosition : RobotPosition 
firstSampleRobotFinalPosition = 
    Known (Location 1 1) East

secondSampleRobotFinalPosition : RobotPosition 
secondSampleRobotFinalPosition = 
    Lost (Location 3 3) North

thirdSampleRobotFinalPosition : RobotPosition 
thirdSampleRobotFinalPosition = 
    Known (Location 2 3) South

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