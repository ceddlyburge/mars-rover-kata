module Rover exposing (..)

import List
import List.Extra
import Parser exposing (Parser, (|=), (|.))
import String

type alias Inputs = {
    mars: Mars,
    robots: List Robot
    }

type alias Mars = {
    right: Int,
    upper: Int
    }

type alias Location = {
    x: Int,
    y: Int
    }

type Orientation =
    North
    | East
    | South
    | West

type alias Robot = {
    initialPosition: RobotPosition,
    instructions: List RobotInstruction
    }

type RobotPosition = 
    Known Location Orientation
    | Lost Location Orientation

type RobotInstruction = 
    Forward
    | RotateLeft
    | RotateRight

rove : String -> String
rove inputs =
    """1 1 E

3 3 N LOST
2 3 S"""

-- The mapAccuml is quite complicated, might be better to pattern match the list
-- This would also allow the introduction of a type to represent the return value
-- from the function (a final position and a list of scents)
updateRobots : Mars -> List Robot -> List RobotPosition
updateRobots mars robots =
    let
        scents = []
        scentsAndFinalPositions = 
            List.Extra.mapAccuml 
                (updateRobotAndScents mars)
                scents 
                robots
    in
        Tuple.second scentsAndFinalPositions

updateRobotAndScents : Mars -> List Location -> Robot -> (List Location, RobotPosition)
updateRobotAndScents mars scents robot =
    let
        robotFinalPosition = updateRobot mars scents robot.initialPosition robot.instructions
    in
        (updateScents robotFinalPosition scents, robotFinalPosition)


updateRobot : Mars -> List Location -> RobotPosition -> List RobotInstruction -> RobotPosition
updateRobot mars scents robotInitialPosition robotInstructions =
    List.foldl (updateRobotPosition mars scents) robotInitialPosition robotInstructions

updateScents : RobotPosition -> List Location -> List Location
updateScents robotFinalPosition scents = 
    case robotFinalPosition of 
        Known _ _ ->
            scents
        Lost location _ ->
            location :: scents

updateRobotPosition : Mars -> List Location -> RobotInstruction -> RobotPosition -> RobotPosition
updateRobotPosition mars scents robotInstruction robotPosition =
    case robotPosition of
        Known location orientation ->
            updateKnownRobotPosition mars location orientation robotInstruction scents
        Lost location orientation ->
            Lost location orientation


updateKnownRobotPosition : Mars -> Location -> Orientation -> RobotInstruction -> List Location -> RobotPosition
updateKnownRobotPosition mars location orientation robotInstruction scents =
    case robotInstruction of
        RotateLeft ->
            Known location (rotateLeft orientation)
        RotateRight ->
            Known location (rotateRight orientation)
        Forward ->
            forward mars location orientation scents


rotateLeft : Orientation -> Orientation
rotateLeft orientation =
    case orientation of
        North ->
            West
        East ->
            North
        South ->
            East
        West ->
            South

rotateRight : Orientation -> Orientation
rotateRight orientation =
    case orientation of
        North ->
            East
        East ->
            South
        South ->
            West
        West ->
            North

forward : Mars -> Location -> Orientation -> List Location -> RobotPosition
forward mars location orientation scents =
    let 
        robotProvisionalLocation = provisionalLocation location orientation
    in
        if isLocationLost mars robotProvisionalLocation then
            if isLocationScented location scents then
                Known location orientation
            else 
                Lost location orientation
        else
            Known robotProvisionalLocation orientation

isLocationLost : Mars -> Location -> Bool
isLocationLost mars location =
    location.x < 0 
    || location.y < 0
    || location.x > mars.right -- might be >=
    || location.y > mars.upper -- might be >=

isLocationScented : Location -> List Location -> Bool
isLocationScented location scents =
    List.member location scents

provisionalLocation : Location -> Orientation -> Location
provisionalLocation location orientation =
    case orientation of
        North ->
            { location | y = location.y + 1 }
        East ->
            { location | x = location.x + 1 }
        South ->
            { location | y = location.y - 1 }
        West ->
            { location | x = location.x - 1 }

inputsParser : Parser Inputs
inputsParser = 
    Parser.succeed Inputs
        |= marsParser
        |. Parser.spaces
        |= robotsParser


robotsParser : Parser (List Robot)
robotsParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = Parser.spaces
        , item = robotParser
        , trailing = Parser.Optional
        }

robotParser : Parser Robot
robotParser =
    Parser.succeed Robot
        |= robotPositionParser
        |. Parser.spaces
        |= robotInstructionsParser


marsParser : Parser Mars
marsParser =
    Parser.succeed Mars
        |= Parser.int
        |. Parser.spaces
        |= Parser.int

robotPositionParser : Parser RobotPosition
robotPositionParser =
    Parser.succeed Known
        |= locationParser
        |. Parser.spaces
        |= orientationParser

locationParser : Parser Location
locationParser =
    Parser.succeed Location
        |= Parser.int
        |. Parser.spaces
        |= Parser.int

orientationParser : Parser Orientation
orientationParser =
    Parser.oneOf
        [ Parser.map (always North) (Parser.keyword "N")
        , Parser.map (always East) (Parser.keyword "E")
        , Parser.map (always South) (Parser.keyword "S")
        , Parser.map (always West) (Parser.keyword "W")
        ]

robotInstructionsParser : Parser (List RobotInstruction)
robotInstructionsParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = (Parser.chompWhile (\c -> False))
        , item = robotInstructionParser
        , trailing = Parser.Optional
        }

robotInstructionParser : Parser RobotInstruction
robotInstructionParser =
    Parser.oneOf
        [ Parser.map (always Forward) (Parser.token "F")
        , Parser.map (always RotateLeft) (Parser.token "L")
        , Parser.map (always RotateRight) (Parser.token "R")
        ]

outputRobotPosition : RobotPosition -> String
outputRobotPosition robotPosition = 
    case robotPosition of
        Known location orientation ->
            String.fromInt(location.x) ++ " " ++ String.fromInt(location.y) ++ " " ++ outputOrientation(orientation)
        Lost location orientation ->
            ""

outputOrientation : Orientation -> String
outputOrientation orientation = 
    case orientation of
        North ->
            "N"
        East ->
            "E"
        South ->
            "S"
        West ->
            "W"
