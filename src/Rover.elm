module Rover exposing (..)

import List
import List.Extra

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
2 3 S """

updateRobots : Mars -> List Robot -> List RobotPosition
updateRobots mars robots =
    let
        scents = []
        scentsAndFinalPositions = 
            List.Extra.mapAccuml 
                (\scentss robot -> ([], updateRobot mars scentss robot.initialPosition robot.instructions)) 
                scents 
                robots
    in
        Tuple.second scentsAndFinalPositions
    

updateRobot : Mars -> List Location -> RobotPosition -> List RobotInstruction -> RobotPosition
updateRobot mars scents robotInitialPosition robotInstructions =
    List.foldl (updateRobotPosition mars scents) robotInitialPosition robotInstructions

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
