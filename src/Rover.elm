module Rover exposing (..)

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

type RobotPosition = 
    Known Location Orientation
    | Lost Location

type RobotInstruction = 
    Forward
    | RotateLeft
    | RotateRight

rove : String -> String
rove inputs =
    """1 1 E

3 3 N LOST
2 3 S """

updateRobotPosition : Mars -> RobotPosition -> RobotInstruction -> RobotPosition
updateRobotPosition mars robotPosition robotInstruction =
    case robotPosition of
        Known location orientation ->
            updateKnownRobotPosition mars location orientation robotInstruction
        Lost location ->
            Lost location


updateKnownRobotPosition : Mars -> Location -> Orientation -> RobotInstruction -> RobotPosition
updateKnownRobotPosition mars location orientation robotInstruction =
    case robotInstruction of
        RotateLeft ->
            Known location (rotateLeft orientation)
        RotateRight ->
            Known location (rotateRight orientation)
        Forward ->
            forward mars location orientation


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

forward : Mars -> Location -> Orientation -> RobotPosition
forward mars location orientation =
    let 
        robotProvisionalLocation = provisionalLocation location orientation
    in
        if isLocationLost mars robotProvisionalLocation then
            Lost location
        else
            Known robotProvisionalLocation orientation

isLocationLost : Mars -> Location -> Bool
isLocationLost mars location =
    location.x < 0 
    || location.y < 0
    || location.x > mars.right -- might be >=
    || location.y > mars.upper -- might be >=


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
