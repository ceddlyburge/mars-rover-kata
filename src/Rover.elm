module Rover exposing (..)

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

-- this will need to take a parameter to represent mars later (to check if robots become lost)
updateKnownRobotPosition : Location -> Orientation -> RobotInstruction -> RobotPosition
updateKnownRobotPosition location orientation robotInstruction=
    case robotInstruction of
        RotateLeft ->
            Known location (rotateLeft orientation)
        RotateRight ->
            Known location (rotateRight orientation)
        Forward ->
            forward location orientation


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

forward : Location -> Orientation -> RobotPosition
forward location orientation =
    let 
        robotProvisionalLocation = provisionalLocation location orientation
    in
        if (robotProvisionalLocation.x < 0 || robotProvisionalLocation.y < 0) then
            Lost location
        else
            Known robotProvisionalLocation orientation

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
