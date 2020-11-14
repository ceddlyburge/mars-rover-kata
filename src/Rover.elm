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
            Known location orientation
        Forward ->
            Known location orientation


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

