module Bowling exposing (ScoreEntries, ScoreEntry(..), getScore)


type ScoreEntry
    = Pins Int
    | Spare
    | Strike


type alias ScoreEntries =
    List ScoreEntry


type alias FrameNumber =
    Int


getScore : ScoreEntries -> Int
getScore =
    calculateScore 1


calculateScore : FrameNumber -> ScoreEntries -> Int
calculateScore frameNb entries =
    let
        scoreNextFrame =
            calculateScore (frameNb + 1)
    in
    case ( frameNb, entries ) of
        ( 11, _ ) ->
            0

        ( _, Strike :: Strike :: Strike :: rest ) ->
            30 + scoreNextFrame (Strike :: Strike :: rest)

        ( _, Strike :: ((Pins _) as pins) :: Spare :: rest ) ->
            20 + scoreNextFrame (pins :: Spare :: rest)

        ( _, Strike :: Strike :: ((Pins nbPins) as pins) :: rest ) ->
            20 + nbPins + scoreNextFrame (Strike :: pins :: rest)

        ( _, Strike :: ((Pins x) as px) :: ((Pins y) as py) :: rest ) ->
            10 + x + y + scoreNextFrame (px :: py :: rest)

        ( _, (Pins _) :: Spare :: ((Pins x) as pins) :: rest ) ->
            10 + x + scoreNextFrame (pins :: rest)

        ( _, (Pins _) :: Spare :: Strike :: rest ) ->
            20 + scoreNextFrame (Strike :: rest)

        ( _, (Pins x) :: (Pins y) :: rest ) ->
            x + y + scoreNextFrame rest

        _ ->
            -1000
