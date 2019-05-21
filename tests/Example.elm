module Example exposing (suite)

import Bowling exposing (..)
import Expect exposing (Expectation, atLeast, atMost)
import Fuzz exposing (Fuzzer, constant, int, intRange, list, string)
import Test exposing (..)


traverse : (a -> Fuzzer b) -> List a -> Fuzzer (List b)
traverse fn list =
    case list of
        [] ->
            Fuzz.constant []

        x :: xs ->
            Fuzz.map2 (::) (fn x) (traverse fn xs)


onlyRandomSpares : Fuzzer ScoreEntries
onlyRandomSpares =
    List.repeat 11 9
        |> traverse (\max -> intRange 0 max)
        |> Fuzz.map (List.map Pins >> List.intersperse Spare)


fuzzOpenFrame : Int -> Int -> ( Fuzzer Int, Fuzzer Int )
fuzzOpenFrame maxFramePins maxPinsFirstThrow =
    ( Fuzz.intRange 0 maxPinsFirstThrow, Fuzz.intRange 0 (maxFramePins - maxPinsFirstThrow) )


toTuppleScoreEntry : ( Int, Int ) -> ( ScoreEntry, ScoreEntry )
toTuppleScoreEntry =
    Tuple.mapBoth Pins Pins


randomOpenThrows : Int -> Fuzzer ( ScoreEntry, ScoreEntry )
randomOpenThrows maxPins =
    List.range 0 maxPins
        |> List.map (fuzzOpenFrame maxPins >> Fuzz.tuple >> Fuzz.map toTuppleScoreEntry)
        |> Fuzz.oneOf


generateOpenFrames : Int -> Fuzzer ScoreEntries
generateOpenFrames frameCount =
    List.repeat frameCount 9
        |> traverse randomOpenThrows
        |> Fuzz.map (List.concatMap (\( x, y ) -> [ x, y ]))


onlyOpenFrames : Fuzzer ScoreEntries
onlyOpenFrames =
    generateOpenFrames 10


intersperseEvery2 : a -> List a -> List a
intersperseEvery2 elem list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        [ x, y ] ->
            [ x, y ]

        x :: y :: rest ->
            x :: y :: elem :: intersperseEvery2 elem rest


intersperseEvery3 : a -> List a -> List a
intersperseEvery3 elem list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        [ x, y ] ->
            [ x, y ]

        [ x, y, z ] ->
            [ x, y, z ]

        x :: y :: z :: rest ->
            x :: y :: z :: elem :: intersperseEvery3 elem rest


alternOpenStrikeFrames : Fuzzer ScoreEntries
alternOpenStrikeFrames =
    generateOpenFrames 6
        |> Fuzz.map (intersperseEvery2 Strike)


alternDoubleStrikeOpenFrames : Fuzzer ScoreEntries
alternDoubleStrikeOpenFrames =
    generateOpenFrames 4
        |> Fuzz.map (intersperseEvery2 Strike >> intersperseEvery3 Strike)


alternSpareStrikeFrames : Fuzzer ScoreEntries
alternSpareStrikeFrames =
    generateOpenFrames 6
        |> Fuzz.map (List.concatMap (\entry -> [ entry, Spare ]))
        |> Fuzz.map (intersperseEvery2 Strike)


pinsFromScoreEntry : ScoreEntry -> Maybe Int
pinsFromScoreEntry entry =
    case entry of
        Pins x ->
            Just x

        _ ->
            Nothing


pinsFromScoreEntries : ScoreEntries -> List Int
pinsFromScoreEntries =
    List.filterMap pinsFromScoreEntry


sumPinsScoreEntries : ScoreEntries -> Int
sumPinsScoreEntries =
    pinsFromScoreEntries >> List.sum


sumPinsScoreEntriesWithSkippedThrowsAtBothEnds : Int -> ScoreEntries -> Int
sumPinsScoreEntriesWithSkippedThrowsAtBothEnds skipCount entries =
    let
        totalThrows =
            List.length entries
    in
    entries
        |> List.drop skipCount
        |> List.take (totalThrows - 2 * skipCount)
        |> pinsFromScoreEntries
        |> List.sum


entryFromChar : Char -> ScoreEntry
entryFromChar c =
    case c of
        '0' ->
            Pins 0

        '1' ->
            Pins 1

        '2' ->
            Pins 2

        '3' ->
            Pins 3

        '4' ->
            Pins 4

        '5' ->
            Pins 5

        '6' ->
            Pins 6

        '7' ->
            Pins 7

        '8' ->
            Pins 8

        '9' ->
            Pins 9

        '/' ->
            Spare

        'X' ->
            Strike

        _ ->
            Pins 0


cardFromChars : List Char -> ScoreEntries
cardFromChars =
    List.map entryFromChar


cardFromString : String -> ScoreEntries
cardFromString =
    String.toList >> cardFromChars


scoreFromString : String -> Int
scoreFromString =
    cardFromString >> getScore


suite : Test
suite =
    describe "The Bowling module"
        [ describe "Throwing strikes"
            -- Nest as many descriptions as you like.
            [ test "should yeild a score of 300 for 12 strikes" <|
                \_ ->
                    Expect.equal 300 (scoreFromString "XXXXXXXXXXXX")
            , fuzz alternOpenStrikeFrames "should yield the following score when throwing open and strike frames alternatively: 5*10 + frame1 + 2*(sum of all but last remaining open frames)" <|
                \scoreCard ->
                    let
                        score =
                            getScore scoreCard
                    in
                    Expect.equal (5 * 10 + sumPinsScoreEntries scoreCard + sumPinsScoreEntriesWithSkippedThrowsAtBothEnds 2 scoreCard) score
            , fuzz alternDoubleStrikeOpenFrames "should yield the following score when always throwing 2 strikes after an open frame: 90 + all knocked down pins with applied multiplicator 1,1,3,2,3,2,3,2" <|
                \scoreCard ->
                    let
                        score =
                            getScore scoreCard

                        scoreMult =
                            [ 1, 1, 3, 2, 3, 2, 3, 2 ]

                        expectedPinsScore =
                            pinsFromScoreEntries scoreCard
                                |> List.map2 (*) scoreMult
                                |> List.sum

                        expectedScore =
                            90 + expectedPinsScore
                    in
                    Expect.equal expectedScore score
            , fuzz alternSpareStrikeFrames "should yield 200 points when alterning spares and strikes" <|
                (getScore >> Expect.equal 200)
            ]
        , describe "Throwing only spares"
            [ fuzz onlyRandomSpares "should yield a score that is 100 + the sum of all non spare symbols" <|
                \scoreCard ->
                    let
                        score =
                            getScore scoreCard

                        --We have to drop the first non-spare symbol here as it is not following
                        --a spare beacause it's the first frame.
                        nonSparePins =
                            List.drop 1 scoreCard
                                |> sumPinsScoreEntries

                        expectedScore =
                            10 * 10 + nonSparePins
                    in
                    Expect.equal expectedScore score
            ]
        , describe "Only open frames"
            [ fuzz onlyOpenFrames "should yield a score that is the sum of all knocked down pins" <|
                \scoreCard ->
                    let
                        score =
                            getScore scoreCard
                    in
                    Expect.equal (sumPinsScoreEntries scoreCard) score
            ]
        , describe "Impossible"
            [ test "should yield -1000 points for impossible input" <|
                \_ ->
                    Expect.equal -1000 <| scoreFromString "0"
            ]
        ]
