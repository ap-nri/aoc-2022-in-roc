app "day3"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.File, pf.Task, pf.Path, Json]
    provides [main] to pf

splitListInHalf = \list ->
    List.split list ((List.len list) // 2)

priority : Int Unsigned32 -> Int Unsigned32
priority = \num -> 
    if num > 96 then
        num - 96 
    else
        num - 64 + 26

priorityOfCommonElementInHalves : Str -> Result (Int Unsigned32) [OutOfBounds]
priorityOfCommonElementInHalves = \line ->
    Str.toScalars line
    |> splitListInHalf
    |> \{ before, others } -> [ before, others ]
    |> priorityOfCommonElement


intersections : List (Set (Int Unsigned32)) -> Set (Int Unsigned32)
intersections = \sets ->
    when sets is
        [] -> Set.empty
        [ first ] -> first
        [ first, second ] -> Set.intersection first second
        _ ->
            { before, others } = List.split sets 2
            Set.intersection
                (intersections before)
                (intersections others)


priorityOfCommonElement :
    List (List (Int Unsigned32))
    -> Result (Int Unsigned32) [OutOfBounds]
priorityOfCommonElement = \lists ->
    List.map lists Set.fromList
        |> intersections
        |> Set.toList
        |> \list -> Result.map (List.get list 0) priority


priorityThreeAtATime :
    List (Str)
    -> Result
        (Int Unsigned32)
        [ NoCommonElement
        , OnlyTwoElements
        , OnlyOneElement
        , MoreThanThreeElements Nat
        ]
priorityThreeAtATime = \l ->
    { before, others } = List.split l 3
    # this is where I'd like to  destructure with first :: second :: third :: rest
    when before is
        [ _, _, _, ] ->
            when List.map before Str.toScalars |> priorityOfCommonElement is
                Ok priorityOfHead ->
                    when (priorityThreeAtATime others) is
                        Ok priorityOfTail ->
                            Ok (priorityOfHead + priorityOfTail)
                        err ->
                            err

                Err OutOfBounds ->
                    Err NoCommonElement

        [] -> Ok 0
        [_] -> Err OnlyOneElement
        [_, _ ] -> Err OnlyTwoElements
        # an impossible state if we have destructuring with a tail/rest
        _ -> Err (MoreThanThreeElements (List.len before))

main =
    content <- "./day3.txt"
        |> Path.fromStr
        |> File.readUtf8
        |> Task.onFail (\err ->
            when err is
                FileReadUtf8Err _path _fileErr -> crash "FileReadUtf8Err"
                FileReadErr _ _ -> crash "FileReadErr: asd"
        )
        |> Task.await

    lines = Str.split content "\n"

    sumOfPriorities : Result (Int Unsigned32) [OutOfBounds]
    sumOfPriorities =
        List.mapTry
            lines
            priorityOfCommonElementInHalves
        |> Result.map List.sum
    
    part1 =
        when sumOfPriorities is
            Ok sp -> Num.toStr sp
            Err OutOfBounds -> "at some point there was a backpack with no common element in two halves"

    threeLinePriorities = 
        when priorityThreeAtATime lines is
            Ok p ->
                Num.toStr p
            Err NoCommonElement -> "No Common Element"
            Err OnlyTwoElements -> "Only Two Elements"
            Err OnlyOneElement -> "Only One Element"
            Err (MoreThanThreeElements n) ->
                ns = Num.toStr n
                "You called List.split l 3 and got \(ns) I'm honestly impressed"

    Stdout.line "part 1 \(part1)\npart 2 \(threeLinePriorities)"


