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

priorityOfCommonElementInHalves : Str -> (Int Unsigned32)
priorityOfCommonElementInHalves = \line ->
    Str.toScalars line
    |> splitListInHalf
    |> \{ before, others } -> [ before, others ]
    |> priorityOfCommonElement


intersectMany : List (Set (Int Unsigned32)) -> Set (Int Unsigned32)
intersectMany = \sets ->
    when sets is
        [] -> Set.empty
        [ first ] -> first
        [ first, second ] -> Set.intersection first second
        _ ->
            { before, others } = List.split sets 2
            Set.intersection
                (intersectMany before)
                (intersectMany others)


priorityOfCommonElement :
    List (List (Int Unsigned32))
    -> (Int Unsigned32)
priorityOfCommonElement = \sets ->
    List.map sets Set.fromList
        |> intersectMany
        |> Set.toList
        |> gimme
        |> priority

gimme : List (Int Unsigned32) -> (Int Unsigned32)
gimme = \l ->
    when l is
        [] -> crash "empty!"
        [x] -> x
        [x, y, ..] ->
            xs = Num.toStr x
            ys = Num.toStr y
            ls = Num.toStr (List.len l)
            crash "Expected 1 got \(ls) starting with \(xs), \(ys)"

priorityThreeAtATime : List (Str) -> Int Unsigned32
priorityThreeAtATime = \l ->
    { before, others } = List.split l (3)
    # this is where I'd like to  destructure with first :: second :: third :: rest
    when before is
        [ _, _, _, ] ->
            (List.map before Str.toScalars |> priorityOfCommonElement) + (priorityThreeAtATime others)

        [] -> 0
        [_] -> crash "one??"
        [_, _ ] -> crash "two??"
        _ ->
            len = List.len before |> Num.toStr
            crash "how did you get \(len)?"

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

    sumOfPriorities : Str
    sumOfPriorities =
        List.map
            lines
            priorityOfCommonElementInHalves
        |> List.sum
        |> Num.toStr

    threeLinePriorities = priorityThreeAtATime lines |> Num.toStr

    Stdout.line "part 1 \(sumOfPriorities)\npart 2 \(threeLinePriorities)"


