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
    |> toThree
    |> priorityOfCommonElement

toThree : { before : a, others : b} -> { first : a, second : b, third : b}
toThree = \{ before, others } -> { first : before, second : others, third : others }

priorityOfCommonElement :
    { first : List (Int Unsigned32)
    , second : List (Int Unsigned32)
    , third : List (Int Unsigned32)}
    -> (Int Unsigned32)
priorityOfCommonElement = \{first, second, third} ->
    Set.intersection
        (Set.fromList first)
        (Set.fromList second)
    |> Set.intersection (Set.fromList third)
    |> Set.toList
    |> gimme
    |> priority

gimme : List a -> a
gimme = \l ->
    when l is
        [x] -> x
        _ -> crash "gimme can fail" 

priorityThreeAtATime : List (Str) -> Int Unsigned32
priorityThreeAtATime = \l ->
    { before, others } = List.split l (3)
    # this is where I'd like to  destructure with first :: second :: third :: rest
    when before is
        [ first, second, third ] ->
            (priorityOfCommonElement 
                { first : Str.toScalars first
                , second : Str.toScalars second
                , third : Str.toScalars third
                }
            ) + (priorityThreeAtATime others)

        _ -> 0

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
            (priorityOfCommonElementInHalves)
        |> List.sum
        |> Num.toStr

    threeLinePriorities = priorityThreeAtATime lines |> Num.toStr

    Stdout.line "part 1 \(sumOfPriorities)\npart 2 \(threeLinePriorities)"


