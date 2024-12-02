let sample =
    ["7 6 4 2 1";
    "1 2 7 8 9";
    "9 7 6 2 1";
    "1 3 2 4 5";
    "8 6 4 4 1";
    "1 3 6 7 9"]
let lines = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input.txt"
let readReport (line:string) = line.Split(" ") |> Array.map int
let reports = lines |> Array.map readReport
type RawReport = int array
type SafeReport = RawReport option
//mapi over reports with function that compares the element with the preceding one (except where i = 0) and returns true if the elements are not equal and are not +- 4 or more
let isSafePair elem i (arr: RawReport) =
    if i = 0 then
        true
    else
        let succMinusPrec = elem - arr.[i - 1]
        if arr.[0] > arr.[1] then
            succMinusPrec > -4 && succMinusPrec < 0
        else
            succMinusPrec < 4 && succMinusPrec > 0

let test1 = [|10|]
isSafePair 10 0 test1
isSafePair 5 1 test1
isSafePair 6 1 test1
isSafePair 7 1 test1
isSafePair 8 1 test1
isSafePair 9 1 test1
isSafePair 10 1 test1
isSafePair 11 1 test1
isSafePair 12 1 test1
isSafePair 13 1 test1
isSafePair 14 1 test1

let isSafe report = report |> Array.mapi (fun i e -> isSafePair e i report) |> Array.forall id
isSafe reports.[0]
isSafe reports.[1]

let rawToSafeReport report =
    if isSafe report then
        Some report
    else
        None

sample |> List.toArray |> Array.map readReport |> Array.choose rawToSafeReport

let safeReports =
    reports |> Array.choose rawToSafeReport

let count = safeReports |> Array.length
//val count: int = 559

(*
For part 2, we will reuse isSafePair but we can't simply map over the report
.... actually for deconstruction it's easier to switch to lists, so we have a llist version instead of an array version

We need a 'dampened safe' function to check ecah pair for safety but allow skipping the first element that isn't safe
Our ascending check should skip should compare elements 0 and 2 if 0 and 1 aren't a safe pair
*)
type Direction =
    | Unknown
    | Ascending
    | Descending

let dampenedSafe (report: RawReport) =
    let report = Array.toList report
    let isPairSafe a b direction =
        let dir =
            if direction = Unknown then
                if b > a then
                    Ascending
                elif a > b then
                    Descending
                else
                    Unknown
            else
                direction
        let diff = 
            match dir with
            | Unknown -> 0
            | Ascending -> b - a
            | Descending -> a - b
        (diff < 4 && diff > 0, dir)
    let rec f rep dampened direction =
        match rep with
        | [] -> false //should never have an empty list in this function
        | [_] -> false //should never have a single element list in this function
        | [_;_] when not dampened -> true //The last pair doesn't matter if there has not been any dampening
        | [prev;this] -> fst (isPairSafe prev this direction) //If there has been dampening, however, then the result depends on the safety of the last pair
        | prev :: this :: rest ->
            match isPairSafe prev this direction with
            | (true, dir) -> f (this :: rest) dampened dir //If this pair is safe, check the next
            | (false, dir) when not dampened -> f (prev :: rest) true dir //If this pair is unsafe but we can dampen it, do that, and check the pair formed by skipping this one
            | (false, dir) -> false //Any errors and it's not safe, because weve already applied dampening
    if f report false Unknown then
        Ok report
    else
        Error report

//Test against the sample
sample |> List.toArray |> Array.map readReport |> Array.map dampenedSafe
//Everything matches, awesome!


let dampenedSafeReports =
    reports |> Array.map dampenedSafe

dampenedSafeReports |> Array.filter (fun r -> r.IsOk) |> Array.length
//val it: int = 588
//That's not the right answer; your answer is too low.
//Booooo!
//Our strategy above was to ignore the second element of a pair that's an error. But we might have better results in some cases if we ignore the first element of a pair that's an error

(*
Part 2, take 2
For each report, build a list of reports:
* the original
* with the zeroth element omitted
* with the first element omitted...

Check each version in the list. Return true if at least one results in a safeReport (maybe the use the logic from part 1)
*)

let generateCandidateReports original =
    [original] @ [for i in 0..(List.length original - 1) -> List.removeAt i original]

generateCandidateReports [1;2;3;4;5]
(*
val it: int list list =
  [[1; 2; 3; 4; 5]; [2; 3; 4; 5]; [1; 3; 4; 5]; [1; 2; 4; 5]; [1; 2; 3; 5];
   [1; 2; 3; 4]]
Looks good!
*)

let rec goodCandidate candidates =
    match candidates with
    | [] -> None
    | [rep] -> rep |> List.toArray |> rawToSafeReport
    | rep :: rest ->
        let result = rep |> List.toArray |> rawToSafeReport
        match result with
        | Some x -> Some x
        | None -> goodCandidate rest

reports |> Array.map (fun rep -> rep |> Array.toList |> generateCandidateReports |> goodCandidate) |> Array.choose id |> Array.length
//val it: int = 601

(*

That's the right answer! You are one gold star closer to finding the Chief Historian.


Let's see which ones gave us problems
*)

let exhaustiveSafeReports = reports |> Array.map (fun rep -> rep |> Array.toList |> generateCandidateReports |> goodCandidate)

(exhaustiveSafeReports, dampenedSafeReports) ||> 
Array.map2 (fun a b ->
    match (a, b) with
    | (Some _, Error b) -> Some b
    | _ -> None
) |> Array.choose id

(*
val it: int list array =
  [|
    [67; 65; 68; 71; 72; 73; 74]; Case 1: Valid first pair makes the ordering appear to be descending, but it's ascending if we remove 65
    [10; 9; 13; 14; 17; 20]; Case 1
    [16; 20; 22; 24; 27; 30; 31; 32]; Case 2: Must remove 16 to get a valid sequence
    [21; 28; 30; 33; 35; 36]; Case 2
    [97; 99; 96; 93; 92; 91; 88]; Case 1, inverted
    [99; 95; 92; 89; 86; 84]; Case 2
    [85; 79; 78; 76; 74]; Case 2
    [77; 81; 82; 83; 85; 87; 88]; Case 2
    [43; 38; 35; 32; 31]; Case 2
    [81; 87; 88; 89; 90]; Case 2
    [62; 65; 61; 58; 57; 56; 53; 51]; Case 1, inverted
    [28; 30; 27; 25; 22; 19; 16]; Case 1, inverted
    [46; 43; 44; 43; 42] Case 3: must remove the first 43 so we don't go back up to 44, or repeat 43
  |]

Mostly the errors in our original were because we always treated the first element as valid,
There was also a common pattern where the heuristic of the using the first and 2nd/3rd element to determine ascending/descending trips us up because
although a valid pair in itself it's counter to the direction of every other valid pairing

The last case was less classifiable than these. Generating all candidates and testing each until we get a match is important to discover that there is a valid report in the data.
It's simpler to do it this way than try to implement backtracking
*)