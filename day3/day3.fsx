let sample = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
open System.Text.RegularExpressions
let mulParser = Regex "(mul)\(([0-9]+),([0-9]+)\)"
let sampleMatches = sample |> mulParser.Matches

let (|Mul|_|) (instruction: Match) = if instruction.Groups.[1].Value = "mul" then Some (instruction.Groups.[2].Value |> int, instruction.Groups.[3].Value |> int) else None

let inst1 =
    match sampleMatches.[0] with
    | Mul (a, b) -> Some (a * b)
    | _ -> None
//val inst1: int option = Some 8
//Go to go....

let exec inst =
    match inst with
    | Mul (a, b) -> Some (a * b)
    | _ -> None

let sum a b =
    match b with
    | Some x -> a + x
    | None -> a

$"{__SOURCE_DIRECTORY__}\\input.txt"
    |> System.IO.File.ReadAllText
    |> mulParser.Matches
    |> Seq.cast<Match>
    |> Seq.map exec
    |> Seq.fold sum 0
//173785482
//Gold Star!

let instParser = Regex "((do|don't)\(\))|((mul)\([0-9]+,[0-9]+\))"
let (|Do|_|) (instruction: Match) = if instruction.Groups.[2].Value = "do" then Some () else None
let (|Don't|_|) (instruction: Match) = if instruction.Groups.[2].Value = "don't" then Some () else None
let (|Mul2|_|) (instruction: Match) =
    if instruction.Groups.[4].Value = "mul" then
        match (instruction.Value |> mulParser.Match) with
        | Mul inst -> Some inst
        | _ -> None
    else None

type EnableState = |On|Off
type ComputerState = {acc: int; on: EnableState}

let execInstruction state inst =
    match inst with
    | Do -> { state with on = On }
    | Don't -> { state with on = Off }
    | Mul2 (a, b) when state.on = On -> { state with acc = state.acc + a * b }
    | _ -> state

$"{__SOURCE_DIRECTORY__}\\input.txt"
    |> System.IO.File.ReadAllText
    |> instParser.Matches
    |> Seq.cast<Match>
    |> Seq.fold execInstruction { acc = 0; on = On }
//83158140
//Another gold star!