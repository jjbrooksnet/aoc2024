let sample = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20""" |> _.Split('\n')
type RawEquation = uint64 * uint64 list
let stringToRawEquation (line: string):RawEquation =
    let parts = line.Split ": "
    (parts.[0] |> uint64, parts.[1].Split ' ' |> Array.map uint64 |> Array.toList)

let calc nums =
    let rec doSums (from: uint64 list) =
        match from with
        | [] -> failwith "Empty set of values"
        | [x] -> [x]
        | x :: xs -> ((doSums xs) |> List.map ((+) x)) @ ((doSums xs) |> List.map (( * ) x))
    nums
    |> List.rev
    |> doSums

calc (snd <| stringToRawEquation sample.[0]) //val it: int list = [29; 190] OK!
calc (snd <| stringToRawEquation sample.[1]) //val it: int list = [148; 3267; 3267; 87480]



System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input.txt"
|> Array.map stringToRawEquation
|> Array.map
    (fun (sum, nums) ->
        let doneSums = calc nums
        if (doneSums |> List.contains sum) then Some sum else None
    )
|> Array.choose id
|> Array.sum
//val it: uint64 = 12553187650171UL


let numConcat (a: uint64) (b: uint64) =
    a * (pown 10UL (b.ToString().Length)) + b

numConcat 123UL 78UL
numConcat 1245UL 45678UL

let threeSums a b = [a + b ; a * b ; numConcat a b]

let rec doSums nums =
    match nums with
    | [] -> failwith "Empty list of values"
    | [x] -> [[x]]
    | [x ; y] -> [threeSums x y]
    | x :: y :: zs -> threeSums x y |> List.collect (fun a -> doSums (a :: zs))

sample.[4]
|> stringToRawEquation
|> snd
|> doSums


System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input.txt"
|> Array.map stringToRawEquation
|> Array.map
    (fun (sum, nums) ->
        let doneSums = (doSums nums |> List.collect id)
        if (doneSums |> List.contains sum) then Some sum else None
    )
|> Array.choose id
|> Array.sum
//val it: uint64 = 96779702119491UL