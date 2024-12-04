let sample = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"
let sampleLines = sample.Split ("\n") |> Array.toList
(*
We can simplify the problem of finding all instances of XMAS in any direction by:
1. searching for XMAS and SAMX
2. rotating the grid by 45, 90 and -45 degress, and searching each line in the rotated grid

N.B. -45 is flipping the grid and then + 45, so we only need 90 and 45 degree rotations
*)

(*
Rotate90 is simple; build strings from every 0th, 1st, 2nd, 3rd... string in the list
*)
let everyNthChar (source: string list) (n: int) =
    source |> List.map (fun s -> s.[n]) |> List.toArray |> System.String |> string

let rotate90 (source: string list) =
    [ for i in 0..(source.[0].Length - 1) do everyNthChar source i ]

rotate90 sampleLines
(*Looks OK
val it: System.String list =
  ["MMAMXXSSMM"; "MSMSMXMAAX"; "MAXAAASXMM"; "SMSMSMMAMX"; "XXXAAMSMMA";
   "XMMSMXAAXX"; "MSAMXXSSMM"; "AMASAAXAMA"; "SSMMMMSAMS"; "MAMXMASAMX"]


Rotate45 is a little more complicated
The starting point is every character in the top row, and the first character of every row
(except the top row, so we don't do the top left twice)

A row-col tuple probably makes sense for the starting point, and to help navigate the diagonal movement up to the edge of the grid 
*)
let getStarts (source: string list) =
    let minCol = 0
    let maxCol = source.[0].Length - 1
    let minRow = 1
    let maxRow = source.Length - 1
    [ for col in minCol..maxCol do (0, col)] @ [ for row in minRow..maxRow do (row, 0)]

getStarts sampleLines
(*
val it: (int * int) list =
  [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7); (0, 8);
   (0, 9); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0); (8, 0);
   (9, 0)]
Seems fine, and (0, 0) isn't repeated

We can have a function to advance the location, keeping within in bounds of the grid
This can be an option type, so it returns None if we would go over the edge
*)
let advance (source: string list) ((row: int), (col: int)) =
    let maxRow = source.Length - 1
    let maxCol = source.[0].Length - 1
    let newRow = row + 1
    let newCol = col + 1
    if newRow > maxRow || newCol > maxCol then
        None
    else
        Some (newRow, newCol)

//Let's test it out
advance sampleLines (0, 0) //val it: (int * int) option = Some (1, 1)
advance sampleLines (1, 9) //val it: (int * int) option = None
advance sampleLines (8, 7) //val it: (int * int) option = Some (9, 8)
advance sampleLines (9, 8) //val it: (int * int) option = None
//Great, we can move diagonally from our starting point and never leave the grid

let diagonalMoves (source: string list) (start: int * int) =
    let rec keepMoving across path current =
        let nextMove = advance across current
        match nextMove with
        | Some move -> keepMoving across (path @ [move]) move
        | None -> path
    start |> keepMoving source [start]

diagonalMoves sampleLines (0, 0) //[(0, 0); (1, 1); (2, 2); (3, 3); (4, 4); (5, 5); (6, 6); (7, 7); (8, 8); (9, 9)]
diagonalMoves sampleLines (8, 7) //[(8, 7); (9, 8)]

let moveOverChars (source: string list) (moves: (int * int) list) =
    moves |> List.map (fun (row, col) -> source[row].[col]) |> List.toArray |> System.String |> string

//Now we can build rotate45
let rotate45 source =
    source
    |> getStarts
    |> List.map (diagonalMoves source)
    |> List.map (moveOverChars source)

rotate45 sampleLines
(*
Awesome!

  ["MSXMAXSAMX"; "MASAMXXAM"; "MMXSXASA"; "SXMMAMS"; "XMASMA"; "XSAMM"; "MMMX";
   "ASM"; "SA"; "M"; "MMASMASMS"; "ASAMSAMA"; "MMAMMXM"; "XXSAMX"; "XMXMA";
   "SAMX"; "SAM"; "MX"; "M"]

*)

let generateAllRotations source =
    source @ rotate45 source @ (rotate90 source) @ (rotate45 <| List.rev source)

generateAllRotations sampleLines

let wordsearchInRow (source: string) (word: string) =
    let reversedWord = word.ToCharArray () |> Array.rev |> System.String |> string
    let rec countOccurencesFrom (inString: string) (fromChar: int) (searchWord: string) (currentCount: int) =
        let foundAt = inString.IndexOf (searchWord, fromChar)
        if fromChar >= inString.Length then
            currentCount
        else
            if foundAt = -1 then
                currentCount
            else
                countOccurencesFrom inString (foundAt + 1) searchWord (currentCount + 1)
    let count = countOccurencesFrom source 0 word 0
    if reversedWord = word then
        count
    else
        count + countOccurencesFrom source 0 reversedWord 0
wordsearchInRow "XMASXMASSAMX" "XMAS"
//val it: int = 3

let wordSearch source word =
    source |> List.map (fun row -> wordsearchInRow row word) |> List.sum

wordSearch (generateAllRotations sampleLines) "XMAS"
//val it: int = 18

wordSearch (System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input.txt" |> Array.toList |> generateAllRotations) "XMAS"
//val it: int = 2642
//Gold star!

let isXmas (source: string array) (row: int) (col: int) =
    match (row, col) with
    | (0, _) -> false
    | (_, 0) -> false
    | (r, _) when r >= source.Length - 1 -> false
    | (_, c) when c >= source.[0].Length - 1 -> false
    | (r, c) when source.[r].[c] = 'A' ->
        let topleft = source.[r - 1].[c - 1]
        let bottomright = source.[r + 1].[c + 1]
        let bottomleft = source.[r + 1].[c - 1]
        let topright = source.[r - 1].[c + 1]
        let matchesNegativeDiagonal = (topleft = 'M' && bottomright = 'S') || (topleft = 'S' && bottomright = 'M')
        let matchesPositiveDiagonal = (bottomleft = 'M' && topright = 'S') || (bottomleft = 'S' && topright = 'M')
        matchesNegativeDiagonal && matchesPositiveDiagonal
    | _ -> false

let input: string array = System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input.txt"
let xmases = [ 
    for i in 0 .. input.Length - 1 do
        for j in 0 .. input.[0].Length - 1 do
            isXmas input i j
    ]
xmases |> List.filter id |> List.sumBy (fun (_) -> 1)
//val it: int = 1974
//Hooray!