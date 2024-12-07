type Position = {Row: int; Col: int}
type Direction = |N|E|S|W
type Guard = {Position: Position; Direction: Direction}
type MapTile = |Obstacle|VisitedFloor|UnvisitedFloor
type State = {RoomMap: MapTile[][]; MinRow: int; MaxRow: int; MinCol: int; MaxCol: int; Guard: Guard; ReachedExit: bool}

let charToMapTileAndGuardDirection (input: char) =
    match input with
    | '#' -> Some (Obstacle, None)
    | 'X' -> Some (VisitedFloor, None)
    | '.' -> Some (UnvisitedFloor, None)
    | '^' -> Some (VisitedFloor, Some N)
    | '>' -> Some (VisitedFloor, Some E)
    | 'V' -> Some (VisitedFloor, Some S)
    | '<' -> Some (VisitedFloor, Some W)
    | _ -> None
let mapTileToChar (input: MapTile) =
    match input with
    | Obstacle -> '#'
    | VisitedFloor -> 'X'
    | UnvisitedFloor -> '.'
let stringToMapRowGuardColAndDirection (input: string) =
    let (tiles, guard) = input.ToCharArray() |> Array.map charToMapTileAndGuardDirection |> Array.choose id |> Array.unzip
    let guardCol = guard |> Array.tryFindIndex (fun x -> x.IsSome)
    match guardCol with
    | Some col -> (tiles, guardCol, guard.[col])
    | None -> (tiles, None, None)
let initialiseGameState (input: string array ) =
    let (rows, guardCols, directions) = input |> Array.map stringToMapRowGuardColAndDirection |> Array.unzip3
    let guardRow = guardCols |> Array.tryFindIndex (fun x -> x.IsSome)
    let rowCount = rows.Length
    let colCount = rows.[0].Length
    match guardRow with
    | Some row -> {
        RoomMap = rows
        MinRow = 0
        MaxRow = rowCount - 1
        MinCol = 0
        MaxCol = colCount - 1
        Guard = {
            Position = { Row = row; Col = guardCols.[row].Value }
            Direction = directions.[row].Value
            }
        ReachedExit = false
        }
    | None -> failwith "No Guard Found"
let tileRowToString (tileRow: MapTile[]) =
    tileRow |> Array.map mapTileToChar |> System.String
let roomMapToString (roomMap: MapTile[][]) =
    roomMap |> Array.map tileRowToString |> String.concat "\n"
let visitTileOnRoomMap (oldMap: MapTile[][]) pos =
    [|
        for r in 0 .. oldMap.Length - 1 ->
            [|
                for c in 0 .. oldMap[r].Length - 1 ->
                    if r = pos.Row && c = pos.Col then
                        VisitedFloor
                    else
                        oldMap[r][c]
            |]
    |]
let rotateGuard (guard: Guard) =
    let newDirection =
        match guard.Direction with
        | N -> E
        | E -> S
        | S -> W
        | W -> N
    { guard with Direction = newDirection }

let moveGuard (state: State) =
    let nextPosition =
        match state.Guard.Direction with
        | N -> { state.Guard.Position with Row = state.Guard.Position.Row - 1 }
        | E -> { state.Guard.Position with Col = state.Guard.Position.Col + 1 }
        | S -> { state.Guard.Position with Row = state.Guard.Position.Row + 1 }
        | W -> { state.Guard.Position with Col = state.Guard.Position.Col - 1 }
    if nextPosition.Row < state.MinRow || nextPosition.Row > state.MaxRow || nextPosition.Col < state.MinCol || nextPosition.Col > state.MaxCol then
        { state with ReachedExit = true}
    else
        match state.RoomMap[nextPosition.Row][nextPosition.Col] with
        | VisitedFloor -> { state with Guard = { Position = nextPosition; Direction = state.Guard.Direction } }
        | UnvisitedFloor -> { state with RoomMap = visitTileOnRoomMap state.RoomMap nextPosition; Guard = { Position = nextPosition; Direction = state.Guard.Direction } }
        | Obstacle -> { state with Guard = rotateGuard state.Guard }
let playGame input =
    let rec checkStateAndMove state =
        if state.ReachedExit then
            state
        else
            state |> moveGuard |> checkStateAndMove
    input
    |> initialiseGameState
    |> checkStateAndMove
let sampleMap = """....#.....
    .........#
    ..........
    ..#.......
    .......#..
    ..........
    .#..^.....
    ........#.
    #.........
    ......#...""" |> _.Split('\n')

let print str = printfn "%s" str

let endGame =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input.txt"
    |> playGame
    |> _.RoomMap

endGame 
|> roomMapToString
|> print

endGame |> Array.sumBy (fun row -> row |> Array.sumBy (fun tile -> if tile.IsVisitedFloor then 1 else 0))
//5331
//1 Star :>