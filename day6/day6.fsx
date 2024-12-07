type Position = {Row: int; Col: int}
type Direction = |N|E|S|W
type Guard = {Position: Position; Direction: Direction}
//Part 2: Add a list of the directions through which we visited a tile, in the order in which we visited. We'll use this to see where we can place obstacles to cause loops
type MapTile = |Obstacle|VisitedFloor of Direction list|UnvisitedFloor
type State = {RoomMap: MapTile[][]; MinRow: int; MaxRow: int; MinCol: int; MaxCol: int; Guard: Guard; ReachedExit: bool}

let charToMapTileAndGuardDirection (input: char) =
    match input with
    | '#' -> Some (Obstacle, None)
    | '.' -> Some (UnvisitedFloor, None)
    | '^' -> Some (VisitedFloor [N], Some N)
    | '>' -> Some (VisitedFloor [E], Some E)
    | 'V' -> Some (VisitedFloor [S], Some S)
    | '<' -> Some (VisitedFloor [W], Some W)
    | _ -> None
let mapTileToChar (input: MapTile) =
    match input with
    | Obstacle -> '#'
    | VisitedFloor directions ->
        let vertical = (directions |> List.contains N) || (directions |> List.contains S)
        let horizontal = (directions |> List.contains E) || (directions |> List.contains W)
        if (vertical && horizontal) then '+' elif vertical then '|' else '-'
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
let visitTileOnRoomMap (oldMap: MapTile[][]) (position: Position) (direction: Direction) =
    [|
        for r in 0 .. oldMap.Length - 1 ->
            [|
                for c in 0 .. oldMap[r].Length - 1 ->
                    if r = position.Row && c = position.Col then
                        match oldMap[r][c] with
                        | VisitedFloor directions ->
                            if directions |> List.contains direction then
                                failwith "Looping"
                            else
                                VisitedFloor <| directions @ [direction]
                        | UnvisitedFloor -> VisitedFloor [direction]
                        | Obstacle -> failwith "Crashed into obstacle"
                    else
                        oldMap[r][c]
            |]
    |]
let setObstacleAt (oldMap: MapTile[][]) (position: Position) =
    [|
        for r in 0 .. oldMap.Length - 1 ->
            [|
                for c in 0 .. oldMap[r].Length - 1 ->
                    if r = position.Row && c = position.Col then
                        Obstacle
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
let calculateNextPosition state =
    let nextPosition =
        match state.Guard.Direction with
        | N -> { state.Guard.Position with Row = state.Guard.Position.Row - 1 }
        | E -> { state.Guard.Position with Col = state.Guard.Position.Col + 1 }
        | S -> { state.Guard.Position with Row = state.Guard.Position.Row + 1 }
        | W -> { state.Guard.Position with Col = state.Guard.Position.Col - 1 }
    if nextPosition.Row < state.MinRow || nextPosition.Row > state.MaxRow || nextPosition.Col < state.MinCol || nextPosition.Col > state.MaxCol then
        None
    else
        Some nextPosition
let getTileAtPosition state position = state.RoomMap[position.Row][position.Col]
let moveGuard (state: State) =
    let nextPosition = calculateNextPosition state
    match nextPosition with
    | None -> { state with ReachedExit = true}
    | Some position ->        
        match getTileAtPosition state position with
        (*
            We'll revisit a VisitedFloor tile so we can update the map to record the direction we're travelling while visiting
            We'll count rotating at an Obstacle as an extra visit, so again we record the direction we'll be leaving in, as well as the direction we arrived from
        *)
        | VisitedFloor directions -> { state with RoomMap = visitTileOnRoomMap state.RoomMap position state.Guard.Direction ; Guard = { Position = position; Direction = state.Guard.Direction } }
        | UnvisitedFloor -> { state with RoomMap = visitTileOnRoomMap state.RoomMap position state.Guard.Direction ; Guard = { Position = position; Direction = state.Guard.Direction } }
        | Obstacle ->
            let newGuardState = rotateGuard state.Guard
            { state with RoomMap = visitTileOnRoomMap state.RoomMap newGuardState.Position newGuardState.Direction ; Guard = newGuardState }

let playFromState state =
    let rec checkStateAndMove state =
        if state.ReachedExit then
            Some state
        else
            try
                state |> moveGuard |> checkStateAndMove
            with
                | _ -> None
    checkStateAndMove state
let playGame input =
    input
    |> initialiseGameState
    |> playFromState
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
    |> _.Value

endGame.RoomMap 
|> roomMapToString
|> print

endGame.RoomMap |> Array.sumBy (fun row -> row |> Array.sumBy (fun tile -> if tile.IsVisitedFloor then 1 else 0))
//5331
//1 Star :>

(*
With the revisions to record the direction of travel (in order) on each visit to a tile we can check the tiles to see where we can add obstacles to create loops.

Example. Let's say the direction is N and the guard encounters a tile they visited while travelling E, adding an obstacle N of us would force them down the path they've already taken, as the rotation from N is to E

So if there is a visited tile in (r, c) with [E; N], adding an obstacle at (r - 1, c) would create a loop, unless r - 1 is off the board
*)

let newObstacles (state: State) =
    [|
        let onMap = state.RoomMap
        for r in 0 .. onMap.Length - 1 do
            for c in 0 .. onMap[r].Length - 1 do
                match onMap[r][c] with
                | VisitedFloor directions ->
                    match directions with
                    | [E; N] when r - 1 >= state.MinRow -> Some { Row = r - 1 ; Col = c } //Add an obstacle N to force N to rotate back to E
                    | [S; E] when c + 1 <= state.MaxCol -> Some { Row = r ; Col = c + 1} //Add an obstacle E to force E to rotate back to S
                    | [W; S] when r + 1 <= state.MaxRow -> Some { Row = r + 1 ; Col = c } //Add an obstacle S to force S to rotate back to W
                    | [N; W] when c - 1 >= state.MinCol -> Some { Row = r; Col = c - 1} //Add an obstacle W to force W to rotate back to N
                    | _ -> None
                | _ -> None
    |] |> Array.toList |> List.choose id |> List.distinct

newObstacles endGame |> List.length
//319
//That's not the right answer; your answer is too low.
// :(
sampleMap |> playGame |>  _.Value.RoomMap |> roomMapToString |> print
sampleMap |> playGame |> _.Value |> newObstacles
(*
We have got 3 of the 6 locations we could place obstacles in the sample:
[{ Row = 6; Col = 3 }
;{ Row = 7; Col = 6 }
;{ Row = 7; Col = 7 }]  

The ones we do not have are where an early turn would put us into the path of an obstacle.

How will we test for those?
One way would be to simulate the placement of an obstacle and see if it causes a loop. 
E.g. if the guard is travelling west, if there is an obstacle north of the guard's position, then an obstacle directly in front of the guard
would cause them to turn north, even eventually east... and potentially loop around
*)

let checkWhetherItIsWorthTurning (state: State) =
    let nextPosition = calculateNextPosition state
    match nextPosition with
    | None -> None
    | Some position ->
        match getTileAtPosition state position with
        | Obstacle -> None
        | _ ->
            match playFromState { state with RoomMap = (setObstacleAt state.RoomMap position) } with
            | Some _ -> None
            | _ -> nextPosition

let playFromStateWithObstacleGeneraction state =
    let rec checkStateAndMove state candidateObstacles =
        if state.ReachedExit then
            candidateObstacles
        else
            let candidateObstacle = state |> checkWhetherItIsWorthTurning
            let newCandidates =
                match candidateObstacle with
                | Some obstacle -> candidateObstacles @ [obstacle]
                | _ -> candidateObstacles
            checkStateAndMove (state |> moveGuard) newCandidates
    checkStateAndMove state []
    |> List.distinct

sampleMap
|> initialiseGameState
|> playFromStateWithObstacleGeneraction

System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\\input.txt"
|> initialiseGameState
|> playFromStateWithObstacleGeneraction
|> List.length
//1940
//That's not the right answer; your answer is too high. :(:(:(:(