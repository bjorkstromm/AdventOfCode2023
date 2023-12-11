type Tile =
    | NS
    | EW
    | NE
    | NW
    | SE
    | SW
    | Ground
    | Start

type Direction =
    | N
    | E
    | S
    | W

let parse filename =
    let rec find (x,y) map =
        if y >= Array2D.length2 map then failwith "No start found"
        elif x >= Array2D.length1 map then find (0, y + 1) map
        elif map.[x, y] = Start then (x, y)
        else find (x + 1, y) map

    let map =
        filename
        |> System.IO.File.ReadAllLines
        |> Array.map (fun line ->
            line.ToCharArray()
            |> Array.map (function
                | '|' -> NS
                | '-' -> EW
                | 'L' -> NE
                | 'J' -> NW
                | '7' -> SW
                | 'F' -> SE
                | '.' -> Ground
                | 'S' -> Start
                | c -> failwithf "Unknown tile: %c" c))
        |> array2D
    let start = find (0, 0) map
    (start, map)

let loop (start, map : Tile [,]) =
    let rec loop (y, x) dir d =
        let d = d + 1
        match (map.[y,x], dir) with
        | (NS, N) -> loop (y + 1, x) N d
        | (NS, S) -> loop (y - 1, x) S d

        | (EW, W) -> loop (y, x - 1) W d
        | (EW, E) -> loop (y, x + 1) E d

        | (NE, N) -> loop (y, x + 1) E d
        | (NE, W) -> loop (y - 1, x) S d

        | (NW, N) -> loop (y, x - 1) W d
        | (NW, E) -> loop (y - 1, x) S d

        | (SE, S) -> loop (y, x + 1) E d
        | (SE, W) -> loop (y + 1, x) N d

        | (SW, S) -> loop (y, x - 1) W d
        | (SW, E) -> loop (y + 1, x) N d

        | (Ground, _) -> failwithf "Fell off the map at (%d, %d) coming from (%A) " y x dir
        | (Start, _) -> d
        | _ -> failwithf "Unknown tile at (%d, %d): %A coming from %A" y x map.[y,x] dir

    let (y, x) = start
    let (start, dir) =
        match map.[y-1,x] with
        | NS | SE | SW -> ((y-1,x), S)
        | _ ->
            match map.[y+1,x] with
            | NS | NE | NW -> ((y+1,x), N)
            | _ ->
                match map.[y,x-1] with
                | EW | NE | SE -> ((y,x+1), E)
                | _ ->
                    match map.[y,x+1] with
                    | EW | NW | SW -> ((y,x-1), W)
                    | _ -> failwith "Start is not a start tile"

    loop start dir 0


let part1 filename =
    filename
    |> parse
    |> loop
    |> (fun d -> d / 2)