open System

type Symbol =
    | Number of int
    | Symbol of char
    | Empty

    member this.IsNumeric =
        match this with
        | Number _ -> true
        | _ -> false

    member this.IsSymbolic =
        match this with
        | Symbol _ -> true
        | _ -> false

type SchematicNumber = {
    Number: int
    Start: (int * int)
    Stop: (int * int)
    IsPartNumber: bool
    Gears: (int * int) []
}

let parse file =
    file
    |> System.IO.File.ReadAllLines
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (fun c ->
            match c with
            | '.' -> Empty
            | c when Char.IsDigit c -> Number (Char.GetNumericValue c |> int)
            | _ -> Symbol c
        )
    )
    |> array2D

let numbers (schematic : Symbol[,]) =
    let adjacent (y, s, e) =
        let xlen = Array2D.length2 schematic
        let ylen = Array2D.length1 schematic
        let over = if y > 0 then schematic.[y-1,s..e] |> Array.mapi (fun i c -> (y-1, s+i), c) else [||]
        let under = if y < ylen - 1 then schematic.[y+1,s..e] |> Array.mapi (fun i c -> (y+1, s+i), c) else [||]
        let ys = if y > 0 then y - 1 else y
        let ye = if y < xlen - 1 then y + 1 else y
        let left = if s > 0 then schematic.[ys..ye,s-1] |> Array.mapi (fun i c -> (ys+i, s-1), c) else [||]
        let right = if e < xlen - 1 then schematic.[ys..ye,e+1] |> Array.mapi (fun i c -> (ys+i, e+1), c) else [||]

        [|over;under;left;right|]
        |> Array.concat

    let isPartNumber (adjacent : ((int * int) * Symbol) []) =
        adjacent
        |> Array.exists (fun (_, s) -> s.IsSymbolic)

    let gears (adjacent : ((int * int) * Symbol) []) =
        adjacent
        |> Array.choose (fun (p, s) ->
            match s with
            | Symbol c when c = '*' -> Some p
            | _ -> None
        )

    let processLine y =
        let rec loop (symbols : (int * Symbol) []) =
            match symbols |> Array.skipWhile (fun (_, s) -> not(s.IsNumeric)) with
            | [||] -> []
            | start ->
                let startIndex = start |> Array.head |> fst
                let numbers = start |> Array.takeWhile (fun (_, s) -> s.IsNumeric)
                let stopIndex = numbers.[^0] |> fst
                let skip = numbers.Length
                let number =
                    numbers
                    |> Array.rev
                    |> Array.mapi (fun i (_, Number n) -> (pown 10 i) * n)
                    |> Array.reduce (+)
                let adjacent = adjacent (y, startIndex, stopIndex)
                { 
                    Number = number
                    Start = (y, startIndex)
                    Stop = (y, stopIndex)
                    IsPartNumber = isPartNumber adjacent
                    Gears = gears adjacent
                } :: if skip >= start.Length then []
                    else (loop (start |> Array.skip skip))

        schematic.[y,*]
        |> Array.indexed
        |> loop

    let upper = (schematic |> Array2D.length1) - 1
    [|0..upper|]
    |> Array.map processLine
    |> List.concat

let part1 filename =
    filename
    |> parse
    |> numbers
    |> Seq.filter _.IsPartNumber
    |> Seq.sumBy _.Number

let part2 filename =
    filename
    |> parse
    |> numbers
    |> List.toArray
    |> Array.collect (fun n -> n.Gears |> Array.map (fun g -> (g, n.Number)))
    |> Array.groupBy fst
    |> Array.choose (fun (_, (g)) ->
        match g with
        | [|(_,x);(_,y)|] -> Some ((bigint(x) * bigint(y)))
        | _ -> None)
    |> Array.sum