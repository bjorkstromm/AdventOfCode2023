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
    let isPartNumber (y, s, e)  =
        let xlen = Array2D.length2 schematic
        let ylen = Array2D.length1 schematic
        let over = if y > 0 then schematic.[y-1,s..e] else [||]
        let under = if y < ylen - 1 then schematic.[y+1,s..e] else [||]
        let ys = if y > 0 then y - 1 else y
        let ye = if y < xlen - 1 then y + 1 else y
        let left = if s > 0 then schematic.[ys..ye,s-1] else [||]
        let right = if e < xlen - 1 then schematic.[ys..ye,e+1] else [||]

        [|over;under;left;right|]
        |> Array.concat
        |> Array.exists _.IsSymbolic

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
                { 
                    Number = number
                    Start = (y, startIndex)
                    Stop = (y, stopIndex)
                    IsPartNumber = isPartNumber (y, startIndex, stopIndex)
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