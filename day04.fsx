open System
open System.Collections.Generic

type Card = {
    Id : int
    WinningNumbers : int[]
    NumbersOnHand : int[]
}

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = System.Text.RegularExpressions.Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let parseCard (input : string) =
    let tokens = input.Split (':', 2, StringSplitOptions.RemoveEmptyEntries)
    match tokens.[0] with
    | ParseRegex @"Card\s+([0-9]+)" [Integer id] ->
        let tokens = tokens.[1].Split ('|', 2, StringSplitOptions.RemoveEmptyEntries)
        let winningNumbers =
            tokens.[0].Split (' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
        let numbersOnHand =
            tokens.[1].Split (' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
        { 
            Id = id
            WinningNumbers = winningNumbers
            NumbersOnHand = numbersOnHand
        }
    | _ -> failwith "Invalid input"

let parseCards (filename : string) =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.map parseCard

let matches card =
    card.WinningNumbers
        |> Set.ofArray
        |> Set.intersect (Set.ofArray card.NumbersOnHand)
        |> Set.count

let points card =
    let matches = card |> matches
    pown 2 (matches - 1)

let part1 (filename : string) =
    filename
    |> parseCards
    |> Array.map points
    |> Array.sum

// Part 2

let part2 (filename : string) =
    let cards = parseCards filename
    let max = cards |> Array.maxBy (fun c -> c.Id)

    let increment (played : Dictionary<int,uint64>) (id : int) =
        if id > max.Id then played
        else
            played.[id] <- match played.TryGetValue id with
                                | (true, n) -> n + 1UL
                                | _ -> 1UL
            played

    let play (played : Dictionary<int,uint64>) (card : Card) =
        let played = increment played card.Id
        let matches = card |> matches
        if matches = 0 then played
        else
            [|1..matches|]
            |> Array.fold (fun played i ->
                [|1UL..played.[card.Id]|]
                |> Array.fold (fun played _ -> increment played (card.Id + i)) played) played

    let played = Dictionary<int,uint64>()

    cards
    |> Array.fold play played
    |> Seq.map (fun kv -> (kv.Key, kv.Value))
    |> Seq.sumBy snd
