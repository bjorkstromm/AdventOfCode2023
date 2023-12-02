type Round = {
    Red : int
    Green : int
    Blue : int
}

type Game = {
    Id : int
    Rounds: Round[]
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

// 3 blue, 4 red
let parseRound (input : string) =
    let folder round (cube : string) =
        match cube.Trim() with
        | ParseRegex @"([0-9]+) (red|blue|green)" [Integer count; color] ->
            match color with
            | "blue" -> { round with Blue = count }
            | "red" -> { round with Red = count }
            | "green" -> { round with Green = count }
            | _ -> failwith "Invalid color"
        | _ -> failwith "Invalid input"

    input.Split (',')
    |> Array.fold folder { Blue = 0; Red = 0; Green = 0 }

// Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
let parseGame (input : string) =
    let tokens = input.Split (':', 2)
    match tokens[0] with
    | ParseRegex @"Game ([0-9]+)" [Integer id] ->
        let rounds =
            tokens.[1].Split (';')
            |> Array.map parseRound
        { Id = id; Rounds = rounds }
    | _ -> failwith "Invalid input"

let valid config game =
    let validRound round =
        round.Red <= config.Red
        && round.Green <= config.Green
        && round.Blue <= config.Blue

    game.Rounds |> Array.forall validRound


// only 12 red cubes, 13 green cubes, and 14 blue cubes
let config = {
    Red = 12
    Green = 13
    Blue = 14
}

let part1 fileName =
    fileName
    |> System.IO.File.ReadAllLines
    |> Array.map parseGame
    |> Array.filter (valid config)
    |> Array.sumBy _.Id

// Part 2

let minimum game =
    let folder acc round =
        {
            Red = max acc.Red round.Red
            Green = max acc.Green round.Green
            Blue = max acc.Blue round.Blue
        }
    game.Rounds
    |> Array.fold folder { Red = 0; Green = 0; Blue = 0 }

let power round = round.Red * round.Green * round.Blue

let part2 fileName =
    fileName
    |> System.IO.File.ReadAllLines
    |> Array.map parseGame
    |> Array.map (minimum >> power)
    |> Array.sum